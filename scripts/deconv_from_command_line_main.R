# deconve_from_command_line_main.R
# Driver script for deconvolution, with input & parameters input as command line options.
# Copyright Â Bren Osberg, MDC Berlin, started Jan. 2019
#==================================================================================================
# source("https://bioconductor.org/biocLite.R")
# biocLite("methylKit")
suppressMessages(library(methylKit))
suppressMessages(library(GenomicRanges))
suppressMessages(library(stringr))
suppressMessages(library(rtracklayer))
suppressMessages(library(matrixStats))
suppressMessages(library(data.table))

# ----- parse command-line args ----------
args<-commandArgs(trailingOnly = TRUE)

## Help section
if("--help" %in% args) {
  cat("
      Expected input arguments are: 
      --sampleID: name of the sample 
      --methylcall_fin:  full path of the methylRaw input file.
      --db_save_dir:  not sure if I need this...
      --assembly:  the name of the reference genome.
      --Sigmat_biomarkers_in:  (fullpath) csv file listing biomarkers and Sigmat profiles for reference CTs 
      --mincov:  minimum coverage for a biomarker for inclusion.
      --upper_constraint: (bool) is total cell fractions required to be <1.
      --lower_constraint: ^^(ditto)^^                                   >0.
      --path_OUT:  the location of the output file 
      --logFile file to print the logs to
      --help              - print this text
      
      Example:
      ./test.R --arg1=1 --arg2='output.txt' --arg3=TRUE \n\n")
  
  q(save="no")
}


## Default setting when no arguments passed
if(length(args) < 1) {
  args <- c("--help")
}



## Parse arguments (we expect the form --arg=value)
parseArgs <- function(x) strsplit(sub("^--", "", x), "=")

argsDF <- as.data.frame(do.call("rbind", parseArgs(args)))
argsL  <- as.list(as.character(argsDF$V2))

names(argsL) <- argsDF$V1


# if plotting/analysis is necessary:
# source( paste0(R_funcdef_PATH,'analysis_funcs.R') ) ;
#  "N6_RDML00568_HT3FHCCXY_L8"
#  "/scratch/AG_Akalin/bosberg/cfDNA_cardiac_pigx_output/06_methyl_calls/N6_RDML00568_HT3FHCCXY_L7_1_val_1_bt2.sorted.deduped_CpG.txt"
#  "/scratch/AG_Akalin/bosberg/cfDNA_cardiac_pigx_output/06a_temp/"
#  "hg19"
#  paste0(R_SIGMAT_PATH,"Sun_PNAS/Sun_sigmat_allbioms.csv");
#  10;
#  1
#  1
# ("...07_deconved/")


sampleID               <- argsL$sampleID
methylcall_fin         <- argsL$methylcall_fin
# db_save_dir            <- argsL$db_save_dir
assembly               <- argsL$assembly
Sigmat_biomarkers      <- argsL$Sigmat_biomarkers
mincov                 <- as.numeric( argsL$mincov )
upper_constraint       <- as.numeric( argsL$upper_constraint )
lower_constraint       <- as.numeric( argsL$lower_constraint )
path_OUT               <- argsL$path_OUT
Rdeconv_funcdef_PATH   <- argsL$Rdeconv_funcdef_PATH 
strandedness           <- argsL$strandedness 
# ^^ not just "scripts/" 
# logFile            <- argsL$logFile

source( paste0(Rdeconv_funcdef_PATH,'load-convert_datatypes_funcs.R') ) ;
source( paste0(Rdeconv_funcdef_PATH,'deconv_funcs.R') ) ;
 
 Raw_refdat = read.csv( Sigmat_biomarkers, sep="\t", header = TRUE )
 # Refdat files should be in the format
 # Genomic location  CT1 CT2 ... NCT   
 # chr1:45-67         45 56  ...
 # chr5:78-89         50 98  ...
 # ...
 
 #NCT = "Number of Cell types" included in the signature matrix
 NCT  =  dim(Raw_refdat)[2] - 1;
 
 refdat = list();
 #Biomarkers, in GRanges format:
 refdat$biom_GR = as( as.character(Raw_refdat[,1]), "GRanges");
 
 # Just the signature matrix:
 refdat$Sigmat_whole  = 0.01*Raw_refdat[,2:(NCT+1)]
 
 refdat$CT_list       = colnames( refdat$Sigmat )
 #================================================================
 #------------     IMPORT  YOUR SAMPLE DATA:      ----------------
 
 print(paste("Importing experimental data"))
 
 # Import your raw sample data:
 methraw_ob <- get_single_methraw_ob ( fin             = methylcall_fin,
                                       dbdir           = db_save_dir, 
                                       ID              = sampleID, 
                                       assembly_in     = assembly,
                                       mincov_in       = mincov, 
                                       read.context_in = "CpG",
                                       skip            = 0,
                                       pipeline_in     = "amp",
                                       header_in       = TRUE)
 
 
 # project the experimental sample data onto the regions of interest.
 sample_dat    = convert_methob_to_Expdat( methraw_in = methraw_ob, 
                                           refdat     = refdat, 
                                           mincounts  = 1, 
                                           ID         = sampleID ) 
 
 # now update the refdat to only include ROIs with hits in the sample data.   
 refdat$Sigmat  = as( refdat$Sigmat_whole[sample_dat$biomark_hits ,] , "matrix")
 #================================================================
 #------------    define DECONVOluion parameters   ---------------
 
 eps = 0.01;
 deconv_params   =  list( "mu"          = 1e-5,
                          "epsilon"     = eps,
                          "method"      = "NM",
                          "costfunc"    = "LSQ",
                          "minbound"    = lower_constraint,
                          "maxbound"    = upper_constraint,
                          "conditions"  = get_conditions( NCT          = NCT,
                                                          impose_le_1  = upper_constraint,
                                                          impose_ge_1  = lower_constraint,
                                                          epsilon      = eps),
                          "init_guess"  =  matrix(1/NCT-(0.01*eps/NCT),NCT,1)
                          )
 
 #============     Start deconvolving:  ==================
 
 deconv_out = get_cell_fracs_NMconstr ( target_sample     = sample_dat$ROI_meth_profile,  
                                        refdat_in         = refdat,
                                        deconv_params     = deconv_params
                                       )
   
 # ===== write a function that takes arbitrary "method" argument inputs: (not done yet)    =====
 # get_cell_fracs( initial_profile  = p_init, 
 #                              target_profile   = sample_dat$ROI_meth_profile,   
 #                              Sigmat           = sample_dat$Sigmat_whits, 
 #                              epsilon          = epsilon,
 #                              mu_in            = mu, 
 #                              minbound         = FALSE, 
 #                              maxbound         = TRUE) 
 
 
 print(paste("saving data"))
 
 #------ output the CT fractions:
 write.table( deconv_out$par,
              file = paste0( path_OUT,
                             sampleID,
                             "_",strandedness,
                             "_deconv_vals.csv"), 
              sep="\t", 
              row.names = refdat$CT_list, 
              col.names=FALSE )

 #--- output remaining deconv data

 saveRDS( deconv_out, 
          file = paste0( path_OUT,
                         sampleID,
                         "_",strandedness,
                         "_deconv_dat.rds"), 
        ) 

 saveRDS( sample_dat, 
          file = paste0( path_OUT,
                         sampleID,
                         "_",strandedness,
                         "_sample_dat.rds"), 
        ) 

 print(paste(" -------- BSseq_deconv.R program complete. ---------") )
