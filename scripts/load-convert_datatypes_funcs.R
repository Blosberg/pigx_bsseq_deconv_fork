#== BSseq_deconv_funcs.R -function definitions necessary for deconvolution script on output from the BSseq pipeline (.cov file) ======
# ---last updated Nov. 8 0:54
# Brendan Osberg, MDC, Berlin 2017
#==================================================================
#--- import multiple methylcalled datasets into list: ----
get_raw_methob_list <- function( PATHin, 
                                 IDs, 
                                 assembly_in="hg19", 
                                 mincov_in=1, 
                                 read.context_in="CpG" )
  {
  result = list()
  
  for ( i in c(1:length(IDs) ) )
    {
    CT  = IDs[i]
    fin = paste0( PATHin, CT, "_ref_CpG.txt" )
    
    print( paste( "--- reading data from source: ", fin , " --- ") )
    
    result[[i]] = get_single_methraw_ob(  fin = fin, 
                                          ID = CT, 
                                          assembly_in="hg19", 
                                          mincov_in=1, 
                                          read.context_in="CpG"  )
      
    }
  names(result) = IDs
  
  return(result)
}  
#==================================================================
#--- import a single specific methylcalled dataset: ----
get_single_methraw_ob <- function( fin             = stop("Source file must be defined"),
                                   dbdir_in        = stop("db output dir must be defined"), 
                                   ID              = "unnamed", 
                                   assembly_in     = "hg19", 
                                   mincov_in       = 10, 
                                   read.context_in = "CpG",
                                   skip            = 0,
                                   pipeline_in     = "amp",
                                   header_in       = TRUE)
{   

result = methylKit::methRead( location     = fin, 
                              sample.id    = ID, 
                              assembly     = assembly_in,
                              pipeline     = pipeline_in, 
                              header       = header_in,
                              skip         = skip,
                              context      = read.context_in,
                              mincov       = mincov_in
  )

 return(result)
}

#==================================================================
# convert methylraw object expdata object

convert_methob_to_Expdat  <- function(methraw_in  = stop("methraw_in must be defined"), 
                                      refdat      = stop("refdat must be defined"), 
                                      mincounts   = 1,  
                                      ID          = "unnamed" ) 
{ 
  #---- now collate the experimental reads onto the selected biomarker regions.
  EXP_methraw_filtered  = methylKit::regionCounts( object        = methraw_in, 
                                                   regions       = refdat$biom_GR, 
                                                   strand.aware  = FALSE, 
                                                   cov.bases     = 1 ) 
  
  #---- get a vector of values over these regions.
  EXP_methraw_filtered2 = as( EXP_methraw_filtered,"methylRaw")
  ROI_meth_profile      = EXP_methraw_filtered2$numCs/EXP_methraw_filtered2$coverage
  
  #--- GRanges object containing all of the experimental data that hits upon RsOI
  biomarkers_with_hits_GR          = as(EXP_methraw_filtered2,"GRanges")  # convert methylraw object to GRange
  biomarkers_with_hits_GR          = sortSeqlevels(biomarkers_with_hits_GR)                        # sort internal chromosome ordering
  biomarkers_with_hits_GR          = sort(biomarkers_with_hits_GR)                                 # sort by chromosome level
  biomarkers_with_hits_GR$methfrac = biomarkers_with_hits_GR$numCs/biomarkers_with_hits_GR$coverage
  
  NROI_whits = length(biomarkers_with_hits_GR);
  if ( NROI_whits == 0 )
     { stop("No biomarkers from the matrix were hit by the experimental reads. No convolution can be performed. Exiting.")}

  #--- NOW COMPARE BACK TO ORIGINAL BIOMARKERS TO CHECK WHICH ONES WERE INCLUDED.  
  overlaps_hits_w_original_biomarker <- findOverlaps( biomarkers_with_hits_GR, refdat$biom_GR)
  
  # all of biomarkers_with_hits_GR should be hit in sequential order --otherwise, flag.
  if( !identical( queryHits( overlaps_hits_w_original_biomarker ), c(1:NROI_whits) ) )
    { stop("ERROR: biomarker hits becoming disordered.") }
  
  # the refdat$biom_GR hits are now the subset ROI that will go into our deconvolution matrix.
  biomark_hits = subjectHits(overlaps_hits_w_original_biomarker)
  Sigmat_whits = refdat$Sigmat[ biomark_hits, ]

  #--- Matrix using just the biomarkers hit on in the experiment, 
  #--- target values for each tissue type are listed by col.
  #
  # My old way of getting avg_methylation --produces the same thing as ROI_meth_profile, above
  # the following is clumsy, but explicit; good to keep as a reference:
  #  for (j in c(1:L)) #---- now loop through each j biomarker being considered and grab the avg. meth.
  #  {
  #  range                 = subsetByOverlaps(EXP_GR, refdat$Biomarks_GR[j])
  #  avg_meth_range        = sum( mcols(range)$numCs) / ( sum(mcols(range)$coverage ) ) 
  #  ROI_meth_profile[j,1] = avg_meth_range
  #  }
  #----------------------------------
  result <- list( "sampleID"                = ID, 
                  "ROI_meth_profile"        = ROI_meth_profile, 
                  "biomark_hits"            = biomark_hits, 
                  "biomarkers_with_hits_GR" = biomarkers_with_hits_GR, 
                  "Sigmat_whits"            = Sigmat_whits )
  return(result)
}

#==================================================================
#--- reduce the data to GRanges with sufficient coverage in all loci for all CTs
convert_2GR <- function( Methob_in = NULL, 
                         min_refcov=1,
                         refname = "unnamed")
{
  NCT     = length(Methob_in)
  CT_list = names(Methob_in)
  
  refdat_mincov    = list()
  refdat_mincov_GR = list()
  
  #----- get a GRanges object of sufficiently covered sites for each CT.
  for ( i in c(1:NCT ) )
  {
  print( paste(" --- converting methraw_object ", CT_list[i] , " to GRanges --- ") )
    refdat_mincov [[i]]    = Methob_in[[i]][ Methob_in[[i]]$coverage >= min_refcov  ]
    refdat_mincov_GR [[i]] = sort( 
      GRanges(  
        Methob_in[[i]][ Methob_in[[i]]$coverage >= min_refcov  ]
      ) 
    )
  }

names(refdat_mincov_GR) <- CT_list
 
return(refdat_mincov_GR)
}

#==================================================================
#--- take a GRanges refdat object and filter to take only biomarkers covered on ALL of the CTs
methraw_list_biomark_projection  <- function( Methob_list_in = stop("must be defined"),
                                              biom_target    = stop("must be defined"), 
                                              mincov=1,
                                              zeropadding = TRUE)
{
  NCT     = length(Methob_list_in)
  CT_list = names(Methob_list_in)
  
  zeropadded_GR_projection = list()
  
  for ( i in c(1:NCT ) )
  {
    print( paste(" --- projecting methraw_object ", CT_list[i] , " to biomarker_set provided --- ") )
    if( zeropadding == TRUE )
      {
      zeropadded_GR_projection[[i]] = get_GRoverlap_with_zeropadding( Methob_list_in[[i]], 
                                                                      biom_target, 
                                                                      mincov=1)   
      }
    else{ 
      stop("Why have you not set zeropadding to 'TRUE' ? ")
      }
  }
  
  names(zeropadded_GR_projection) <- CT_list
  
  return(zeropadded_GR_projection)
}


#==================================================================
#--- take a GRanges refdat object and filter to take only biomarkers covered on ALL of the CTs
filter_4_universal_coverage  <- function( refdat_nonuniv_GR_in = stop("must be defined"),
                                          NROI_initial         = stop("must be defined"),
                                          min_refcov           = 1,
                                          refname              = "unnamed"
                                        )
{
  NCT          = length( refdat_nonuniv_GR_in)
  allcovered_hitlist = list() #--- a list for each CT of indices that correspond to the arrays actually put together in the Sigmat
  all_covered_GR     = list() #--- the actualy GRanges list object we intend to export.
  
  
  #------ Find the universal coverage biomarker set -----------------------------------------------------
  if( NCT < 2  )
    {  stop("NCT is less than 2. Deconvolving makes no sense.") }
  
  all_covered_biomGR =  refdat_nonuniv_GR_in [[1]][ queryHits ( findOverlaps( refdat_nonuniv_GR_in [[1]], refdat_nonuniv_GR_in [[2]] ) )   ]  
  mcols(all_covered_biomGR)=NULL
  for ( i in c(3:NCT ) )
    { 
    all_covered_biomGR = all_covered_biomGR[  queryHits ( findOverlaps( all_covered_biomGR, refdat_nonuniv_GR_in [[i]] ) ) ]
    # this is the list of sites sufficiently covered by ALL CTs
    }
  
  NROI = length(all_covered_biomGR) # FINAL!
  
  #--- apply final filter to original data sets --extract the entries that correspond to these biomarkers:
  for ( i in c(1:NCT ) )
  { 
    print( paste(" --- selecting universal coverage methylation profile for CT: ", names(refdat_nonuniv_GR_in)[i] , " --- ") )
    
    temp = findOverlaps( all_covered_biomGR, refdat_nonuniv_GR_in [[i]] )
    if( !identical( queryHits(temp), c(1:NROI) ) )
      { stop("not hitting all overlaps exactly once.") }
    
    allcovered_hitlist[[i]] = subjectHits( temp )
    #=== the subset of biomarkers for this CT that were hit sufficiently by ALL CTs
    
    if(length(allcovered_hitlist[[i]] ) != NROI)
      { stop("inconsistency between NROIS and hitlist") }
    
    all_covered_GR[[i]]       = refdat_nonuniv_GR_in [[i]][ allcovered_hitlist[[i]] ] #the actual marker locations.
  }
  
  names(all_covered_GR) <- names(  refdat_nonuniv_GR_in )
  
  return( all_covered_GR )
}


#==================================================================
#--- get a Granges object from a methylkit and target biomarker array with zeros
get_GRoverlap_with_zeropadding <- function( Methobj_in, biom_target, mincov=1) 
{   
#Methobj_in is the read data that we are then comparing to established biomarkers
#biom_target in GRanges format. mincov is an int specifying minimum coverage of these biomarkers
#this function does it and returns an array with zero padding at all points where 
#no hits were observed (so multiple such objects can then be added together intelligibly)

Nbiom  = length(biom_target) #--the end result must have the same length.

# ---- Assume that we already have a well-defined methylraw object as an input. 
# ---- (so we don't need to do this: Methobj_in = methRead(file_in, sampleID_name , refgenome, mincov = 1 )
Methob_filtered_GR   = as(  regionCounts( Methobj_in, biom_target, strand.aware = FALSE, cov.bases=mincov), "GRanges")

#--- figure out where to map the hits to:
ranges_hits                                     = findOverlaps( Methob_filtered_GR, biom_target )

#============ define the output ==============
result = GRanges( seqnames = seqnames(biom_target), ranges= ranges(biom_target) )  

result$cov   = matrix(0,Nbiom,1); 
result$cov[   as.matrix(ranges_hits)[,2] ] = Methob_filtered_GR$coverage;

result$numCs = matrix(0,Nbiom,1);
result$numCs[ as.matrix(ranges_hits)[,2] ] = Methob_filtered_GR$numCs;

result$numTs = matrix(0,Nbiom,1)
result$numTs[ as.matrix(ranges_hits)[,2] ] = Methob_filtered_GR$numTs

return(result)
}


#==================================================================
#--- inner product:
get_overlap <- function( v1, v2)
{
  L=length(v1)
  if(L != length(v2))
    stop("lengths dont agree")
  return( sum(v1*v2)/L )
}
  
#==================================================================
#--- grab inner products for a whole matrix.
get_overlap_matrix <- function( input_M)
{
  temp=dim(input_M)
  NROI = temp[1];
  NCT  = temp[2];

  result= matrix(0, NCT, NCT)
  
  #---normalize
  N=matrix(0, NROI, NCT)
  
  for( i in c(1:NCT) )
    {
    N[,i]=input_M[,i]/ sqrt( sum(input_M[,i]*input_M[,i] ))
    }
  
  for( i in c(1:NCT) )
    {
    for( j in c(1:NCT) )
      {
      result[i,j]= sum( N[,i]*N[,j] ) 
      }
    }
  return(result)
}


#==================================================================
#--- Convert methylRaw object into a GRanges object.
convert_methlist_to_GR   <-function(  CT_list = NULL, 
                                      methlist_in = NULL, 
                                      refdat = NULL, 
                                      mincov = 1 )
{
  NCT = length(CT_list)
  
  biom_GR  = list()
  for( i in c(1:NCT) )
    {
    biom_GR[[i]] = list()
    }
  names(biom_GR) <- CT_list
  
  if ( NCT    != dim( refdat$mu_ref)[2] )
    stop("NCT definition incompatible with signature matrix");
  
  NROI = length(refdat$Biomarks_gr)
  
  for( i in c(1:NCT) )
  {
    for ( j in c(0:(NSampleBlocks-1) ) )
    {
      print( paste(" processing Sample block ", as.character(j), " of sample ", CT_list[i] , "into GRanges object " ) )
      biom_GR[[i]][[j+1]]  = get_GRoverlap_with_zeropadding( sample_methlist[[i]][[j+1]],  
                                                             refdat$Biomarks_gr, 
                                                             mincov=1 ) 
      #---mincov is set to one here: we don't do any filtering at this stage.
    }
  }
  return(biom_GR)
}
#==================================================================
#--- pre-multiply a reference profile noise factor (for efficiency's sake).
build_G    <-function(  refdat_in = NULL )
{
  return ( (1-refdat_in$Sigmat)*(refdat_in$Sigmat)/( refdat_in$covmat ) )
}
