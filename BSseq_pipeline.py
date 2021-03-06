# PiGx BSseq Pipeline.
#
# Copyright © 2017, 2018 Bren Osberg <Brendan.Osberg@mdc-berlin.de>
# Copyright © 2017 Alexander Gosdschan <alexander.gosdschan@mdc-berlin.de>
# Copyright © 2017 Katarzyna Wreczycka <katwre@gmail.com>
# Copyright © 2017, 2018 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import os

#--- DEFINE OUTPUT DIRECTORIES TO BE PRODUCED 
OUTDIR = config['locations']['output-dir']                      #--- current work dir (important for rmarkdown)

DIR_scripts   = os.path.join(config['locations']['pkglibexecdir'], 'scripts/')
DIR_templates = os.path.join(OUTDIR, 'pigx_work/report_templates/')

DIR_deconv      =  os.path.join(OUTDIR, '08_deconvolution/' )
DIR_bigwig      =  os.path.join(OUTDIR, '07_bigwig_files/')
DIR_methcall    =  os.path.join(OUTDIR, '06_methyl_calls/' )
DIR_sorted      =  os.path.join(OUTDIR, '05_sorting_deduplication/' )
DIR_mapped      =  os.path.join(OUTDIR, '04_mapping/' )
DIR_posttrim_QC =  os.path.join(OUTDIR, '03_posttrimming_QC/' )
DIR_trimmed     =  os.path.join(OUTDIR, '02_trimming/' )
DIR_rawqc       =  os.path.join(OUTDIR, '01_raw_QC/' )

DIR_final       = os.path.join(OUTDIR, "Final_Reports/")


#--- DEFINE PATHS AND FILE NAMES:

PATHIN     = "pigx_work/input/"           # location of the data files to be imported (script creates symbolic link)
GENOMEPATH = "pigx_work/refGenome/"       # where the reference genome being mapped to is stored
ASSEMBLY   = config['general']['assembly'] # version of the genome being mapped to

# include function definitions and extra rules
include   : os.path.join(config['locations']['pkglibexecdir'], 'scripts/func_defs.py')
validate_config(config)

#--- LIST THE OUTPUT FILES TO BE PRODUCED: 

# Below is a mapping of rule names to the expected output files they
# produce.  The desired output files are specified in
# "OUTPUT_FILES".  A different set of output files can be
# selected to run fewer rules.

targets = {
    # rule to print all rule descriptions
    'help': {
        'description': "Print all rules and their descriptions.",
        'files': []
    },

    # This is an expensive one-time rule to prepare the genome.
    'genome-prep': {
        'description': "Convert reference genome into Bisulfite analogue.",
        'files': [
            GENOMEPATH+"Bisulfite_Genome/CT_conversion/genome_mfa.CT_conversion.fa",
            GENOMEPATH+"Bisulfite_Genome/GA_conversion/genome_mfa.GA_conversion.fa"
        ]
    },

    'raw-qc': {
        'description': "Perform raw quality control.",
        'files': files_for_sample(list_files_rawQC)
    },

    # This rule is always executed, as trimming is a prerequisite for
    # subsequent rules
    'trimgalore': {
        'description': "Trim the reads.",
        'files': files_for_sample(list_files_TG)
    },

    # fastQC output files are not needed downstream and need to be
    # called explicitly.
    'posttrim-qc': {
        'description': "Perform quality control after trimming.",
        'files': files_for_sample(list_files_posttrim_QC)
    },

    'mapping': {
        'description': "Align and map reads with Bismark.",
        'files': files_for_sample(list_files_bismark)
    },

    'sorting': {
        'description': "Sort bam files.",
        'files': files_for_sample(list_files_sortbam)
    },

    'deduplication': {
        'description': "Deduplicate bam files.",
        'files': files_for_sample(list_files_dedupe)
    },

    'methyl-calling': {
        'description': "Process bam files.",
        'files': files_for_sample(list_files_methylcalling)
    },

    'bigwig': {
        'description': "export bigwig files to separate folder for visualization",
        'files': files_for_sample(list_files_bw_export)
    },

    'deconv': {
        'description': "Deconvolve methylraw files.",
        'files': files_for_sample(list_files_deconv)
    }

}

# selected_targets_default = ['bigwig']
# selected_targets_default = ['deconv', 'bigwig']
selected_targets_default = ['deconv']

# Selected output files from the above set.
selected_targets = config['execution']['target'] or selected_targets_default

# FIXME: the list of files must be flattened twice(!).  We should make
# sure that the targets really just return simple lists.
from itertools import chain
OUTPUT_FILES = list(chain.from_iterable(chain.from_iterable([targets[name]['files'] for name in selected_targets])))

# print("OUTPUT_FILES=")
# print(OUTPUT_FILES)
# ==============================================================================================================
#
#                                         BEGIN RULES
#
# rules are separated by "==" bars into pairs for paired-end and single-end (subdivided by smaller "--" dividers)
# rules are (generally) presented in hierarchical order of dependency (i.e. last to first)
# ===============================================================================================================


rule all:
    input:
        OUTPUT_FILES

rule help:
    run:
        for key in sorted(targets.keys()):
            print('{}:\n  {}'.format(key, targets[key]['description']))

# Record any existing output files, so that we can detect if they have
# changed.
expected_files = {}
onstart:
    if OUTPUT_FILES:
        for name in OUTPUT_FILES:
            if os.path.exists(name):
                expected_files[name] = os.path.getmtime(name)

# Print generated target files.
onsuccess:
    if OUTPUT_FILES:
        # check if any existing files have been modified
        generated = []
        for name in OUTPUT_FILES:
            if name not in expected_files or os.path.getmtime(name) != expected_files[name]:
                generated.append(name)
        if generated:
            print("The following files have been generated:")
            for name in generated:
                print("  - {}".format(name))


# ==========================================================================================
# Export a bigwig file:

rule export_bigwig:
    input:
        seqlengths = os.path.join(DIR_mapped,   "Refgen_"+ASSEMBLY+"_chromlengths.csv"),
        rdsfile    = os.path.join(DIR_methcall, "{bigwig_prefix}_methylRaw.RDS")
    output:
        bw         = os.path.join(DIR_bigwig,   "{bigwig_prefix}.bw")
    message: fmt("exporting bigwig files.")
    shell:
        nice('Rscript', ["{DIR_scripts}/export_bw.R",
                         "{input.rdsfile}",
                         "{input.seqlengths}",
                         ASSEMBLY,
                         "{output}"])


# ==========================================================================================
# Deconvolve the methyl-raw object:

rule deconvolve_se:
    input:
        methraw_in  = os.path.join(DIR_methcall,"{sample}_se_bt2.sorted.deduped_CpG.txt"),
    output:
        cellfracs   = os.path.join(DIR_deconv,"{sample}_SE_deconv_vals.csv"),
        deconvdat   = os.path.join(DIR_deconv,"{sample}_SE_deconv_dat.rds")
    params:
        sampleID          = "{sample}",
        methraw_in        = os.path.join(DIR_methcall,"{sample}_se_bt2.sorted.deduped_CpG.RDS"),
        assembly          = ASSEMBLY,
        Sigmat_biomarkers = "./Sigmats/Sun_PNAS/Sun_sigmat_allbioms.csv", # @@@TODO: generalize this 
        mincov            = 10,
        upper_constraint  = 1,          
        lower_constraint  = 1,          
        path_OUT          = os.path.join(DIR_deconv)
    log:
        os.path.join(DIR_deconv,"{sample}_deconv.log")
    message: 
        fmt("Deconvolve cell types")
    shell:
        nice('Rscript', ["{DIR_scripts}/deconv_from_command_line_main.R",
                         "--Rdeconv_funcdef_PATH={DIR_scripts}",
                         "--ampleID={params.sampleID}",
                         "--methylcall_fin={params.methraw_in}",
                         "--assembly={params.assembly}",
                         "--Sigmat_biomarkers={params.Sigmat_biomarkers}",
                         "--mincov={params.mincov}", 
                         "--upper_constraint={params.upper_constraint}",
                         "--lower_constraint={params.lower_constraint}",
                         "--strandedness='SE'",
                         "--path_OUT={params.path_OUT}"])
                
#-----------------------
rule deconvolve_pe:
    input:
        methraw_in  = os.path.join(DIR_methcall,"{sample}_1_val_1_bt2.sorted.deduped_CpG.txt"),
    output:
        cellfracs   = os.path.join(DIR_deconv,"{sample}_PE_deconv_vals.csv"),
        deconvdat   = os.path.join(DIR_deconv,"{sample}_PE_deconv_dat.rds")
    params:
        sampleID          = "{sample}",
        methraw_in        = os.path.join(DIR_methcall,"{sample}_1_val_1_bt2.sorted.deduped_CpG.txt"),
        assembly          = ASSEMBLY,
        Sigmat_biomarkers = "~/projects/Cell_type_deconvolution/Sigmats/Sun_PNAS/Sun_sigmat_allbioms.csv", # @@@TODO: generalize this 
        mincov            = 10,
        upper_constraint  = 1,          
        lower_constraint  = 1,          
        path_OUT          = os.path.join(DIR_deconv)
    log:
        os.path.join(DIR_deconv,"{sample}_deconv.log")
    message: 
        fmt("Deconvolve cell types")
    shell:
        nice('Rscript', ["{DIR_scripts}/deconv_from_command_line_main.R",
                         "--Rdeconv_funcdef_PATH={DIR_scripts}",
                         "--sampleID={params.sampleID}",
                         "--methylcall_fin={params.methraw_in}",
                         "--assembly={params.assembly}",
                         "--Sigmat_biomarkers={params.Sigmat_biomarkers}",
                         "--mincov={params.mincov}", 
                         "--upper_constraint={params.upper_constraint}",
                         "--lower_constraint={params.lower_constraint}",
                         "--strandedness='PE'",
                         "--path_OUT={params.path_OUT}"])
 # ==========================================================================================
# Process bam files into methyl-called formats:

rule bam_methCall:
    input:
        bamfile     = os.path.join(DIR_sorted,"{prefix}.bam")
    output:
        rdsfile     = os.path.join(DIR_methcall,"{prefix}_methylRaw.RDS"),
        callFile    = os.path.join(DIR_methcall,"{prefix}_CpG.txt")
    params:
        ## absolute path to bamfiles
        inBam       = os.path.join(OUTDIR,DIR_sorted,"{prefix}.bam"),
        assembly    = ASSEMBLY,
        mincov      = int(config['general']['methylation-calling']['minimum-coverage']),
        minqual     = int(config['general']['methylation-calling']['minimum-quality']),
        ## absolute path to output folder in working dir
        rds         = os.path.join(OUTDIR,DIR_methcall,"{prefix}_methylRaw.RDS")
    log:
        os.path.join(DIR_methcall,"{prefix}_meth_calls.log")
    message: fmt("Extract methylation calls from bam file.")
    shell:
        nice('Rscript', ["{DIR_scripts}/methCall.R",
                         "--inBam={params.inBam}",
                         "--assembly={params.assembly}",
                         "--mincov={params.mincov}",
                         "--minqual={params.minqual}",
                         "--rds={params.rds}",
                         "--logFile={log}"])


# ==========================================================================================
# Deduplicate aligned reads from the bam file:

rule deduplication_se:
    input:
        DIR_sorted+"{sample}_se_bt2.sorted.bam"
    output:
        DIR_sorted+"{sample}_se_bt2.sorted.deduped.bam"
    params:
        bam="--bam ",
        sampath="--samtools_path " + tool('samtools')
    log:
        DIR_sorted+"{sample}_deduplication.log"
    message: fmt("Deduplicating single-end aligned reads from {input}")
    shell:
        nice('samtools', [" markdup -rs ", "{input}", "{output}"], "{log}")

#-----------------------
rule deduplication_pe:
    input:
        DIR_sorted+"{sample}_1_val_1_bt2.sorted.bam"
    output:
        DIR_sorted+"{sample}_1_val_1_bt2.sorted.deduped.bam"
    log:
        DIR_sorted+"{sample}_deduplication.log"
    message: fmt("Deduplicating paired-end aligned reads from {input}")
    shell:
        nice('samtools', [" markdup -r ", "{input}", "{output}"], "{log}")


# ==========================================================================================
# Sort the bam file by position (and carry out mate-flagging in paired-end case):

rule sortbam_se:
    input:
        DIR_mapped+"{sample}_trimmed_bismark_bt2.bam"
    output:
        DIR_sorted+"{sample}_se_bt2.sorted.bam"
    message: fmt("Sorting bam file {input}")
    shell:
        nice('samtools', ["sort", "{input}", "-o {output}"])

#-----------------------
rule sortbam_pe:
    input:
        DIR_mapped+"{sample}_1_val_1_bismark_bt2_pe.bam"
    output:
        DIR_sorted+"{sample}_1_val_1_bt2.sorted.bam"
    message: fmt("Sorting bam file {input}")
    shell:
        nice('samtools', ["sort -n ", " {input} ", " | ", tool('samtools'), " fixmate -m  - - ", " | ", tool('samtools'), " sort -o {output} " ])


# ==========================================================================================
# Align and map reads to the reference genome:

bismark_cores = str(config['tools']['bismark']['cores'])

rule bismark_align_and_map_se:
    input:
        refconvert_CT = GENOMEPATH+"Bisulfite_Genome/CT_conversion/genome_mfa.CT_conversion.fa",
	refconvert_GA = GENOMEPATH+"Bisulfite_Genome/GA_conversion/genome_mfa.GA_conversion.fa",
        fqfile = DIR_trimmed+"{sample}_trimmed.fq.gz",
        qc     = DIR_posttrim_QC+"{sample}_trimmed_fastqc.html"
    output:
        DIR_mapped+"{sample}_trimmed_bismark_bt2.bam",
        DIR_mapped+"{sample}_trimmed_bismark_bt2_SE_report.txt"
    params:
        bismark_args = config['tools']['bismark']['args'],
        genomeFolder = "--genome_folder " + GENOMEPATH,
        outdir = "--output_dir  "+DIR_mapped,
        nucCov = "--nucleotide_coverage",
        pathToBowtie = "--path_to_bowtie "+ os.path.dirname(tool('bowtie2')),
        useBowtie2   = "--bowtie2 ",
        samtools     = "--samtools_path "+ os.path.dirname(tool('samtools')),
        tempdir      = "--temp_dir " + DIR_mapped,
        cores = "--multicore " + bismark_cores
    log:
        DIR_mapped+"{sample}_bismark_se_mapping.log"
    message: fmt("Mapping single-end reads to genome {ASSEMBLY}")
    shell:
        nice('bismark', ["{params}", "{input.fqfile}"], "{log}")

rule bismark_align_and_map_pe:
    input:
        refconvert_CT = GENOMEPATH+"Bisulfite_Genome/CT_conversion/genome_mfa.CT_conversion.fa",
	refconvert_GA = GENOMEPATH+"Bisulfite_Genome/GA_conversion/genome_mfa.GA_conversion.fa",
        fin1 = DIR_trimmed+"{sample}_1_val_1.fq.gz",
        fin2 = DIR_trimmed+"{sample}_2_val_2.fq.gz",
        qc   = [ DIR_posttrim_QC+"{sample}_1_val_1_fastqc.html",
                 DIR_posttrim_QC+"{sample}_2_val_2_fastqc.html"]
    output:
        DIR_mapped+"{sample}_1_val_1_bismark_bt2_pe.bam",
        DIR_mapped+"{sample}_1_val_1_bismark_bt2_PE_report.txt"
    params:
        bismark_args = config['tools']['bismark']['args'],
        genomeFolder = "--genome_folder " + GENOMEPATH,
        outdir       = "--output_dir  "+DIR_mapped,
        nucCov       = "--nucleotide_coverage",
        pathToBowtie = "--path_to_bowtie "+ os.path.dirname(tool('bowtie2')),
        useBowtie2   = "--bowtie2 ",
        samtools     = "--samtools_path "+ os.path.dirname(tool('samtools')),
        tempdir      = "--temp_dir "+DIR_mapped,
        cores        = "--multicore "+bismark_cores
    log:
        DIR_mapped+"{sample}_bismark_pe_mapping.log"
    message: fmt("Mapping paired-end reads to genome {ASSEMBLY}.")
    shell:
        nice('bismark', ["{params}", "-1 {input.fin1}", "-2 {input.fin2}"], "{log}")



# ==========================================================================================
# Generate methyl-converted version of the reference genome, if necessary:

rule bismark_genome_preparation:
    input:
        ancient(GENOMEPATH)
    output:
        GENOMEPATH+"Bisulfite_Genome/CT_conversion/genome_mfa.CT_conversion.fa",
        GENOMEPATH+"Bisulfite_Genome/GA_conversion/genome_mfa.GA_conversion.fa"
    params:
        bismark_genome_preparation_args = config['tools']['bismark-genome-preparation']['args'],
        pathToBowtie = "--path_to_bowtie "+ os.path.dirname(tool('bowtie2')),
        useBowtie2   = "--bowtie2 ",
        verbose      = "--verbose "
    log:
        'bismark_genome_preparation_'+ASSEMBLY+'.log'
    message: fmt("Converting {ASSEMBLY} Genome into Bisulfite analogue")
    shell:
        nice('bismark-genome-preparation', ["{params}", "{input}"], "{log}")



# ==========================================================================================
# Create a csv file tabulating the lengths of the chromosomes in the reference genome:

rule tabulate_seqlengths:
    input:
        rules.bismark_genome_preparation.output
    output:
        seqlengths = DIR_mapped+"Refgen_"+ASSEMBLY+"_chromlengths.csv",
    params:
        chromlines = " | " + tool('grep') + " Sequence ",
        chromcols  = " | " + tool('cut') + " -f2,3     ",
        seqnames   = " | " + tool('sed') + " \"s/_CT_converted//g\" "
    message: fmt("Tabulating chromosome lengths in genome: {ASSEMBLY} for later reference.")
    shell:
        nice('bowtie2-inspect', ['-s ' + GENOMEPATH + "Bisulfite_Genome/CT_conversion/BS_CT", '{params.chromlines}', '{params.chromcols}', '{params.seqnames}', ' > {output}'])



# ==========================================================================================
# Carry out post-trimming quality control

rule fastqc_after_trimming_se:
    input:
        DIR_trimmed+"{sample}_trimmed.fq.gz",
    output:
    	DIR_posttrim_QC+"{sample}_trimmed_fastqc.html",
    	DIR_posttrim_QC+"{sample}_trimmed_fastqc.zip"
    params:
        fastqc_args = config['tools']['fastqc']['args'],
        outdir = "--outdir "+DIR_posttrim_QC
    log:
   	    DIR_posttrim_QC+"{sample}_trimmed_fastqc.log"
    message: fmt("Quality checking trimmmed single-end data from {input}")
    shell:
        nice('fastqc', ["{params}", "{input}"], "{log}")

rule fastqc_after_trimming_pe:
    input:
        DIR_trimmed+"{sample}_1_val_1.fq.gz",
        DIR_trimmed+"{sample}_2_val_2.fq.gz"
    output:
    	DIR_posttrim_QC+"{sample}_1_val_1_fastqc.html",
    	DIR_posttrim_QC+"{sample}_1_val_1_fastqc.zip",
    	DIR_posttrim_QC+"{sample}_2_val_2_fastqc.zip",
        DIR_posttrim_QC+"{sample}_2_val_2_fastqc.html"
    params:
        fastqc_args = config['tools']['fastqc']['args'],
        outdir = "--outdir "+DIR_posttrim_QC
    log:
   	    DIR_posttrim_QC+"{sample}_trimmed_fastqc.log"
    message: fmt("Quality checking trimmmed paired-end data from {input}")
    shell:
        nice('fastqc', ["{params}", "{input}"], "{log}")


# ==========================================================================================
# Trim the reads for adapter-ends and quality

rule trim_reads_se:
    input:
       qc   = DIR_rawqc+"{sample}_fastqc.html",
       file = PATHIN+"{sample}.fq.gz"
    output:
       DIR_trimmed+"{sample}_trimmed.fq.gz" #---- this ALWAYS outputs .fq.qz format.
    params:
       extra      = config['tools']['trim-galore']['args'],
       outdir     = "--output_dir "+DIR_trimmed,
       phred      = "--phred33",
       gz         = "--gzip",
       cutadapt   = "--path_to_cutadapt " + tool('cutadapt'),
    log:
       DIR_trimmed+"{sample}.trimgalore.log"
    message: fmt("Trimming raw single-end read data from {input}")
    shell:
       nice('trim-galore', ["{params}", "{input.file}"], "{log}")

rule trim_reads_pe:
    input:
        qc    = [ DIR_rawqc+"{sample}_1_fastqc.html",
                  DIR_rawqc+"{sample}_2_fastqc.html"],
        files = [ PATHIN+"{sample}_1.fq.gz",
                  PATHIN+"{sample}_2.fq.gz"]
    output:
        DIR_trimmed+"{sample}_1_val_1.fq.gz", #---- this ALWAYS outputs .fq.qz format.
        DIR_trimmed+"{sample}_2_val_2.fq.gz",
    params:
        extra          = config['tools']['trim-galore']['args'],
        outdir         = "--output_dir "+DIR_trimmed,
        phred          = "--phred33",
        gz             = "--gzip",
        cutadapt       = "--path_to_cutadapt " + tool('cutadapt'),
        paired         = "--paired"
    log:
        DIR_trimmed+"{sample}.trimgalore.log"
    message:
        fmt("Trimming raw paired-end read data from {input}")
    shell:
        nice('trim-galore', ["{params}", "{input.files}"], "{log}")


# ==========================================================================================
# Perform quality control on raw data

rule fastqc_raw: #----only need one: covers BOTH pe and se cases.
    input:
        PATHIN+"{sample}.fq.gz"
    output:
        DIR_rawqc+"{sample}_fastqc.html",
        DIR_rawqc+"{sample}_fastqc.zip"
    params:
        fastqc_args = config['tools']['fastqc']['args'],
        outdir      = "--outdir "+ DIR_rawqc     # usually pass params as strings instead of wildcards.
    log:
        DIR_rawqc+"{sample}_fastqc.log"
    message: fmt("Quality checking raw read data from {input}")
    shell:
        nice('fastqc', ["{params}", "{input}"], "{log}")


