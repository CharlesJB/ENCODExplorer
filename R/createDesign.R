
#' Create a design for the files associated with the result of a queryEncode,
#' searchEncode or fuzzySearch research.
#' 
#' @return is a \code{data.frame} with files for all the experiments or 
#' a \code{list} of \code{data.frame} with all the file per experiment when
#' the parameter split is set to \code{TRUE}
#'
#' @param input The \code{data.frame} created by a queryEncode or
#' searchEncode research.
#' @param df The \code{data.frame} used to extract the files link.
#' Default :\code{NULL}
#' @param split Allow to the function to return a \code{list} of \code{data.frame}
#' where each \code{data.frame} contain the files for a single experiment
#' Default: \code{FALSE}.
#' @param file_type A string that correspond to the type of the files 
#' that need to be extract.
#' Default: bam
#' @param dataset_type A string that correspong to the type of dataset that
#' will be extrated.
#' Default: experiments
#' @param format The format (long or wide) to represent the data. The 'long' format
#' will contain three columns (File, Experiment, Value). The 'wide' format
#' organize the data as an array with the experiments as columns and files as rows.
#' Default: long
#' @param output_type The type of output of the function, can be \code{data.frame}
#' or a \code{data.table}
#' Default: \code{data.frame}
#' @param ID A two element numeric vector, that first element is the value assign
#' to replicate and the second is the value assign to control.
#' Default: 1 and 2 
#'     
#' @import data.table
#' @import stringr
#' @importFrom tidyr spread
#' @importFrom dplyr filter
#' 
createDesign <- function (input=NULL, df=NULL, split=FALSE, file_type="bam",
                          dataset_type="experiments",format="long",
                          output_type="data.table", ID=c(1,2)){
  stopifnot(class(input) %in% c("data.table", "data.frame"))
  stopifnot(output_type %in% c("data.frame", "data.table"))
  stopifnot(file_type %in% c("bam", "fastq", "fasta", "sam", "bed", "bigbed", "bigWig"))
  stopifnot(length(ID) == 2)
  stopifnot(format %in% c("wide", "long"))
  stopifnot(dataset_type %in% input$dataset_type)
  
  design <- data.table()
  
  if(is.null(df)){
      load(file=system.file("../data/encode_df.rda", package="ENCODExplorer"))
      df <- encode_df
  }
  
  if(is.character(ID)){
      ID <- as.numeric(ID)
  }
  
  # The first step is to clean the main df to avoid having to do this later
  ft <- file_type
  dt <- dataset_type
  clean_df <- df[file_type %in% ft & status == "released" & dataset_type == dt,] 

  # Get experiment/controls matches.
  ## One challenge is that the same control can be used by more than one experiment.
  ## Another challenge is that some experiment don't have a matching control.
  ## Finally, there can also be more than one control per experiment (separated by "; ").
  replicates <- clean_df[accession %in% input$accession, unique(accession)]
  i <- match(replicates, clean_df$accession)
  ctrl <- clean_df[i, controls]
  ctrl <- unlist(strsplit(ctrl, "; "))
  ctrl <- ctrl[!is.na(ctrl)]
  ## The counts variable is the same length as the replicates variable and will contain the
  ## number of controls for each experiment.
  counts <- str_count(clean_df[i, controls], ";") + 1
  counts[is.na(counts)] <- 0
  ## ctrl is a data.table with all the replicates and their matching controls,
  ## with only one experiment/control combination. The trick is that if we have a count
  ## of zero, the corresponding experiment will be removed from the vector after the rep
  ## function call.
  ctrl <- data.table(replicates = rep(replicates, counts), ctrl)

  # Create the ctrl part of the design
  get_ctrl_design <- function(dt) {
    data.table(File = clean_df[dataset == dt$ctrl, href], Experiment = dt$replicates,
               Value = 2)
  }
  design_ctrl <- ctrl[,get_ctrl_design(.SD), by = .(replicates, ctrl),
               .SDcols = c("replicates","ctrl")]
  design_ctrl <- design_ctrl[, `:=`(replicates = NULL, ctrl = NULL)]

  # Create the replicate part of the design
  design_replicates <- clean_df[accession %in% input$accession,
                         .(File = href, Experiment = accession, Value = 1)]

  # Merge the two parts
  design <- rbind(design_replicates, design_ctrl)
  
  # Post-processing
  if (output_type == "data.frame") {
      design <- as.data.frame(design)
  }
  if (split) {
      design <- split(design, design$Experiment)
  }
  if (format == "wide") {
    if (is.data.frame(design)) {
      design <- spread(design, key = Experiment, value = Value)
    } else {
      design <- lapply(design, spread, key = Experiment, value = Value)
    }
  }
  
  if(output_type == "data.frame"){
     return(as.data.frame(design))
  }else{
     return(design) 
  }
  
}
