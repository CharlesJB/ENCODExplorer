
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
#' @param type_file A string that correspond to the type of the files 
#' that need to be extract.
#' Default: bam
#' @param datatype A string that correspong to the type of dataset that
#' will be extrated.
#' Default: experiments
#' @param format The format (long or wide) to represent the data. The 'long' format
#' will contain three columns (File, Experiment, Value). The 'wide' format
#' organize the data as an array with the experiments as columns and files as rows/
#' Default: long
#' @param output_type The type of output of the function, can be \code{data.frame}
#' or a \code(data.table)
#' Default: \code{data.frame}
#' @param ID A two element numeric vector, that first element is the value assign
#' to replicate and the second is the value assign to control.
#' Default: 1 and 2 
#'     
#' @import dplyr
#' @import tidyr
#' @import data.table
#' @export
createDesign <- function (input=NULL, df=NULL, split=FALSE, type_file="bam", datatype="experiments",
                          format="long", output_type="data.frame", ID=c(1,2)){
  stopifnot(output_type %in% c("data.frame", "data.table"))
  stopifnot(type_file %in% c("bam", "fastq", "sam"))
  stopifnot(length(ID) == 2)
  stopifnot(format %in% c("wide", "long"))
  stopifnot(datatype %in% c("experiments", "ucsc-browser-composites", "annotations",
                            "matched-sets", "projects", "regerence-epigenomes",
                            "references"))
  
  if (output_type == "data.frame") {
    design <- data.frame(stringsAsFactors = FALSE)
  }else{
    design <- data.table()
  }
  
  if(is.null(df)){
      load(file=system.file("../data/encode_df.rda", package="ENCODExplorer"))
      df <- encode_df
  }
  
  # This function use a vector of experiment accession to extract information from df
  extract_files <- function(data_vec){
    result <- data.frame()
    for(i in 1:length(data_vec)){
      # Catching and filtering replicates
      replicates <- filter(df, accession == data_vec[i])
      replicates <- filter(replicates, file_type==type_file & status == "released")
      design_rep <- data.frame(File=replicates$href, Experiment=replicates$accession,
                               Value=rep(x=ID[1], times=nrow(replicates)),
                               stringsAsFactors=FALSE)
      # Catching and filtering controls
      controls_acc <- unique(replicates$controls)
      controls <- filter(df, dataset %in% controls_acc)
      controls <- filter(controls, file_type==type_file & status =="released")
      design_ctrl <- data.frame(File=controls$href, Experiment=rep(replicates$accession[1],
                               times=nrow(controls)), Value=rep(x=ID[2], 
                               times=nrow(controls)), stringsAsFactors=FALSE)
      # Merging the data.frame
      result <- rbind(result, design_rep, design_ctrl)
    }
    
    # Changing format with spread when the format is set to wide
    if(format == "wide"){
      result <- spread(result, key=Experiment, value=Value, fill = NA)
    }
    result
  }
  
  
  # Creating a vector with all the experiments
  # Case 1 : Input from searchENCODE
  if (is.null(input$dataset_type)){
    types <- sapply(input$id, function(x) {gsub(x, pattern = "/(.*)/.*/", replacement = '\\1')})
    input <- cbind(input, types)
    data_toKeep <- filter(input, types == datatype)
  } else {
  # Case 2 : Input from queryENCODE
      data_toKeep <- filter(input, dataset_type == datatype)
  }
  
  data_toKeep <- unique(data_toKeep$accession)
  if(length(data_toKeep) == 0){
      print("length of experiments is 0")
      return(design)
  }
  if(!split) {
    design <- extract_files(data_toKeep)
  } else {
    design <- vector("list", length(data_toKeep))
    for(i in 1:length(data_toKeep)){
      design[[i]] <- extract_files(data_toKeep[i])
    }
    design
  }
  
  design
}