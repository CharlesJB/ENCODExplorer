
#' Create a design for the files associated with the result of a queryEncode,
#'  fuzzySearch research or a \code{data.table} from createDesign.
#' 
#' @return is a \code{data.table} with files for all the experiments or 
#' a \code{list} of \code{data.table} with all the file per experiment when
#' the parameter split is set to \code{TRUE}
#'
#' @param input The \code{data.table} created by a queryEncode or
#' searchEncode research, or a 
#' @param df The \code{data.table} used to extract the files link.
#' Default :\code{NULL}
#' @param split Allow to the function to return a \code{list} of \code{data.table}
#' where each \code{data.table} contain the files for a single experiment
#' Default: \code{FALSE}.
#' @param fileFormat A string that correspond to the type of the files 
#' that need to be extract.
#' Default: bam
#' @param dataset_type A string that correspong to the type of dataset that
#' will be extrated.
#' Default: experiments
#' @param format The format (long or wide) to represent the data. The 'long' format
#' will contain three columns (File, Experiment, Value). The 'wide' format
#' organize the data as an array with the experiments as columns and files as rows.
#' Default: long
#' @param output_type The type of output of the function, can be \code{data.table}
#' or a \code{data.table}
#' Default: \code{data.table}
#' @param ID A two element numeric vector, that first element is the value assign
#' to replicate and the second is the value assign to control.
#' Default: 1 and 2 
#' @examples
#' fuzzy_result <- fuzzySearch(searchTerm = "brca", df=encode_df, filterVector ="target")
#' design_result <- createDesign(input = fuzzy_result,df=encode_df, fileFormat="fastq")
#' 
#' @import data.table
#' @import stringr 
#' @importFrom tidyr spread
#' @importFrom dplyr filter
#' @importFrom dplyr setdiff
#' 
#' @export
createDesign <- function (input=NULL, df=NULL, split=FALSE, fileFormat="bam",
                          dataset_type="experiments", format="long",
                          output_type="data.table", ID=c(1,2)){
  stopifnot(class(input) %in% c("data.table", "data.frame"))
  stopifnot(output_type %in% c("data.frame", "data.table"))
  stopifnot(length(ID) == 2)
  stopifnot(format %in% c("wide", "long"))
  
  design <- data.table()
  
  if(is.data.frame(input)){
      input <- as.data.table(input)
  }
  
  if(is.null(df)){
      load(file=system.file("../data/encode_df.rda", package="ENCODExplorer"))
      df <- encode_df
  }
  
  stopifnot(dataset_type %in% df$dataset_type)
  stopifnot(fileFormat %in%  unique(df$file_format))
  
  #Filtering character type of ID
  if(is.character(ID)){
      if(sum(as.numeric((is.na(suppressWarnings(as.numeric(ID))))))>0){
          warning("Error : Invalid type of ID, must be numeric value. Default settings will be use",
                  call. = FALSE)
          ID <- c(1,2)
      }else{
        ID <- as.numeric(ID)
      }
  }
  
  #Before we start creating the design, we have to make sure the input object
  #has file for the given fileFormat
  if (nrow(dplyr::filter(input, file_format == fileFormat)) == 0) {
    msg <- c("Error : there is no ", fileFormat, " file available ")
    warning(msg, call. = FALSE)
    return(data.table(File=character(), Experiement=character(), Value=c()))
  }
  
  
  # The first step is to clean the main df to avoid having to do this later
  ff <- fileFormat
  dt <- dataset_type
  clean_df <- df[file_format %in% ff & status == "released" & dataset_type == dt,] 

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
  counts <- stringr::str_count(clean_df[i, controls], ";") + 1
  counts[is.na(counts)] <- 0
  ## ctrl is a data.table with all the replicates and their matching controls,
  ## with only one experiment/control combination. The trick is that if we have a count
  ## of zero, the corresponding experiment will be removed from the vector after the rep
  ## function call.
  ctrl <- data.table(replicates = rep(replicates, counts), ctrl)

  # Create the ctrl part of the design
  get_ctrl_design <- function(dt) {
      File <- clean_df[accession == dt$ctrl, basename(href)]
      if (length(File) > 0) {
          data.table(File = File, Experiment = dt$replicates,
                     Value = ID[2])
      } else {
          data.table(File = character(), Experiment = character(), Value = numeric())
      }
  }
  
  design_ctrl <- ctrl[,get_ctrl_design(.SD), by = .(replicates, ctrl),
               .SDcols = c("replicates","ctrl")]
  design_ctrl <- design_ctrl[, `:=`(replicates = NULL, ctrl = NULL)]

  # Create the replicate part of the design
  design_replicates <- clean_df[accession %in% input$accession,
                         .(File = basename(href), Experiment = accession, Value = ID[1])]

  # Merge the two parts
  design <- rbind(design_replicates, design_ctrl)
  setorder(design,"Experiment")
  # Post-processing
  if (output_type == "data.frame") {
      design <- as.data.frame(design)
  }
  if (split) {
      design <- split(design, design$Experiment)
  }
  if (format == "wide") {
    if (is.data.table(design)) {
      design <- tidyr::spread(design, key = Experiment, value = Value)
    } else {
      design <- lapply(design, tidyr::spread, key = Experiment, value = Value)
    }
  }
  
  if(output_type == "data.frame"){
     return(as.data.frame(design))
  }else{
     return(design) 
  }
  
}
