#' Fuzzysearch is a searching function for a string or a list of string 
#' within the encode_df \code{data.table}. For faster processing, pass encode_df
#' object as database parameter.
#' @param searchTerm The keyword or a \code{list} of keyword to search. 
#' @param database A \code{data.table} with similar format as encode_df database.
#' @param filterVector A \code{character} to apply the search on specific column.
#' @param multipleTerm A boolean that indicate if the searchTerm is a list or
#' even multiple searchTerm separete by a comma in a single string.
#' @param ignore_case A \code{boolean} to enable the case sensitivity.
#' 
#' @examples  
#'    fuzz_ex <- fuzzySearch(searchTerm=c("ELAVL1","atf7"),
#'    database=get_encode_df_demo(), filterVector ="target", multipleTerm = TRUE)
#'
#' @return A \code{data.table} corresponding the every row of the database that
#' contain at least of one the searchTerm.
#' 
#' @import data.table
#' @importFrom stringi stri_detect_regex
#' @importFrom dplyr filter
#' @export

fuzzySearch <- function(searchTerm=NULL, database=get_encode_df(),filterVector=NULL,
                        multipleTerm=FALSE, ignore_case=TRUE){
    #Testing if the searchTerm input is valid
    if(!(is.list(searchTerm)|is.character(searchTerm)|is.null(searchTerm))){
        print("Invalid searchTerm input")
        return()
    }
    #Making sure the term is a list are character
    if(is.list(searchTerm) & sum(as.numeric(lapply(searchTerm, is.character))) != length(searchTerm)){
        warning("Error : All the searchTerm must be character", call. = FALSE)
        return()
    }

    #Converting the list type of input, or a vector into a single string
    if((is.list(searchTerm)|(is.character(searchTerm))) && length(searchTerm) > 1){
        multipleTerm <- TRUE
        searchTerm <- paste(searchTerm, collapse = ",")
    }
    
    
    #Loading the default data.tables from ENCODExplorer package
    if(!is.data.table(database)) {
        cat("Invalid database input : database entry must be a data.table \n")
        return(NA)    
    }
    
    # All the possible filter
    filterNames <- c("accession", "dataset_type", "lab", "title", "file_type", "platform",
                     "project", "type", "control", "biosample_type", "replicate", "organism",
                     "file_accession", "target", "assay", "biosample_name", "file_format")
    filter <- FALSE
    if(!is.null(filterVector)){
        filter <- TRUE
        match <- filterVector %in% filterNames
        if(sum(as.logical(match)) != length(filterVector)){
            msg <- paste0("Unavailable filter :", filterVector[!match], sep=" " )
            warning(msg, call. = FALSE)
            filterVector <- filterVector[match]
        }
    }
    
    # Converting the list of entry in a valid format
    if(multipleTerm){
        searchTerm <- gsub(pattern="\n", replacement="", x=searchTerm)
        searchTerm <- gsub(pattern=" , ", replacement="|", x=searchTerm)
        searchTerm <- gsub(pattern=", ", replacement="|", x=searchTerm)
        searchTerm <- gsub(pattern=" ,", replacement="|", x=searchTerm)
        searchTerm <- gsub(pattern=",", replacement="|", x=searchTerm)
    }
    
    #Making a subset of encode_df
    df <- database
    if (filter) {
        df <- database[,filterVector, with = FALSE]
    }
    #fun_detect return a row of 0 and 1 depending on a match with searchTerm
    fun_detect <- function(x) {
            stringi::stri_detect_regex(as.character(x), searchTerm, case_insensitive = ignore_case)
    }
    res <- df[,lapply(.SD, fun_detect)]
    toKeep <- as.logical(rowSums(res, na.rm = TRUE))
    result <- database[toKeep,]
    if(nrow(result)==0){
      cat("No result found \n")
      return(result)
    }else{
      cat(paste0("Results: ", length(unique(result$file_accession))," files, ",
                 length(unique(result$accession))," datasets", "\n"))
    }
    result
}
