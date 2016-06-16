
#' Fuzzysearch is a searching function for a string or a list of string 
#' within the encode_df \code{data.table}
#' @param searchTerm The keyword or a \code{list} of keyword to search. 
#' @param database A \code{data.table}
#' @param filterVector 
#' @param multipleTerm A boolean that indicate if the searchTerm is a list or
#' even multiple searchTerm separete by a comma in a single string.
#' @param fixed Boolean use to apply a strict search for the searchTerm within
#' the database
#' 
#' @return A \code{data.table} corresponding the every row of the database that
#' contain at least of one the searchTerm.
#' @importFrom dplyr filter

fuzzySearch <- function(searchTerm=NULL, database=NULL,filterVector=NULL,
                        multipleTerm=FALSE, fixed = FALSE){
    filter <- FALSE
    if(!(is.list(searchTerm)|is.character(searchTerm)|is.null(searchTerm))){
        print("Invalid searchTerm input")
        return()
    }
    #Converting the list type of input into a single string
    if(is.list(searchTerm)){
        multipleTerm <- TRUE
        searchTerm <- paste(searchTerm, collapse = ",")
    }
    
    
    #Loading the default data.tables from ENCODExplorer package
    if(is.null(database)){
        load(file=system.file("../data/encode_df.rda", package="ENCODExplorer"))
    }else if(is.data.table(database)){
        encode_df <- database
    }else{
        cat("Invalid database input : database entry must be a data.table \n")
        return(NA)
    }
    
    # All the possible filter
    filterNames <- c("accession", "dataset_type", "lab", "title", "file_type", "platform",
                     "project", "type", "control", "biosample_type", "replicate", "organism",
                     "file_accession", "target", "assay", "biosample_name", "file_format")
    if(!is.null(filterVector)){
        filter <- TRUE
        match <- filterVector %in% filterNames
        if(sum(as.logical(match)) != length(filterVector)){
            print(paste("These filter were invalid and will not be considered",
                    filterVector[!match],sep=", "))
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
    
    search_list <- vector(mode="list", ncol(encode_df))
    zeroVector <- rep(0,nrow(encode_df))
    
    # Looking for the searchTerm in every column 
    for (i in 1:length(search_list)) {
        if (filter){
            if(names(encode_df)[[i]] %in% filterVector) {
                search_list[[i]] <- grepl(pattern=searchTerm, x=as.character(encode_df[[i]]),
                                         ignore.case=TRUE, fixed=fixed)
            }else{
                search_list[[i]] <- zeroVector
            }
            
        }else{
            search_list[[i]] <- grepl(pattern=searchTerm, x=as.character(encode_df[[i]]),
                                ignore.case=TRUE, fixed=fixed)
        }
    }
    # Compiling all the logical vector
    toKeep <- as.logical(rowSums(as.data.frame(search_list)))
    encode_df[toKeep,]
}