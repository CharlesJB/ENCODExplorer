# filterVector is a vector character that indicate not to apply the search on specific
# column. Value of 1 is the column is selected and 0 if its not.
# accesion - dataset_type - lab - title - file_type - platform - project - control
# biosample_type - replicate - organism

fuzzySearch <- function(searchTerm=NULL, database=NULL, inputIsList=FALSE,
                        filterVector=NULL, filter=FALSE, fixed = FALSE){
    stopifnot(is.character(searchTerm))
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
        
        for(i in 1:length(filterVector)){
            if(!match[i]){
                print(paste(filterVector[i],"is not a valid filter and it will be remove", sep = " "))
            }
        }
        filterVector <- filterVector[match]
    }
    
    
    # Converting the list of entry in a valid format
    if(inputIsList){
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