fuzzySearch <- function(searchTerm = NULL){
    load(file=system.file("../data/encode_df.rda", package="ENCODExplorer"))
    stopifnot(!is.null(searchTerm))
    
    search_list <- vector(mode="list", ncol(encode_df))
    for(i in 1:length(search_list)){
        search_list[[i]] <- grepl(pattern=searchTerm, x=as.character(encode_df[[i]]),
                                ignore.case = TRUE)
    }
    search_list
    toKeep <- as.logical(rowSums(as.data.frame(search_list)))
    encode_df[toKeep,]
}