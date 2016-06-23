if(FALSE) {
    library( "RUnit" )
    library( "ENCODExplorer" )
}
load(file = system.file("inst/extdata/BRCA.rda", package = "ENCODExplorer")) #data.table to use

test.single_term <- function() {
    obs <- fuzzySearch("hela", BRCA)
    checkIdentical(nrow(obs),9L)
    #Testing fixed parameter
    obsFixed <- fuzzySearch("HeLa-S3", BRCA, fixed=T)
    checkIdentical(obs,obsFixed)
    
    obs <- fuzzySearch("hela", BRCA, fixed=T)
    checkIdentical(nrow(obs),0L)
}

test.multiple_term <- function () {
    #Testing the three possible way to pass multiple searchTerm
    #Case 1 : single string, term seperate by comma
    obs <- fuzzySearch("reads, signal", BRCA, multipleTerm = TRUE)
    #Case 2 : list of string
    terms <- list("reads", "signal")
    obsList <- fuzzySearch(terms, BRCA, multipleTerm = TRUE)
    checkIdentical(obs,obsList)
    #Case 3 : character vector
    vec <- c("reads","signal")
    obsVec <- fuzzySearch(vec, BRCA, multipleTerm = TRUE)
    checkIdentical(obs,obsVec)
    
    #All the searchTerm in a list must be character
    terms <- list("reads", 4L)
    obs <- tryCatch(fuzzySearch(terms, RBCA, multipleTerm = TRUE),error=function(e) e, warning=conditionMessage)
    exp <- "Error : All the searchTerm must be character"
    checkIdentical(obs, exp)
}

test.filter <- function () {
    #Test invalid filter
    obs <- tryCatch(fuzzySearch("hepG2", BRCA, filterVector = c("biosample_name", "banana","ice cream")),error=function(e) e, warning=conditionMessage)
    exp <- "This filter is unavailable and will not be considered : banana\nThis filter is unavailable and will not be considered : ice cream\n"
    checkIdentical(obs, exp)
    
    #Testing the handling protocol for invalid filter
    obs <-suppressWarnings(fuzzySearch("hepG2", BRCA, filterVector = c("biosample_name", "banana","ice cream")))
    exp <- fuzzySearch("hepG2", BRCA, filterVector = c("biosample_name"))
    checkIdentical(obs, exp)
    
    
}