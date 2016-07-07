if(FALSE) {
    library( "RUnit" )
    library( "ENCODExplorer" )
}

test.single_term <- function() {
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
  obs <- fuzzySearch("hela", brca)
  checkIdentical(nrow(obs),9L)
}

test.multiple_term <- function () {
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    #Testing the three possible way to pass multiple searchTerm
    #Case 1 : single string, term seperate by comma
    obs <- fuzzySearch("reads, signal", brca, multipleTerm = TRUE)
    #Case 2 : list of string
    terms <- list("reads", "signal")
    obsList <- fuzzySearch(terms, brca, multipleTerm = TRUE)
    checkIdentical(obs,obsList)
    #Case 3 : character vector
    vec <- c("reads","signal")
    obsVec <- fuzzySearch(vec, brca, multipleTerm = TRUE)
    checkIdentical(obs,obsVec)
    
    #All the searchTerm in a list must be character
    terms <- list("reads", 4L)
    obs <- tryCatch(fuzzySearch(terms, RBCA, multipleTerm = TRUE),error=function(e) e, warning=conditionMessage)
    exp <- "Error : All the searchTerm must be character"
    checkIdentical(obs, exp)
}

test.filter <- function () {
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    #Test invalid filter
    obs <- tryCatch(fuzzySearch("hepG2", brca, filterVector = c("biosample_name", "banana","ice cream")),error=function(e) e, warning=conditionMessage)
    exp <- "Unavailable filter :banana Unavailable filter :ice cream "
    checkIdentical(obs, exp)
    
    #Testing the handling protocol for invalid filter
    obs <- suppressWarnings(fuzzySearch("hepG2", brca, filterVector = c("biosample_name", "banana","ice cream")))
    exp <- fuzzySearch("hepG2", brca, filterVector = c("biosample_name"))
    checkIdentical(obs, exp)
    
    
}