if(FALSE) {
    library( "dplyr" )
    library( "RUnit" )
    library( "ENCODExplorer" )
}
library( "RUnit" )
library("data.table")
test.design_long <- function(){
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    obs <- createDesign(brca,encode_df)
    RUnit::checkIdentical(dim(obs),c(22L,3L))
}

test.design_wide <- function(){
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    obs <- createDesign(brca, encode_df, format="wide")
    RUnit::checkIdentical(dim(obs),c(22L,3L))
}

test.design_split_long <- function(){
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    obs <- createDesign(brca, encode_df, split=TRUE)
    RUnit::checkIdentical(length(obs), 2L)
}

test.design_split_wide <- function(){
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    obs <- createDesign(brca, encode_df, format="wide", split=TRUE)
    RUnit::checkIdentical(length(obs), 2L)
}

test.design_ID <- function() {
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    #Testing ID input
    obs <- tryCatch(createDesign(brca, encode_df, ID=c("X","Y")),error=function(e) e, warning=conditionMessage)
    exp <- "Error : Invalid type of ID, must be numeric value. Default settings will be use"
    RUnit::checkIdentical(as.character(obs),exp)
    
    obs <- tryCatch(createDesign(brca, encode_df, ID=c(1)),error=function(e) e, warning=conditionMessage)
    obs <- as.character(obs)
    exp <- "Error: ID must be of length 2.\n"
    RUnit::checkIdentical(as.character(obs),exp)
}

test.design_format <- function() {
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    #Testing file_type 
    obs <- tryCatch(createDesign(brca, encode_df,fileFormat="banana"),error=function(e) e, warning=conditionMessage)
    obs <- as.character(obs)
    exp <- "Error: fileFormat must be a value present within df$file_format.\n"
    RUnit::checkIdentical(as.character(obs),exp)
    #Testing dataset_type
    obs <- tryCatch(createDesign(brca, encode_df,dataset_type="banana"),error=function(e) e, warning=conditionMessage)
    obs <- as.character(obs)
    exp <- "Error: dataset_type must be a value present within df$dataset_type.\n"
    RUnit::checkIdentical(as.character(obs),exp)
}
