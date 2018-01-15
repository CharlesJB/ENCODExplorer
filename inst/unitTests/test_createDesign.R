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
    obs <- createDesign(brca, encode_df, split=T)
    RUnit::checkIdentical(length(obs), 2L)
}

test.design_split_wide <- function(){
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    obs <- createDesign(brca, encode_df, format="wide", split=T)
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
    exp <- "Error: length(ID) == 2 is not TRUE\n"
    RUnit::checkIdentical(as.character(obs),exp)
}

test.design_format <- function() {
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    #Testing file_type 
    obs <- tryCatch(createDesign(brca, encode_df,fileFormat="banana"),error=function(e) e, warning=conditionMessage)
    obs <- as.character(obs)
    exp <- "Error: fileFormat %in% unique(df$file_format) is not TRUE\n"
    RUnit::checkIdentical(as.character(obs),exp)
    #Testing dataset_type
    obs <- tryCatch(createDesign(brca, encode_df,dataset_type="banana"),error=function(e) e, warning=conditionMessage)
    obs <- as.character(obs)
    exp <- "Error: dataset_type %in% df$dataset_type is not TRUE\n"
    RUnit::checkIdentical(as.character(obs),exp)
}
