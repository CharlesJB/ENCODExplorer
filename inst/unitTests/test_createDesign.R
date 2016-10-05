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
    RUnit::checkIdentical(dim(obs),c(10L,3L))
    RUnit::checkIdentical(obs[1], data.table(File="ENCFF000XAH.bam",
                                      Experiment="ENCSR000EDB", Value=1))
    RUnit::checkIdentical(obs[2], data.table(File="ENCFF000XAI.bam",
                                      Experiment="ENCSR000EDB", Value=1))
    RUnit::checkIdentical(obs[4], data.table(File="ENCFF565WKC.bam",
                                      Experiment="ENCSR000EDB", Value=2))
    RUnit::checkIdentical(obs[6], data.table(File="ENCFF000XPF.bam",
                                      Experiment="ENCSR000EDY", Value=1))
}

test.design_wide <- function(){
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    obs <- createDesign(brca, encode_df, format="wide")
    RUnit::checkIdentical(dim(obs),c(10L,3L))
    RUnit::checkIdentical(obs[1], data.table(File="ENCFF000XAH.bam",
                                      ENCSR000EDB=1, ENCSR000EDY=as.numeric(NA)))
    RUnit::checkIdentical(obs[2], data.table(File="ENCFF000XAI.bam",
                                      ENCSR000EDB=1, ENCSR000EDY=as.numeric(NA)))
    RUnit::checkIdentical(obs[3], data.table(File="ENCFF000XFO.bam",
                                      ENCSR000EDB=2, ENCSR000EDY=as.numeric(NA)))
    RUnit::checkIdentical(obs[4], data.table(File="ENCFF000XPF.bam",
                                      ENCSR000EDB=as.numeric(NA),ENCSR000EDY=1))
    RUnit::checkIdentical(obs[5], data.table(File="ENCFF000XPH.bam",
                                      ENCSR000EDB=as.numeric(NA), ENCSR000EDY=1))
    RUnit::checkIdentical(obs[6], data.table(File="ENCFF000XSJ.bam",
                                      ENCSR000EDB=as.numeric(NA), ENCSR000EDY=2))
}

test.design_split_long <- function(){
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    obs <- createDesign(brca, encode_df, split=T)
    RUnit::checkIdentical(length(obs), 2L)
    RUnit::checkIdentical(obs[[1]][1], data.table(File="ENCFF000XAH.bam",
                                           Experiment="ENCSR000EDB", Value=1))
    RUnit::checkIdentical(obs[[1]][2], data.table(File="ENCFF000XAI.bam",
                                           Experiment="ENCSR000EDB", Value=1))
    RUnit::checkIdentical(obs[[1]][3], data.table(File="ENCFF000XFO.bam",
                                           Experiment="ENCSR000EDB", Value=2))
    RUnit::checkIdentical(obs[[2]][1], data.table(File="ENCFF000XPF.bam",
                                           Experiment="ENCSR000EDY", Value=1))
    RUnit::checkIdentical(obs[[2]][2], data.table(File="ENCFF000XPH.bam",
                                          Experiment="ENCSR000EDY", Value=1))
    RUnit::checkIdentical(obs[[2]][3], data.table(File="ENCFF050KJB.bam",
                                           Experiment="ENCSR000EDY", Value=2))
}

test.design_split_wide <- function(){
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    obs <- createDesign(brca, encode_df, format="wide", split=T)
    RUnit::checkIdentical(length(obs), 2L)
    RUnit::checkIdentical(obs[[1]][1], data.table(File="ENCFF000XAH.bam",
                                           ENCSR000EDB=1))
    RUnit::checkIdentical(obs[[1]][2], data.table(File="ENCFF000XAI.bam",
                                           ENCSR000EDB=1))
    RUnit::checkIdentical(obs[[1]][3], data.table(File="ENCFF000XFO.bam",
                                           ENCSR000EDB=2))
    RUnit::checkIdentical(obs[[2]][1], data.table(File="ENCFF000XPF.bam",
                                           ENCSR000EDY=1))
    RUnit::checkIdentical(obs[[2]][2], data.table(File="ENCFF000XPH.bam",
                                           ENCSR000EDY=1))
    RUnit::checkIdentical(obs[[2]][3], data.table(File="ENCFF000XSJ.bam",
                                           ENCSR000EDY=2))
}

test.design_ID <- function() {
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    #Testing ID input
    obs <- tryCatch(createDesign(brca, encode_df, ID=c("X","Y")),error=function(e) e, warning=conditionMessage)
    exp <- "Error : Invalid type of ID, must be numeric value. Default settings will be use"
    RUnit::checkIdentical(as.character(obs),exp)
    
    obs <- tryCatch(createDesign(brca, encode_df, ID=c(1)),error=function(e) e, warning=conditionMessage)
    exp <- "Error: length(ID) == 2 is not TRUE\n"
    RUnit::checkIdentical(as.character(obs),exp)
}
test.design_format <- function() {
    load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
    brca <- dplyr::filter(brca, accession %in% c("ENCSR000EDY","ENCSR000EDB"))
    #Testing file_type 
    obs <- tryCatch(createDesign(brca, encode_df,fileFormat="banana"),error=function(e) e, warning=conditionMessage)
    exp <- "Error: fileFormat %in% unique(df$file_format) is not TRUE\n"
    RUnit::checkIdentical(as.character(obs),exp)
    #Testing dataset_type
    obs <- tryCatch(createDesign(brca, encode_df,dataset_type="banana"),error=function(e) e, warning=conditionMessage)
    exp <- "Error: dataset_type %in% df$dataset_type is not TRUE\n"
    RUnit::checkIdentical(as.character(obs),exp)
}
