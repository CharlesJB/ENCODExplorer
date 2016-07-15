if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

library("RUnit")
test.argument_acc <- function() {
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
  obs = tryCatch(downloadEncode("invalid_accession", df = brca),error=function(e) e, warning=conditionMessage)
  exp = "No result found for invalid_accession"
  RUnit::checkIdentical(obs,exp)
}

test.argument_unvail_format <- function() {
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
  obs <- tryCatch(downloadEncode("ENCSR000DZS", df = brca, format = "fasta"),error=function(e) e, warning=conditionMessage)
  exp <- "Unavailable format fasta"
  RUnit::checkIdentical(obs,exp)
}

test.argument_format_file <- function() {
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
  obs <- tryCatch(downloadEncode("ENCFF000VSN", df = brca, format = "bed"),error=function(e) e, warning=conditionMessage)
  exp <- "Format bed not available for files : ENCFF000VSN"
  RUnit::checkIdentical(obs, exp)
}

test.argument_format_exp <- function() {
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
  obs <- tryCatch(downloadEncode("ENCSR857KDI", df = brca, format = "bed"),error=function(e) e, warning=conditionMessage)
  exp <- "No bed files within experiment ENCSR857KDI"
  RUnit::checkIdentical(obs, exp)
}

test.argument_consistency_dir <- function() {
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
  obs = tryCatch(downloadEncode("ENCSR868RFZ",df = brca,dir = "/aabbbccc"),
                 error=function(e) e, warning=conditionMessage)
  exp = "Can't write in /aabbbccc"
  RUnit::checkIdentical(obs, exp)
}