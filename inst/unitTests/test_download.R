if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}


md5sum_test_file = "b58b4847e8d71d95471c4c017b4a5dc7"

load(file = system.file("extdata/test_rest_query_file.rda", package = "ENCODExplorer")) #query_file

test.argument_acc <- function() {
  obs = tryCatch(downloadEncode("invalid_accession"),error=function(e) e, warning=conditionMessage)
  exp = "No result found for invalid_accession"
  checkIdentical(obs,exp)
}

test.argument_unvail_format <- function() {
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
  obs <- tryCatch(downloadEncode("ENCSR000DZS", dt = brca, format = "fasta"),error=function(e) e, warning=conditionMessage)
  exp <- "No fasta files within experiment ENCSR000DZS"
  checkIdentical(obs,exp)
}

test.argument_format_file <- function() {
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
  obs <- tryCatch(downloadEncode("ENCFF000VSN", dt = brca, format = "bed"),error=function(e) e, warning=conditionMessage)
  exp <- "Format bed not available for files : ENCFF000VSN"
  checkIdentical(obs, exp)
}

test.argument_format_exp <- function() {
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
  obs <- tryCatch(downloadEncode("ENCSR857KDI", dt = brca, format = "bed"),error=function(e) e, warning=conditionMessage)
  exp <- "No bed files within experiment ENCSR857KDI"
  checkIdentical(obs, exp)
}

test.argument_consistency_dir <- function() {
  obs = tryCatch(downloadEncode("ENCSR868RFZ",dir = "/aabbbccc"),
                 error=function(e) e, warning=conditionMessage)
  exp = "Can't write in /aabbbccc"
  checkIdentical(obs, exp)
}



