if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}


test.argument_acc <- function() {
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
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
  load(file = system.file("extdata/BRCA.rda", package = "ENCODExplorer"))
  obs = tryCatch(downloadEncode("ENCSR868RFZ",dir = "/aabbbccc"),
                 error=function(e) e, warning=conditionMessage)
  exp = "Can't write in /aabbbccc"
  checkIdentical(obs, exp)
}



