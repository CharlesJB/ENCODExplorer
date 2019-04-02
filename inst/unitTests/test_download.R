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

test.download_valid <- function() {
    q_results = queryEncodeGeneric(biosample_name="A549", 
                                   file_type="bed narrowPeak", 
                                   target="BHLHE40")
    d_results = downloadEncode(q_results)
    d_result_files = gsub("Success downloading file : ", "", d_results)
    
    checkTrue(all(file.exists(d_result_files)))
    unlink(d_result_files)
    
    # Download again with force=FALSE, should fail... Or not.
    # file.remove(d_result_files)
    # file.create(d_result_files)
    # d_results = downloadEncode(q_results, force=FALSE)
    
    # Test specifying non-canonical df
    q_mixed_up = q_results
    q_mixed_up$file_accession = c("ABC", "DEF")
    d_results = downloadEncode(c("ABC", "DEF"), df=q_mixed_up)
    checkTrue(all(file.exists(d_result_files)))
    unlink(d_result_files)
    
    # Test using other directory.
    dir.create("Test_dir", showWarnings=FALSE)
    d_results = downloadEncode(q_results, dir="Test_dir")
    checkTrue(all(file.exists(file.path("Test_dir", d_result_files))))
    unlink("Test_dir", recursive=TRUE)
}
