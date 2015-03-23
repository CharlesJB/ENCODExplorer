# test_database_experiment = "ENCODE/ENCODEdb/R/tests/test_experiment.tsv"
# test_database_dataset = "ENCODE/ENCODEdb/R/tests/test_dataset.tsv"
# encode_ds = read.delim(file = test_database_dataset, quote="", sep="|")
# encode_exp = read.delim(file = test_database_experiment, quote="", sep="|")
# ids = as.character(encode_exp$file_accession)
# ids = c(ids, "ENCFF001LUU", "ENCFF000VIE")
# s = subset(matrices[[1]], matrices[[1]]$file_accession %in% ids)
# encode_exp = s
# remove(s)
library('RUnit')
source("ENCODE/ENCODEdb/R/prepare_data.R")
source("ENCODE/ENCODEdb/R/rest_api.R")

test_file = "ENCODE/ENCODEdb/R/tests/data/ENCFF001VCK.broadPeak.gz.ori"
md5sum_test_file = "b58b4847e8d71d95471c4c017b4a5dc7"
load(file = "ENCODE/ENCODEdb/R/tests/data/test_encode_ds.rda") # encode_ds
load(file = "ENCODE/ENCODEdb/R/tests/data/test_encode_exp.rda") # encode_exp
load(file = "ENCODE/ENCODEdb/R/tests/data/test_rest_query_file.rda") #query_file
load(file = "ENCODE/ENCODEdb/R/tests/data/test_rest_search_file.rda") #search_file
load(file = "ENCODE/ENCODEdb/R/tests/data/test_rest_search_experiment.rda") #search_experiment
load(file = "ENCODE/ENCODEdb/R/tests/data/test_rest_search_dataset.rda") #search_dataset
load(file = "ENCODE/ENCODEdb/R/tests/data/test_mouse_donor_df.rda") #mouse_donor_df

test_prepare_ENCODEdb <- function() {
  
  
}

test_export_ENCODEdb_matrix <- function() {
  
  
}

test_get_encode_types <- function() {
  
  
}

test_extract_table <- function() {
#   res = extract_table(type = "platform")
#   checkTrue(nrow(res) > 0, "this function should return an non empty data.frame")
#   
#   res = extract_table(type = "test")
#   checkTrue(nrow(res) == 0, "this function should return an empty data.frame")
}

test_clean_table <- function() {
  mouse_df_clean = clean_table(mouse_donor_df)
  
  # test the column renaming
  checkTrue(!("@id" %in% names(mouse_df_clean)), msg = "this function should rename @id in id")
  checkTrue(("id" %in% names(mouse_df_clean)), msg = "this function should rename @id in id")
  
  # test column cleaning
  checkTrue(!("@type" %in% names(mouse_df_clean)), msg = "this function should remove @type")
  checkTrue(!("references" %in% names(mouse_df_clean)), msg = "this function should remove empty column")
  
  # check column formatting
  alternate_access = strsplit(as.character(mouse_df_clean[23,]$alternate_accessions), split = ";", fixed = T)[[1]]
  checkEquals(length(alternate_access),3, msg = "this function should tranform list in single vector whom elements are separated by a ;")
  
}

test_search <- function() {
  limit = 4
  searchTerm = "MCF7"
  res = search(searchTerm, limit)
  # test type de retour
  checkTrue(expr = is.data.frame(res), msg = "res should be a data.frame")
  # test taille de la df
  checkEquals(nrow(res), limit,  
              msg = paste0("res should be a ",limit," rows data.frame"))
  
  # test l'existance du champ accession
  checkTrue(expr = !is.null(res$accession), 
            msg = "res should be get an accession entry")
  # test l'absence de resultat pour requete farfelue
  searchTerm = "AABBCC"
  res = search(searchTerm, limit)
  
  # test type de retour
  checkTrue(expr = is.data.frame(res), msg = "res should be a data.frame")
  # test taille de la df
  checkEquals(nrow(res), 0,  
              msg = paste0("res should be a ",limit," rows data.frame"))
}

test_query_transform <- function() {
  my.term = "mcf7"
  my.transfo = "^m[ ,-]?c[ ,-]?f[ ,-]?7"
  
  checkEquals(query_transform(my.term), my.transfo, 
              msg = "1.Error in query transformation")
  
  my.term = "mcf-7"
  checkEquals(query_transform(my.term), my.transfo, 
              msg = "2.Error in query transformation")
  
  my.term = "mcf 7 "
  checkEquals(query_transform(my.term), my.transfo, 
              msg = "3.Error in query transformation")
  
  my.term = "mcf 7"
  checkEquals(query_transform(my.term), my.transfo, 
              msg = "4.Error in query transformation")
}

test_query <- function() {
  res_exact = query(target = "rabbit iGG", fixed=T)
  res_regexp = query(target = "rabbit iGG", fixed=F)
  res_combo = query(biosample = "A549", assay = "chipseq", 
                    file_format = "bigwig", fixed = F)
  res_dataset_only = query(accession = "ENCSR685AIQ")
  res_exp_dataset = query(dataset_access = "ENCSR403MYH")
  
  # check resuls
  checkTrue(expr = is.null(res_exact),
            msg = "query with approximative target name and fixed = T shouldn't return any dataset results" )
  
  checkEquals(nrow(res_regexp$dataset), 0,
              msg = "2.query with a target filter shouldn't return any dataset results" )
  
  checkTrue(nrow(res_regexp$experiment) > 0,
            msg = "query with approximative target name and fixed = F should return at least one result" )
  
  # check combined query
  checkEquals(as.character(res_combo$experiment$file_accession), "ENCFF000VPN", 
              msg = "this combined query should return this precise result")
  
  # check result for dataset only
  checkEquals(nrow(res_dataset_only$experiment), 0,
              msg = "this query shouldn't return any experiment results" )
  
  checkEquals(nrow(res_dataset_only$dataset), 1, 
              msg = "this query should return this precise result")
  
  # check result for dataset and experiment
  checkTrue(nrow(res_exp_dataset$dataset) > 0,
            msg = "this query should return at least one dataset result")
  
  checkTrue(nrow(res_exp_dataset$experiment) > 0,
            msg = "this query should return at least one experiment result")
}

test_downloadFile <- function() {
  
  # check when arguments are missing
  checkTrue(is.null(downloadFile()), msg = "the function should return a NULL value if the result set and its origin are missing")
  checkTrue(is.null(downloadFile(searchResult = query_file)), msg = "the function should return a NULL value if the result set origin is missing")
  checkTrue(is.null(downloadFile(resultOrigin = "search")), msg = "the function should return a NULL value if the result set is missing")
  
  # check when arguments are incorrect
  checkTrue(is.null(downloadFile(searchResult = query_file, resultOrigin = "web")), 
            msg = "the function should return a NULL value if the result set origin is different from search or query")
  checkTrue(is.null(downloadFile(searchResult = query_file, resultOrigin = "query", 
                                 dir = "/aabbbccc")), msg = "the function should return a NULL value if the result set origin is different from search or query")
  
  # check download file
  downloadFile(searchResult = query_file, resultOrigin = "query", format = "bed_broadPeak", dir = "ENCODE/ENCODEdb/R/tests/")  
  checkTrue(file.exists("ENCODE/ENCODEdb/R/tests/ENCFF001VCK.broadPeak.gz"), 
            msg = "the file ENCFF001VCK.broadPeak.gz is missing")
  
  checkEquals(as.character(md5sum("ENCODE/ENCODEdb/R/tests/ENCFF001VCK.broadPeak.gz")),
              md5sum_test_file)
}

test_getFileId <- function() {
  
  # check arguments
  checkTrue(is.null(getFileId(searchResult = query_file, resultOrigin = "search")), 
            msg = "the function should return a NULL value if the result set and its origin seems incompatible")
  checkTrue(is.null(getFileId(searchResult = search_file, resultOrigin = "query")),
            msg = "the function should return a NULL value if the result set and its origin seems incompatible")
  checkTrue(is.null(getFileId(searchResult = search_file, resultOrigin = "search", format = "web")),
            msg = "the function should return a NULL value if the format is unknown")
  checkTrue(is.null(getFileId(searchResult = search_file, resultOrigin = "search", format = "fastq")),
            msg = "the function should return a NULL value if the format is not available in the subset") 
  
  # check results
  fileIds = getFileId(searchResult = search_file, resultOrigin = "search", format = "bed_broadPeak")  
  checkEquals(fileIds, "ENCFF001VCK")
  
  fileIds = getFileId(searchResult = query_file, resultOrigin = "query", format = "bed_broadPeak")  
  checkEquals(fileIds, "ENCFF001VCK")
}

test_getFileDetails <- function() {
  res = getFileDetails(searchResult = search_file)
  checkTrue(is.list(res), msg = "this function should return a list")
  checkTrue(length(res) == 2, msg = "this function should return a list with 2 elements")
}

test_getExperimentDetails <- function() {
  res = getExperimentDetails(searchResult = search_experiment)
  checkTrue(nrow(res) > 0, msg = "this function should return experiment data")
  res = getExperimentDetails(searchResult = search_dataset)
  checkTrue(nrow(res) == 0, msg = "this function shouldn't return any dataset data")
}

test_getDatasetDetails <- function() {
  res = getDatasetDetails(searchResult = search_dataset)
  checkTrue(nrow(res) > 0, msg = "this function should return dataset data")
  res = getDatasetDetails(searchResult = search_experiment)
  checkTrue(nrow(res) == 0, msg = "this function shouldn't return any experiment data")
}

test_get_schemas <- function() {
  
  
}

test_clean_table()
test_downloadFile()
test_export_ENCODEdb_matrix()
test_extract_table()
test_get_encode_types()
test_get_encode_types()
test_get_schemas()
test_getDatasetDetails()
test_getExperimentDetails()
test_getFileDetails()
test_getFileId()
test_prepare_ENCODEdb()
test_query()
test_query_transform()
test_search()






