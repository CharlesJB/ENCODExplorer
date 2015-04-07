if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}


test.fixed_option <- function() {
  obs = tryCatch(queryEncode(target = "rabbit iGG", fixed=T),error=function(e) e, warning=conditionMessage)
  exp = "No result found. You can try the <searchEncode> function or set the fixed option to FALSE"
  msg = "queryEncode with approximative target name and fixed = T shouldn't return any dataset results"
  
  checkIdentical(obs, exp, msg)
}

test.query_with_target <- function() {
  res_regexp = queryEncode(target = "rabbit iGG", fixed=F)
  checkEquals(nrow(res_regexp$dataset), 0,
              msg = "2.query with a target filter shouldn't return any dataset results" )
  checkTrue(nrow(res_regexp$experiment) > 0,
            msg = "queryEncode with approximative target name and fixed = F should return at least one result" )
}


test.combined_query <- function() {
  res_combo = queryEncode(biosample = "A549", assay = "chipseq", 
                              file_format = "bigwig", fixed = F)
  checkTrue("ENCFF000VPN" %in% as.character(res_combo$experiment$file_accession), 
            msg = "this combined query should return a results set containing the id ENCFF000VPN")
}

test.query_4_dataset <- function() {
  res_dataset_only = queryEncode(set_accession = "ENCSR685AIQ")
  checkEquals(nrow(res_dataset_only$experiment), 0,
              msg = "this query shouldn't return any experiment results" )
  
  checkEquals(nrow(res_dataset_only$dataset), 1, 
              msg = "this query should return this precise result")
}

test.query_4_dataset_and_exp <- function() {
  res_exp_dataset = queryEncode(dataset_access = "ENCSR403MYH")
  checkTrue(nrow(res_exp_dataset$dataset) > 0,
            msg = "this query should return at least one dataset result")
  
  checkTrue(nrow(res_exp_dataset$experiment) > 0,
            msg = "this query should return at least one experiment result")
}

test.combined_query_on_custom_df <- function() {
  load(file = system.file("extdata/test_small_encode_df.rda", package = "ENCODExplorer"))
  res_combo_custom = queryEncode(df = small_encode_df, target = "ELAVL1-human", 
                           fixed = F, file_status = "all")
  
  checkEquals(as.character(res_combo_custom$experiment$file_accession), "ENCFF001VCK", 
              msg = "this combined query should return this precise result")
  
}

test.query_revoked_file <- function() {
  res_revok = queryEncode(assay = "chipseq", biosample = "mcf7", fixed = F, 
                    file_status = "revoked")
  
  checkTrue("ENCFF000RYT" %in% as.character(res_revok$experiment$file_accession), 
              msg = "this combined query should return a result set containg ENCFF000RYT")
}

