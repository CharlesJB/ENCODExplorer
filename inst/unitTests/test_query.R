if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

encodexplorer_data <- get_encode_df()

test.fixed_option <- function() {
  obs = tryCatch(queryEncode(target = "rabbit iGG", fixed=TRUE),
                 error=function(e) e, warning=conditionMessage)
  msg = "queryEncode with approximative target name and fixed = T shouldn't return any dataset results"
  
  checkTrue(!is.null(obs), msg)
}

test.query_with_target <- function() {
  encode_df <- encodexplorer_data 
  res_regexp = queryEncode(df=encode_df, target = "rabbit iGG", fixed=TRUE, quiet=TRUE, fuzzy=TRUE)
  checkEquals(sum(!grepl("experiments", x=res_regexp$dataset_type)), 0,
              msg = "2.query with a target filter shouldn't return any dataset results" )
  checkTrue(sum(grepl("experiments", x=res_regexp$dataset_type)) > 0,
            msg = "queryEncode with approximative target name and fuzzy = T should return at least one result" )
}


test.combined_query <- function() {
  encode_df <- encodexplorer_data 
  res_combo = queryEncode(df=encode_df, biosample_name = "A549", assay = "chipseq",
                          file_format = "bigwig", fixed = TRUE, quiet=TRUE, fuzzy=TRUE,
                          file_status="all", status="all")
  checkTrue("ENCFF366WNG" %in% as.character(res_combo$file_accession),
            msg = "this combined query should return a results set containing the id ENCFF366WNG")
}

#test.query_4_dataset <- function() {
  #data("encode_df")
  #res_dataset_only = queryEncode(df=encode_df, set_accession = "ENCSR180GCH")
  #checkEquals(sum(grepl("experiments", x=res_dataset_only$dataset_type)), 0,
              #msg = "this query shouldn't return any experiment results" )
#
  #checkEquals(sum(!grepl("experiments", x=res_dataset_only$dataset_type)), 2,
              #msg = "this query should return this precise result")
#}

test.query_revoked_file <- function() {
  encode_df <- encodexplorer_data 
  res_revok = queryEncode(assay = "chipseq", biosample_name = "mcf7", fixed = TRUE,
                          file_status = "revoked", df=encode_df, fuzzy=TRUE)

  checkTrue("ENCFF000ZKM" %in% as.character(res_revok$file_accession),
            msg = "this combined query should return a result set containg ENCFF000ZKM")
}

test.query_two_terms <- function() {
  assay1 = "small RNA-seq"
  assay2 = "polyA plus RNA-seq"
  res_two_terms = queryEncode(assay = c(assay1, assay2), biosample_name = "HeLa-S3", fixed = TRUE)

  checkTrue(assay1 %in% res_two_terms$assay && assay2 %in% res_two_terms$assay,
            msg = paste0("this combined query should return a result set containg both ", assay1, " and ", assay2))
}

test.query_fuzzy_prefix <- function() {
  assay1 = "small RNA-seq"
  assay2 = "polyA plus RNA-seq"
  res_fuzzy = queryEncode(assay = "RNA-seq", biosample_name = "HeLa-S3", fixed = TRUE, fuzzy = TRUE)

  checkTrue(assay1 %in% res_fuzzy$assay && assay2 %in% res_fuzzy$assay,
            msg = "this fuzzy query should return all assays which have RNA-seq as a substring")
}

test.query_regex <- function() {
  assay1 = "small RNA-seq"
  assay2 = "polyA plus RNA-seq"
  res_two_terms = queryEncode(assay = ".*RNA-seq", biosample_name = "HeLa-S3", fixed = FALSE)

  checkTrue(assay1 %in% res_two_terms$assay && assay2 %in% res_two_terms$assay,
            msg = "this regex query should return all assays which have RNA-seq as a substring")
}

test.query_na <- function() {
  # Run a query twice: once with all treatment values, and once only with NA treatment.
  res_null = queryEncodeGeneric(assay="TF ChIP-seq", biosample_name="A549", 
                                file_format="bed", status="released", assembly="GRCh38",
                                target="NR3C1")
  res_na = queryEncodeGeneric(assay="TF ChIP-seq", biosample_name="A549", 
                              file_format="bed", status="released", assembly="GRCh38",
                              target="NR3C1", treatment=NA)

  # Make sure all treatments are present when treatment was NULL,
  # and that only NA treatments are present when it is explicitly requested.
  dex_in_null = "dexamethasone" %in% res_null$treatment
  na_in_null = NA %in% res_null$treatment
  dex_in_na = "dexamethasone" %in% res_na$treatment
  na_in_na = NA %in% res_null$treatment
                              
  checkTrue(dex_in_null && na_in_null && !dex_in_na && na_in_na,
            msg = "setting treatment = NA should only return results where treatment is NA.")
}
