if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}


test.fixed_option <- function() {
  obs = tryCatch(queryEncode(target = "rabbit iGG", fixed=T),
                 error=function(e) e, warning=conditionMessage)
  msg = "queryEncode with approximative target name and fixed = T shouldn't return any dataset results"
  
  checkTrue(!is.null(obs), msg)
}

test.query_with_target <- function() {
  data("encode_df")
  res_regexp = queryEncode(df=encode_df, target = "rabbit iGG", fixed=F, quiet=T)
  checkEquals(sum(!grepl("experiments", x=res_regexp$dataset_type)), 0,
              msg = "2.query with a target filter shouldn't return any dataset results" )
  checkTrue(sum(grepl("experiments", x=res_regexp$dataset_type)) > 0,
            msg = "queryEncode with approximative target name and fixed = F should return at least one result" )
}


test.combined_query <- function() {
  data("encode_df")
  res_combo = queryEncode(df=encode_df, biosample_name = "A549", assay = "chipseq",
                          file_format = "bigwig", fixed = F, quiet=T)
  checkTrue("ENCFF366WNG" %in% as.character(res_combo$file_accession),
            msg = "this combined query should return a results set containing the id ENCFF000VPN")
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
  data("encode_df")
  res_revok = queryEncode(assay = "chipseq", biosample_name = "mcf7", fixed = F,
                          file_status = "revoked", df=encode_df)

  checkTrue("ENCFF000ZKM" %in% as.character(res_revok$file_accession),
            msg = "this combined query should return a result set containg ENCFF000ZKM")
}
