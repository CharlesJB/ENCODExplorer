if(FALSE) {
  library( "RUnit" )
  library( "ENCODEdb" )
}

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
checkTrue("ENCFF000VPN" %in% as.character(res_combo$experiment$file_accession), 
            msg = "this combined query should return a results set containing the id ENCFF000VPN")

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
