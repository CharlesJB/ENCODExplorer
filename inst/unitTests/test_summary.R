if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

test.query_consensus_peaks <- function() {
  res = queryConsensusPeaks("GM12878", "GRCh38", "CTCF")
  
  # These should be true regardless of the actual data, so they should not break
  # if encode_df changes.
  
  # Make sur files and file_metadata match,
  checkTrue(length(files(res))==sum(unlist(lapply(file_metadata(res), nrow))))
  sorted_file_names = sort(names(files(res)))
  sorted_file_metadata_names = sort(do.call(rbind, file_metadata(res))$file_accession)
  checkTrue(all(sorted_file_names == sorted_file_metadata_names))

  # Test name assignment.
  new_names = letters[seq_along(res)]
  names(res) <- new_names
  print(names(res))
  print(new_names)
  checkTrue(all(names(res) == new_names))

  # Make sure files are split by the default split.
  checkTrue(all(colnames(metadata(res)) == c(ENCODExplorer:::DEFAULT_CONSENSUS_SPLIT_BY, "split_regions")))
    
  # Test consensus slot.  
  checkTrue(is(consensus(res), "GRangesList"))
  checkTrue(length(res)==length(consensus(res)))
  
  # Test peak slot.
  checkTrue(names(peaks(res)) == names(res))
  checkTrue(is(peaks(res), "list"))
  checkTrue(all(unlist(lapply(peaks(res), function(x) { is(x, "GRangesList")}))))
  checkTrue(sum(unlist(lapply(peaks(res), length)))==length(files(res)))
  
  # Test print. Just making sure it doesn't crash.
  print(res)
  
  # These were functional when written using version 0.99.1 of ENCODExplorerData
  # (2019-04-12 build). Test we're fetching the right files.
  res = queryConsensusPeaks("GM12878", "GRCh38", "CTCF")
  checkTrue(names(res) == "All")
  checkTrue(length(res) == 1)
  checkTrue(length(files(res))==3)
  checkTrue(length(consensus(res)$All)==770)
}

test.query_expression_levels <- function() {
  res = queryGeneExpression("GM12878")
  
  # These should be true regardless of the actual data, so they should not break
  # if encode_df changes.
  
  # Make sur files and file_metadata match,
  checkTrue(length(files(res))==sum(unlist(lapply(file_metadata(res), nrow))))
  sorted_file_names = sort(names(files(res)))
  sorted_file_metadata_names = sort(do.call(rbind, file_metadata(res))$file_accession)
  checkTrue(all(sorted_file_names == sorted_file_metadata_names))

  # Test name assignment.
  new_names = letters[seq_along(res)]
  names(res) <- new_names
  print(names(res))
  print(new_names)
  checkTrue(all(names(res) == new_names))

  # Make sure files are split by the default split.
  meta_col_names = colnames(metadata(res))
  expected_names = c(ENCODExplorer:::DEFAULT_EXPRESSION_SPLIT_BY, "split_regions")
  checkTrue(all(meta_col_names == expected_names))
  
  # Test tpm, fpkm slots
  checkTrue(nrow(tpm(res))==nrow(fpkm(res)))
  checkTrue(all(tpm(res)$id==fpkm(res)$id))
  checkTrue(nrow(tpm(res)) > 0)
  checkTrue(all(colnames(tpm(res))==colnames(fpkm(res))))
  checkTrue(ncol(tpm(res))==(length(res)+1))
  
  print(res)
  
  # These were functional when written using version 0.99.1 of ENCODExplorerData
  # (2019-04-12 build)
  res = queryGeneExpression("GM12878")
  checkTrue(length(res) == 3)
  checkTrue(length(files(res))==6)
  checkTrue(nrow(tpm(res))==61471)
  
}
