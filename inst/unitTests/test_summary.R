if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

test.query_consensus_peaks <- function() {
  res = queryConsensusPeaks("GM12878", "GRCh38", "CTCF", simplify=FALSE)
  
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
  checkTrue(all(names(res) == new_names))

  # Make sure files are split by the default split.
  checkTrue(all(colnames(metadata(res)) == c(ENCODExplorer:::DEFAULT_SPLIT_BY, "split_group")))
    
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
  res = queryConsensusPeaks("GM12878", "GRCh38", "CTCF", simplify=FALSE)
  checkTrue(length(res) == 1)
  checkTrue(length(files(res))==3)
  checkTrue(length(consensus(res)[[1]])==31922)
  
  res = queryConsensusPeaks("GM12878", "GRCh38", "CTCF", simplify=TRUE)
  checkTrue(names(res) == "All")  
}

test.query_expression_levels <- function() {
  res = queryGeneExpression("GM12878", simplify=FALSE)
  
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
  checkTrue(all(names(res) == new_names))

  # Make sure files are split by the default split.
  meta_col_names = colnames(metadata(res))
  expected_names = c("dataset_description", 
                     ENCODExplorer:::DEFAULT_SPLIT_BY, "split_group")
  checkTrue(all(meta_col_names == expected_names))
  
  # Test metric_data, fpkm slots
  example_raw = raw_data(res)[[1]][[1]]
  checkTrue(nrow(metric_data(res))==nrow(example_raw))
  checkTrue(all(metric_data(res)$id==example_raw[[1]]))
  checkTrue(nrow(metric_data(res)) > 0)
  checkTrue(ncol(metric_data(res))==(length(res)+1))
  
  print(res)
  
  # These were functional when written using version 0.99.1 of ENCODExplorerData
  # (2019-04-12 build)
  res = queryGeneExpression("GM12878", simplify=FALSE)
  checkTrue(length(res) == 3)
  checkTrue(length(files(res))==6)
  checkTrue(nrow(metric_data(res))==61471)
  
  res = queryGeneExpression("GM12878", simplify=TRUE)
  checkTrue(ncol(metadata(res))==1)
}
