if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

load(file = "inst/extdata/test_rest_search_file.rda") #search_file

res = getFileDetails(resultSet = search_file)
checkTrue(is.list(res), msg = "this function should return a list")
checkTrue(length(res) == 2, msg = "this function should return a list with 2 elements")
