if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

load(file = "inst/extdata/test_rest_query_file.rda") #query_file
load(file = "inst/extdata/test_rest_search_file.rda") #search_file

# check arguments
checkTrue(is.null(getFileId(resultSet = query_file, resultOrigin = "searchEncode")), 
          msg = "the function should return a NULL value if the result set and its origin seems incompatible")
checkTrue(is.null(getFileId(resultSet = search_file, resultOrigin = "queryEncode")),
          msg = "the function should return a NULL value if the result set and its origin seems incompatible")
checkTrue(is.null(getFileId(resultSet = search_file, resultOrigin = "searchEncode", format = "web")),
          msg = "the function should return a NULL value if the format is unknown")
checkTrue(is.null(getFileId(resultSet = search_file, resultOrigin = "searchEncode", format = "fastq")),
          msg = "the function should return a NULL value if the format is not available in the subset") 

# check results
fileIds = getFileId(resultSet = search_file, resultOrigin = "searchEncode", format = "bed")  
checkEquals(fileIds, "ENCFF001VCK")

fileIds = getFileId(resultSet = query_file, resultOrigin = "queryEncode", format = "bed")  
checkEquals(fileIds, "ENCFF001VCK")
