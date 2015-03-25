if(FALSE) {
  library( "RUnit" )
  library( "ENCODEdb" )
}


md5sum_test_file = "b58b4847e8d71d95471c4c017b4a5dc7"

load(file = system.file("extdata/test_rest_query_file.rda", package = "ENCODEdb")) #query_file

# check when arguments are missing
checkTrue(is.null(download()), msg = "the function should return a NULL value if the result set and its origin are missing")
checkTrue(is.null(download(resultSet = query_file)), msg = "the function should return a NULL value if the result set origin is missing")
checkTrue(is.null(download(resultOrigin = "search")), msg = "the function should return a NULL value if the result set is missing")

# check when arguments are incorrect
checkTrue(is.null(download(resultSet = query_file, resultOrigin = "web")), 
          msg = "the function should return a NULL value if the result set origin is different from search or query")
checkTrue(is.null(download(resultSet = query_file, resultOrigin = "query", 
                           dir = "/aabbbccc")), msg = "the function should return a NULL value if the result set origin is different from search or query")

# check download file
download(resultSet = query_file, resultOrigin = "query", format = "bed_broadPeak", dir = system.file("extdata/", package = "ENCODEdb"))  
checkTrue(file.exists(system.file("extdata/ENCFF001VCK.broadPeak.gz",  package = "ENCODEdb")), 
          msg = "the file ENCFF001VCK.broadPeak.gz is missing")

checkEquals(as.character(tools::md5sum(system.file("extdata/ENCFF001VCK.broadPeak.gz",  package = "ENCODEdb"))),
            md5sum_test_file)

file.remove(system.file("extdata/ENCFF001VCK.broadPeak.gz",  package = "ENCODEdb"))

