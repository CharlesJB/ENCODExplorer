if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

limit = 4
searchTerm = "MCF7"
res = searchEncode(searchTerm, limit)

test.ret_type <- function() {
  checkTrue(expr = is.data.frame(res), msg = "res should be a data.frame")
}

test.limit <- function() {
  checkEquals(nrow(res), limit)
}

test.ret_content <- function() {
  checkTrue(expr = !is.null(res$accession), 
            msg = "res should be get an accession entry")
}
