
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
  checkEquals(nrow(res), limit,  
              msg = paste0("res should be a ",limit," rows data.frame"))
}

test.ret_content <- function() {
  checkTrue(expr = !is.null(res$accession), 
            msg = "res should be get an accession entry")
}

test.ret_warning <- function() {
  searchTerm = "asdf"
  obs = tryCatch(searchEncode(searchTerm, limit),error=function(e) e, warning=conditionMessage)
  exp = "No result found"
  msg = "The function shouldn't return any results for the search AABBCC"
  
  checkIdentical(obs, exp, msg)
}


