#' Simulate a query on ENCODE website and return the result as a 
#' \code{data.frame}
#'
#' \code{data.frame}s produced when converting JSON to \code{data.frame} with
#' the \code{fromJSON} function will sometime have columns that are lists
#' and/or columns that are \code{data.frames}.
#'
#' This function simulates a basic query on ENCODE website 
#'
#' @param searchTerm a search term
#' @param limit the maximum number of return entries, default 10. 
#' @param quiet logical value enables to switch off the result summary 
#' information when setting at TRUE.
#' will return all the result. It can generate large results set.
#'
#' @return a \code{data.frame} corresponding Every object that matches the 
#' search term
#'
#' @examples
#'        searchEncode("ChIP-Seq+H3K4me1")
#' @importFrom ENCODExplorerData clean_table
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite flatten
#' @export
#' 
#' 
searchEncode <- function(searchTerm = NULL, limit = 10, quiet = FALSE) {
  searchTerm = gsub(x = searchTerm, pattern = " ",replacement = "+")
  r = data.frame()
  filters = paste0("searchTerm=",searchTerm, "&format=json&limit=", limit)
  url <- "https://www.encodeproject.org/search/?"
  url <- paste0(url, filters)
  
  if (RCurl::url.exists(url)) {
    res <- jsonlite::fromJSON(url)
    if (res[["notification"]] == "Success") {
      r <- res[["@graph"]]
    } else {
      warning("No result found", call. = TRUE)
    }
  } else {
    stop("Could not connect to www.encodeproject.org")
  }
  search_results = suppressWarnings(ENCODExplorerData::clean_table(jsonlite::flatten(r)))
  if(!quiet) {cat(paste0("results : ",length(unique(search_results$id)),"\n"))}
  
  search_results
}
