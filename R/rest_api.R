#' Extract a data.frame corresponding to a table in ENCODE database
#'
#' @return a \code{data.frame} corresponding to the table asked. If no match is
#'   found, returns an empty \code{data.frame}
#'
#' @examples
#' lab <- ENCODEdb:::extract_table("lab")
extract_table <- function(type) {
  url <- "https://www.encodeproject.org/search/?type="
  url <- paste0(url, type, "&limit=all")
  res <- jsonlite::fromJSON(url)
  if (res[["notification"]] != "Success") {
    data.frame()
  } else {
    res[["@graph"]]
  }
}
