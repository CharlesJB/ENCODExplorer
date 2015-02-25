#' Create the data.frames for the table in ENCODE
#'
#' @return a \code{list} with selected tables from ENCODE.
#'
#' The following table are downloaded"
#' \describe{
#'   \item{biosample}{}
#'   \item{experiment}{}
#'   \item{lab}{}
#'   \item{dataset}{}
#'   \item{files}{}
#'   \item{target}{}
#' }
#'
#' @examples
#' lab <- ENCODEdb:::extract_table("lab")
prepare_data <- function() {
  types <- c("biosample",
             "experiment",
             "lab",
             "dataset",
             "file",
             "target")

  res <- lapply(types, ENCODEdb:::extract_table)
  names(res) <- types
  res
}
