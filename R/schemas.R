#' Extract the schemas from ENCODE's github
#'
#' The JSONs are fetched from:
#'   https://github.com/ENCODE-DCC/encoded/tree/master/src/encoded/schemas
#'
#' The data is extracted using the github api:
#'   https://developer.github.com/guides/getting-started/
#'
#' The data is then converted in \code{data.frame} using the \code{jsonlite}
#' package.
#'
#' @return a \code{list} of JSON converted in \code{data.frame}.
#'
#' @examples
#'   schema <- get_schemas()
#'
#'
get_schemas <- function() {
  # 1. Extract the description of the schemas
  encode_api_url <- "https://api.github.com/repos"
  encoded_repo <- "encode-dcc/encoded"
  schema_path <- "src/encoded/schemas"
  url <- paste(encode_api_url, encoded_repo, "contents", schema_path, sep = "/")
  schema_description <- jsonlite::fromJSON(url)

  # 2. Fetch all the JSON files
  raw_git_url <- "https://raw.githubusercontent.com"
  
  base_url <- paste(raw_git_url, encoded_repo, "master", schema_path, sep = "/")
  urls <- paste(base_url, schema_description$name, sep = "/")
  # We need to suppress warnings:
  #   Unexpected Content-Type: text/plain; charset=utf-8
  schema_json <- suppressWarnings(lapply(urls, jsonlite::fromJSON))
  names(schema_json) <- get_encode_types()
  schema_json
}
