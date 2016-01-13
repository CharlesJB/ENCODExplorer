#' Extract a data.frame corresponding to a table in ENCODE database
#'
#' @param type The type of table to extract from ENCODE rest api.
#'
#' @return a \code{data.frame} corresponding to the table asked. If no match is
#'   found, returns an empty \code{data.frame}
#'   
#' @import jsonlite
#' @import RCurl
extract_table <- function(type) {
  filters = "&limit=all"
  filters = paste0(filters, "&frame=object&format=json")
  
  url <- "https://www.encodeproject.org/search/?type="
  url <- paste0(url, type, filters)
  
  if (RCurl::url.exists(url)) {
	  res <- jsonlite::fromJSON(url)
	  if (res[["notification"]] != "Success") {
		data.frame()
	  } else {
		res[["@graph"]]
	  }
   } else {
	 data.frame()
   }
}

#' Clean a data.frame that was produced by extract_table
#'
#' \code{data.frame}s produced when converting JSON to \code{data.frame} with
#' the \code{fromJSON} function will sometime have columns that are lists
#' and/or columns that are \code{data.frames}.
#'
#' This function will either remove columns that are not relevant and convert
#' columns to a vector.
#'
#' @param table The table produced by the \code{extract_table} function.
#'
#' @return a \code{data.frame} corresponding to the cleaned version of the
#' input \code{data.frame}.
#'
#'
clean_table <- function(table) {
  
  clean_column <- function(column_name) {
    column <- table[[column_name]]
    # Case: data.frame
    if (is.data.frame(column)) {
      if (ncol(column) == 1 & nrow(column) == nrow(table)) {
        column <- column[,1]
      } else {
        column <- NULL
      }
      
      # Case: list
    } else if (is.list(column)) {
      # List of empty list
      if (all(sapply(column, length) == 0)) {
        column <- NULL
      } else if (column_name == "@type") {
        column <- NULL
      } else if (column_name == "related_files") {
          column <- sapply(column, function(x) {
            if (class(x) == "character" & length(x) > 0) {
              paste(x, collapse = ";")
            } else {
              NA
            }
          })
        # List of character vector
      } else if (all(sapply(column, class) == "character")) {
        if (all(sapply(column, length) <= 1)) {
          column <- sapply(column, function(x) {
            if (length(x) > 0) {
              x[[1]]
            } else {
              NA
            }
          })
        } else {
          column <- sapply(column, function(x) {
            if (length(x) > 0) {
              paste(x, collapse = ";")
            }
            else {
              NA
            }
          })
          
        }
        # List of data.frames
      } else if (all(sapply(column, class) == "data.frame")) {
        if (all(sapply(column, nrow) <= 1) &
              all(sapply(column, ncol) <= 1)) {
          column <- sapply(column, function(x) {
            if (length(x) > 0) {
              x[[1,1]]
            } else {
              NA
            }
          })
        } else {
          column <- NULL
        }
      }
      # List of something else
      else {
        column <- NULL
      }
    }
    column
  }
  table_names <- gsub("@", "", colnames(table))
  table <- lapply(colnames(table), clean_column)
  names(table) <- table_names
  table[sapply(table, is.null)] <- NULL
  as.data.frame(table)
}

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
#' @param limit the maximum number of return entries, default 10. \code{limit = all}
#' will return all the result. It can generate large results set.
#'
#' @return a \code{data.frame} corresponding Every object that matches the 
#' search term
#'
#' @examples
#'  searchEncode("ChIP-Seq+H3K4me1")
#' @import jsonlite
#' @export
searchEncode <- function(searchTerm = NULL, limit = 10) {
  searchTerm = gsub(x = searchTerm, pattern = " ",replacement = "+")
  
  filters = paste0("searchTerm=",searchTerm, "&format=json&limit=", limit)
  url <- "https://www.encodeproject.org/search/?"
  url <- paste0(url, filters)

  res <- jsonlite::fromJSON(url)
  if (res[["notification"]] != "Success") {
    warning("No result found", call. = TRUE)
    r = data.frame()
  } else {
    r = res[["@graph"]]
  }
  
  search_results = clean_table(r)
  print(paste0("results : ",length(unique(search_results$accession)), " entries"))
  search_results
}


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
#' @import jsonlite
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
