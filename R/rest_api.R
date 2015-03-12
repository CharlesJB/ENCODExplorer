#' Extract a data.frame corresponding to a table in ENCODE database
#'
#' @param type The type of table to extract from ENCODE rest api.
#' Examples: "biosample", "file", "experiment", etc...
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
#' @examples
#' lab <- ENCODEdb:::extract_table("lab")
#' lab <- ENCODEdb:::clean_table(lab)
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
          column <- NULL
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
      # List of something else
      } else {
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
