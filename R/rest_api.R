#' Extract a data.frame corresponding to a table in ENCODE database
#'
#' @param type The type of table to extract from ENCODE rest api.
#'
#' @return a \code{data.frame} corresponding to the table asked. If no match is
#'         found, returns an empty \code{data.frame}
#'         
#' @import jsonlite
#' @import RCurl
extract_table <- function(type) {
  filters = "&limit=all"
  filters = paste0(filters, "&frame=object&format=json")
  
  url <- "https://www.encodeproject.org/search/?type="
  url <- paste0(url, type, filters)
  results <- data.frame()
  
  if (RCurl::url.exists(url)) {
    res <- jsonlite::fromJSON(url)
    if (res[["notification"]] == "Success") {
      results <- res[["@graph"]]
    }
  } else {
    
    #temp <- strsplit(type, split = '')[[1]]
    #utype <- paste(c(toupper(temp[1]), temp[-1]), collapse = '')
    temp <- strsplit(type, split="_")[[1]]
    utype <- sapply(temp,function(x){paste0(toupper(substr(x,1,1)),
                                           substr(x,2,nchar(x)))})
    utype <- paste(utype, collapse='')
    url <- "https://www.encodeproject.org/search/?type="
    url <- paste0(url, utype, filters)
    
    if (RCurl::url.exists(url)) {
      res <- jsonlite::fromJSON(url)
      if (res[["notification"]] == "Success") {
        results <- res[["@graph"]]
      }
    }
  }
  
  return(results)
}

#' Clean a single column of the data.frame
#'
#' The input column can either be a data.frame, a vector of character, a vector
#' of numeric or a list of one the previous type.
#'
#' This function will either remove columns that are not relevant and convert
#' columns to a vector or data.frame.
#'
#' @param column_name The name of the column for the table that is been process.
#' @param table The table produced by the \code{extract_table} function.
#'
#' @return a \code{data.frame} corresponding to the cleaned version of the
#' input \code{data.frame}.
#' 
#' @importFrom tidyr spread
#' 
clean_column <- function(column_name, table) {
  
    stopifnot(is.character(column_name))
    stopifnot(column_name %in% colnames(table))
    stopifnot(length(column_name) == 1)
    stopifnot(is.data.frame(table))
    stopifnot(nrow(table) >= 1)
    
    
    column <- table[[column_name]]

    clean_empty_list <- function(x){
      if(is.list(x) & length(x) == 0){
        NULL
      }else{
        x
      }
    }

    # Case: data.frame
    if (is.data.frame(column)) {
        if (nrow(column) == nrow(table)) {
            for (i in 1:ncol(column)){
              column[[i]]<-lapply(column[[i]], unlist)
              column[[i]] <- sapply(column[[i]], function(x) {
                if (length(x) > 0) {
                  paste(x, collapse="; ")
                } else {
                  NA
                }
              })
            }
        } else {
            column <- NULL
        }
    
    # Case: character
    } else if (is.character(column)) {
        column
    # Case: numeric
    } else if (is.numeric(column)) {
        if (length(column) == nrow(table)) {
            column
        } else {
            column <- NULL
        }
    #Case: logical
    }else if (is.logical(column)) {
      if (length(column) == nrow(table)) {
        column
      } else {
        column <- NULL
      }
    #Case: integer
    }else if (is.integer(column)){
      if (length(column) == nrow(table)){
        column
      }else{
        column <- NULL
      }
      
    } else if (is.list(column)) {
        #Removing empty list in the column
        column <- lapply(column,clean_empty_list)
        
        # List of empty list
        if (all(sapply(column, length) == 0)) {
            column <- NULL
      
        # List of numeric
        } else if (all(sapply(column, class) == "numeric" | 
                  sapply(column, is.null))) {
            if (length(column) == nrow(table)) {
              column <- sapply(column, function(x) {
                if (length(x) > 0) {
                  paste(x, collapse="; ")
                } else {
                  NA
                }
              })
            } else {
                column <- NULL
            }
        # List of integer
        } else if (all(sapply(column, class) == "integer" | 
                       sapply(column, is.null))) {
          if (length(column) == nrow(table)) {
            column <- sapply(column, function(x) {
              if (length(x) > 0) {
                paste(x, collapse="; ")
              } else {
                NA
              }
            })
          } else {
            column <- NULL
          }
        
        # List of logical
        } else if (all(sapply(column, class) == "logical" | 
                      sapply(column, is.null))) {
            if (length(column) == nrow(table)){
                column <- sapply(column, function(x) {
                    if (length(x) > 0) {
                        paste(x, collapse="; ")
                    } else {
                         NA
                    }
                })
            }else{
                column <- NULL
            }
        
        # List of character vector
        } else if (all(sapply(column, class) == "character" |
                   sapply(column, is.null))) {
            
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
                        paste(x, collapse="; ")
                    } else {
                        NA
                    }
                })
        
            }
        # List of data.frames
        } else if (all(sapply(column, class) == "data.frame" |
                   sapply(column, is.null))) {
            res <- vector("list", length(column))
            for (i in 1:length(column)) {
                if (is.null(column[[i]])) {
                    res[[i]] <- NA
                } else if (nrow(column[[i]]) >= 1) {
                    if (nrow(column[[i]]) == 1) {
                        res[[i]] <- unlist(column[[i]])
                        list_name <- names(unlist(column[[i]]))
                    } else {
                        res[[i]] <- unlist(column[[i]][1,])
                        list_name <- names(unlist(column[[i]][1,]))
                        if (ncol(column[[i]]) == 1) {
                            list_name <- names(unlist(column[[i]][1]))
                            list_name <- unique(gsub("\\d", "", list_name))
                        }
                        
                    }
                    
                    list_name <- (gsub("\\d", "", list_name))
                    list_name <- (gsub("@", "", list_name))
                    list_name_unique <- unique(gsub("\\d", "", list_name))
                    
                    if (!(length(list_name) == length(list_name_unique))) {
                        splited <- split(res[[i]], list_name)
                        res[[i]] <- sapply(splited, paste0, collapse=";")
                        names(res[[i]]) <- list_name_unique
                    } else{
                        names(res[[i]]) <- list_name 
                    }
                } else {
                    res[[i]] <- NA
                }
            }
        
            column_clean <- res
            list_data <- vector("list",length(column))
            list_data <- lapply(seq_along(column_clean),function(x) {
                a=column_clean[[x]]
                if (all(is.na(a))) {
                    df <- data.frame(sample=NULL, col_name=NULL, value=NULL)
                } else {
                    df <- data.frame(sample=x, col_name=names(a), value=a,
                           stringsAsFactors=FALSE)
                    row.names(df) <- NULL
                }
                df
            })
      
            df_clean <- do.call("rbind", list_data)
            df_clean$sample <- factor(df_clean$sample,
                                      levels=seq_along(column_clean))
            df_clean <- spread(df_clean, col_name, value, drop=FALSE)
            df_clean$sample <- NULL
            column <- df_clean
        }
    
    # List of something else
    } else {
      column <- NULL
    }

  type <- c("character", "data.frame", "logical",
            "numeric", "integer", "NULL")
  stopifnot(class(column) %in% type)
  if(class(column) == "data.frame"){
      stopifnot(nrow(column) == nrow(table))
  }else if((class(column) %in% type) & !(class(column) == "NULL")){
      stopifnot(length(column) == nrow(table))
  }

  column
}

#' Clean a data.frame that was produced by extract_table
#'
#' \code{data.frame}s produced when converting JSON to \code{data.frame} with
#' the \code{fromJSON} function will sometime have columns that are lists
#' and/or columns that are \code{data.frames}.
#'
#' This function will either remove columns that are not relevant and convert
#' columns to a vector or data.frame.
#'
#' @param table The table produced by the \code{extract_table} function.
#'
#' @return a \code{data.frame} corresponding to the cleaned version of the
#' input \code{data.frame}.
#'
#' 

clean_table <- function(table) {
  
    class_vector <- as.vector(sapply(table, class))
    table <- table[,class_vector %in% c("character", "list", "data.frame",
                                        "logical", "numeric", "integer")]
    table_names <- gsub("@", "", colnames(table))
    table <- lapply(colnames(table), clean_column, table)
    names(table) <- table_names
    table[sapply(table, is.null)] <- NULL
    result <- as.data.frame(table)
    
    final_result <- lapply(X = result, FUN = function(x) if(class(x) == "factor") {as.character(x)} else {x})
    final_result
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
#' @import jsonlite
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
  }
  
  search_results = clean_table(r)
  search_results <- search_results[,order(colnames(search_results))]
  if(!quiet) cat(paste0("results : ",length(unique(search_results$accession)),
                        " entries\n"))
  search_results
}


#' Extract the schemas from ENCODE's github
#'
#' The JSONs are fetched from:
#'        https://github.com/ENCODE-DCC/encoded/tree/master/src/encoded/schemas
#'
#' The data is extracted using the github api:
#'         https://developer.github.com/guides/getting-started/
#'
#' The data is then downloaded using the \code{jsonlite} package.
#'
#' @return a \code{list} of schemas.
#' 
#' @import jsonlite
get_schemas <- function() {
  # 1. Extract the description of the schemas
  types <- get_encode_types()
  schema_names <- paste0(types, ".json")
  names(schema_names) <- types
  
  # 2. Fetch all the JSON files
  raw_git_url <- "https://raw.githubusercontent.com"
  encoded_repo <- "encode-dcc/encoded"
  schema_path <- "src/encoded/schemas"
  
  base_url <- paste(raw_git_url, encoded_repo, "master", schema_path, 
                    sep = "/")
  urls <- paste(base_url, schema_names, sep = "/")
  # We need to suppress warnings:
  #         Unexpected Content-Type: text/plain; charset=utf-8
  schema_json <- suppressWarnings(lapply(urls, jsonlite::fromJSON))
  schema_json
}

#' A list of known tables from ENCODE database.
#'
#' The type (table) names are extracted from the schema list from ENCODE-DCC
#' github repository:
#'        https://github.com/ENCODE-DCC/encoded/tree/master/src/encoded/schemas
#'
#' The data is extracted using the github api:
#'         https://developer.github.com/guides/getting-started/
#'
#' @return a vector of \code{character} with the names of the known tables in
#'         the ENCODE database.
#'
#' @import tools
get_encode_types <- function() {
  encode_api_url <- "https://api.github.com/repos"
  encoded_repo <- "encode-dcc/encoded"
  schemas <- "src/encoded/schemas"
  url <- paste(encode_api_url, encoded_repo, "contents", schemas, sep = "/")
  schema_names <- jsonlite::fromJSON(url)$name
  schema_names <- schema_names[grepl(".json$", schema_names)]
  tools::file_path_sans_ext(schema_names)
}
