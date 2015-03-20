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
  filters = "&limit=all"
  filters = paste0(filters, "&frame=object")
  
  url <- "https://www.encodeproject.org/search/?type="
  url <- paste0(url, type, filters)
  
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
#' res = ENCODEdb::search(searchTerm = "mcf7", limit = 2)
search <- function(searchTerm, limit = "10") {
  filters = paste0("searchTerm=",searchTerm, "&limit=", limit)
  url <- "https://www.encodeproject.org/search/?"
  url <- paste0(url, filters)
  
  res <- jsonlite::fromJSON(url)
  if (res[["notification"]] != "Success") {
    warning("No result found", call. = F)
    r = data.frame()
  } else {
    r = res[["@graph"]]
  }
  
  r = clean_table(r)
  r
}

#' Produce a subset of data following predefined criteria
#'
#' After running the \code{prepare_ENCODEDb} function, this function will allow 
#' you to extract a subset of data encording to the following criteria : 
#' accession, assay name, biosample, dataset accession, file accession, 
#' file format, laboratory, donor organism, target and treatment.
#' 
#' By default, the query can be made on an exact match term. This behaviour can 
#' be modified by setting the \code{fixed} argument at \code{TRUE}
#' 
#' @param selection criteria.
#'
#' @return a \code{list} of two \code{data.frame}s containing data about ENCODE 
#' experiments and datasets
#'
#' @examples
#' matrices = ENCODEdb:export_ENCODEDB_matrix(database_filename)
#' encode_exp = ENCODEdb::matrices[[1]]
#' encode_ds = ENCODEdb::matrices[[2]]
#' res = ENCODEdb::query(fileFormat = "GTF", biosample = "mcf-7", fixed = F)
query <- function(acces = NULL, assayName = NULL, biosample = NULL, 
                  dataset_access = NULL, fileAccess = NULL, fileFormat = NULL, 
                  labo = NULL, organism = NULL, target = NULL, treatment = NULL,
                  fixed = TRUE) {
  
  s1 = encode_exp
  s2 = encode_ds
  
  if(fixed) {
    
    if(!is.null(acces)) {
      s1 <- subset(s1, s1$accession == acces)
      s2 <- subset(s2, s2$accession == acces)
    }
    
    if(!is.null(assayName)) {
      s1 <- subset(s1, s1$assay == assayName)
    }
    
    if(!is.null(biosample)) {
      s1 <- subset(s1, s1$biosample_name == biosample)
    }
    
    if(!is.null(dataset_access)) {
      s1 <- subset(s1, s1$dataset_accession == dataset_access)
      s2 <- subset(s2, s2$accession == dataset_access)
    }
    
    if(!is.null(fileAccess)) {
      s1 <- subset(s1, s1$file_accession == fileAccess)
      s2 <- subset(s2, s2$file_accession == fileAccess)
    }
    
    if(!is.null(fileFormat)) {
      s1 <- subset(s1, s1$file_format == fileFormat)
      s2 <- subset(s2, s2$file_format == fileFormat)
    }
    
    if(!is.null(labo)) {
      s1 <- subset(s1, s1$lab == labo)
      s2 <- subset(s2, s2$lab == labo)
    }
    
    if(!is.null(organism)) {
      s1 <- subset(s1, s1$organism == organism)
    }
    
    if(!is.null(target)) {
      s1 <- subset(s1, s1$target == target)
    }
    
    if(!is.null(treatment)) {
      s1 <- subset(s1, s1$treatment == treatment)
    }
  }
  else
  {
    # retirer ignorer les espaces, les tirets et la casse
    # m cf 7 = MCf7 = mcf-7 = MCF-7 ... etc
    
    if(!is.null(acces)) {
      query.transfo = query_transform(access)
      select.entries = grepl(x = s1$accession, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s1 = s1[select.entries,]
      
      select.entries = grepl(x = s2$accession, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s2 = s2[select.entries,]
    }
    
    if(!is.null(assayName)) {
      query.transfo = query_transform(assayName)
      select.entries = grepl(x = s1$assay, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s1 = s1[select.entries,]
    }
    
    if(!is.null(biosample)) {
      query.transfo = query_transform(biosample)
      select.entries = grepl(x = s1$biosample_name, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s1 = s1[select.entries,]
    }
    
    if(!is.null(dataset_access)) {
      query.transfo = query_transform(dataset_access)
      select.entries = grepl(x = s1$dataset_accession, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s1 = s1[select.entries,]
      
      select.entries = grepl(x = s2$accession, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s2 = s2[select.entries,]
    }
    
    if(!is.null(fileAccess)) {
      query.transfo = query_transform(fileAccess)
      select.entries = grepl(x = s1$file_accession, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s1 = s1[select.entries,]
      
      select.entries = grepl(x = s2$file_accession, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s2 = s2[select.entries,]
    }
    
    if(!is.null(fileFormat)) {
      query.transfo = query_transform(fileFormat)
      select.entries = grepl(x = s1$file_format, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s1 = s1[select.entries,]
      
      select.entries = grepl(x = s2$file_format, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s2 = s2[select.entries,]
    }
    
    if(!is.null(labo)) {
      query.transfo = query_transform(labo)
      select.entries = grepl(x = s1$lab, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s1 = s1[select.entries,]
      
      select.entries = grepl(x = s2$lab, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s2 = s2[select.entries,]
    }
    
    if(!is.null(organism)) {
      query.transfo = query_transform(organism)
      select.entries = grepl(x = s1$organism, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s1 = s1[select.entries,]
    }
    
    if(!is.null(target)) {
      query.transfo = query_transform(target)
      select.entries = grepl(x = s1$target, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s1 = s1[select.entries,]
    }
    
    if(!is.null(treatment)) {
      query.transfo = query_transform(treatment)
      select.entries = grepl(x = s1$treatment, pattern = query.transfo, 
                             ignore.case = T, perl = T)
      s1 = s1[select.entries,]
    }
    
  }
  
  
  
  if((nrow(s1) + nrow(s2)) == 0) {
    warning("No result found. You can try the <search>", call. = F)
    list()
  }
  else
  {
    if(nrow(s1) == nrow(encode_exp) || nrow(s2) == nrow(encode_ds)) {
      warning("Please provide at least one valid criteria", call. = F)
      list()
    }
    else
    {
      list(experiment = s1, dataset = s2)
    }
  }
  
  
}

query_transform <- function(my.term) {
  my.term = gsub(my.term, pattern = " ", replacement = "", fixed = T)
  my.term = gsub(my.term, pattern = "-", replacement = "", fixed = T)
  my.term = gsub(my.term, pattern = ",", replacement = "", fixed = T)
  my.term = strsplit(my.term, split = "")[[1]]
  my.term.4.grep = paste0("^",paste(my.term, collapse = "[ ,-]?"))
  
  my.term.4.grep
}


#' Download files from the Internet.
#'
#' After processing to a basic search with the \code{search} function or a 
#' precise search thanks to the \code{query} function, you can proceed to the
#' downloading of all the corresponding files.
#'
#' This function can be used to download a set of files by providing the results
#' set, its origin (search or query), the file format and finally the destination
#' directory.
#'
#' @param searchResult the results set
#' @param resultOrigin name of the function used to generate the result set 
#' (\code{search} or \code{query})
#' @param format file format, default = all
#' @param dir the name of the directory where the downloaded file will be saved.
#' Default = /tmp
#'
#'
#' @examples
#' res = ENCODEdb::search(searchTerm = "mcf7", limit = 2)
downloadFile <- function(searchResult = NULL , resultOrigin = NULL, 
                         format = "all", dir = "/tmp") {
  if(is.null(searchResult) || is.null(resultOrigin)) {
    warning("You have to provide both results set and its origin to use the 
            download function", call. = F)
  }
  else
  {
    if(resultOrigin %in% c("search", "query"))
    {
      encode_root = "https://www.encodeproject.org"
      if(file.access(dir, mode = 2) == 0) {
        filesId = getFileId(searchResult = searchResult, 
                            resultOrigin = resultOrigin, format = format)
        
        temp = subset(encode_exp, encode_exp$file_accession %in% filesId)
        temp2 = subset(encode_ds, encode_ds$file_accession %in% filesId)
        urls = c(as.character(temp$href), as.character(temp2$href))
        urls
        for (url in urls) {
          fileName = strsplit(x = url, split = "@@download",fixed = T)[[1]][2]
          ret = download.file(url = paste0(encode_root,url), quiet = T,
                              destfile = paste0(dir,fileName), method = "wget" )
        }
        
      }
      else
      {
        warning(paste0("Can't write in ", dir), call. = F)
      }
    }
    # origin farfelue 
    else
    {
      warning("You have to provide a valid results set origin to use the 
            download function : search or query", call. = F)
    }
  }
}

getFileId <- function(searchResult, resultOrigin, format = "all") {
  
  if(resultOrigin == "search") {
    if(class(searchResult) == "data.frame")
    {
      d = getFileDetails(searchResult)
    }
    else
    {
      warning("Unexpected format for a result set coming from our search 
              function", call. = F)
    }
    
  }
  else
  {
    if(class(searchResult) == "list" && length(searchResult) == 2)
    {
      d = searchResult
    }
    else
    {
      warning("Unexpected format for a result set coming from our query 
              function", call. = F)
    }
  }
  
  
  r = c()
  formats = c(levels(d$experiment$file_format),levels(d$dataset$file_format))
  if(format != "all") {
    if(!(format %in% formats)) {
      warning("Unknown file format", call. = F)
    }
    else
    {
      avail_format =  unique(c(as.character(d$experiment$file_format),
                               as.character(d$dataset$file_format)))
      if(!(format %in% avail_format)) {
        warning("This file format is not available in your dataset", call. = F)
      }
      else
      {
        temp = subset(d$experiment, d$experiment$file_format == format)
        temp2 = subset(d$dataset, d$dataset$file_format == format)
        r = c(as.character(temp$file_accession), 
              as.character(temp2$file_accession))
      }
    }
  }
  else {
    r = c(as.character(d$experiment$file_accession), 
          as.character(d$dataset$file_accession))
  }
  
  r
}

getFileDetails <- function(searchResult) {
  details = list(
    experiment = getExperimentDetails(searchResult),
    dataset = getDatasetDetails(searchResult)
  )
  details
}

getExperimentDetails <- function(searchResult) {
  exp = searchResult$accession
  subset(encode_exp,encode_exp$accession %in% exp)
}

getDatasetDetails <- function(searchResult) {
  ds = searchResult$accession
  subset(encode_ds,encode_ds$accession %in% ds)
}
