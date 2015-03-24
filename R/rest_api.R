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
#' @export
search <- function(searchTerm = NULL, limit = 10) {
  searchTerm = gsub(x = searchTerm, pattern = " ",replacement = "+")
  
  filters = paste0("searchTerm=",searchTerm, "&format=json&limit=", limit)
  url <- "https://www.encodeproject.org/search/?"
  url <- paste0(url, filters)

  res <- jsonlite::fromJSON(url)
  if (res[["notification"]] != "Success") {
    warning("No result found", call. = F)
    r = data.frame()
  } else {
    r = res[["@graph"]]
  }
  
  search_results = clean_table(r)
  print(paste0("results : ",length(unique(search_results$accession)), " entries"))
  search_results
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
#' res = ENCODEdb::query(file_format = "GTF", biosample = "mcf-7", fixed = F)
#' @export
query <- function(accession = NULL, assay = NULL, biosample = NULL, 
                  dataset_access = NULL, file_accession = NULL, file_format = NULL, 
                  lab = NULL, organism = NULL, target = NULL, treatment = NULL,
                  fixed = TRUE) {
  if(!can_run()) stop("Please complete the data preparation before running the this function")
  
  if(is.null(accession) && is.null(assay) && is.null(biosample) && is.null(dataset_access) &&
       is.null(file_accession) && is.null(file_format) && is.null(lab) && is.null(organism) &&
       is.null(target) && is.null(treatment))
  {
    warning("Please provide at least one valid criteria", call. = F)
    NULL
    
  }
  else
  {
    s1 = matrices$experiment
    s2 = matrices$dataset
    
    ac = accession
    as = assay
    bs = biosample
    da = dataset_access
    fa = file_accession
    ff = file_format
    lb = lab
    og = organism
    tg = target
    tr = treatment
    
    if(fixed) {
      
      if(!is.null(ac)) {
        s1 <- subset(s1, s1$accession == ac)
        s2 <- subset(s2, s2$accession == ac)
      }
      
      if(!is.null(as)) {
        s1 <- subset(s1, s1$assay == as)
        s2 <- subset(s2, s2$assay == as)
      }
      
      if(!is.null(bs)) {
        s1 <- subset(s1, s1$biosample_name == bs)
        s2 <- subset(s2, s2$biosample_name == bs)
      }
      
      if(!is.null(da)) {
        s1 <- subset(s1, s1$dataset_accession == da)
        s2 <- subset(s2, s2$accession == da)
      }
      
      if(!is.null(fa)) {
        s1 <- subset(s1, s1$file_accession == fa)
        s2 <- subset(s2, s2$file_accession == fa)
      }
      
      if(!is.null(ff)) {
        s1 <- subset(s1, s1$file_format == ff)
        s2 <- subset(s2, s2$file_format == ff)
      }
      
      if(!is.null(lb)) {
        s1 <- subset(s1, s1$lab == lb)
        s2 <- subset(s2, s2$lab == lb)
      }
      
      if(!is.null(og)) {
        s1 <- subset(s1, s1$organism == og)
        s2 <- subset(s2, s2$organism == og)
      }
      
      if(!is.null(tg)) {
        s1 <- subset(s1, s1$target == tg)
        s2 <- subset(s2, s2$target == tg)
      }
      
      if(!is.null(tr)) {
        s1 <- subset(s1, s1$treatment == tr)
        s2 <- subset(s2, s1$treatment == tr)
      }
    }
    else
    {
      # retirer ignorer les espaces, les tirets et la casse
      # m cf 7 = MCf7 = mcf-7 = MCF-7 ... etc
      
      if(!is.null(ac)) {
        query.transfo = query_transform(ac)
        select.entries = grepl(x = s1$accession, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$accession, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(as)) {
        query.transfo = query_transform(as)
        select.entries = grepl(x = s1$assay, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$assay, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(bs)) {
        query.transfo = query_transform(bs)
        select.entries = grepl(x = s1$biosample_name, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$biosample_name, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(da)) {
        query.transfo = query_transform(da)
        select.entries = grepl(x = s1$dataset_accession, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$accession, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(fa)) {
        query.transfo = query_transform(fa)
        select.entries = grepl(x = s1$file_accession, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$file_accession, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(ff)) {
        query.transfo = query_transform(ff)
        select.entries = grepl(x = s1$file_format, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$file_format, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(lb)) {
        query.transfo = query_transform(lb)
        select.entries = grepl(x = s1$lab, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$lab, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(og)) {
        query.transfo = query_transform(og)
        select.entries = grepl(x = s1$organism, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$organism, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(tg)) {
        query.transfo = query_transform(tg)
        select.entries = grepl(x = s1$target, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$target, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(tr)) {
        query.transfo = query_transform(tr)
        select.entries = grepl(x = s1$treatment, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$treatment, pattern = query.transfo, 
                               ignore.case = T, perl = T)
        s2 = s2[select.entries,]
      }
      
    }
    
    
    
    if((nrow(s1) + nrow(s2)) == 0) {
      warning("No result found. You can try the <search> function or set the fixed option to FALSE", call. = F)
      NULL
    }
    else
    {
      query_results = list(experiment = s1, dataset = s2)
      print(paste0("experiment results : ",nrow(query_results$experiment)," files in ",length(unique(query_results$experiment$accession))," experiments ; dataset results : ",nrow(query_results$dataset), " files"))
      query_results
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
#' @param resultSet the results set
#' @param resultOrigin name of the function used to generate the result set 
#' (\code{search} or \code{query})
#' @param format file format, default = all
#' @param dir the name of the directory where the downloaded file will be saved.
#' Default = /tmp
#'
#'
#' @examples
#' res = ENCODEdb::search(searchTerm = "mcf7", limit = 2)
#' @export
download <- function(resultSet = NULL , resultOrigin = NULL, 
                         format = "all", dir = "/tmp") {
  if(!can_run()) stop("Please complete the data preparation before running the this function")
  
  if(is.null(resultSet) || is.null(resultOrigin)) {
    warning("You have to provide both results set and its origin to use the download function", call. = F)
    NULL
  }
  else
  {
    if(resultOrigin %in% c("search", "query"))
    {
      encode_root = "https://www.encodeproject.org"
      if(file.access(dir, mode = 2) == 0) {
        filesId = getFileId(resultSet = resultSet, 
                            resultOrigin = resultOrigin, format = format)
        
        temp = subset(matrices$experiment, matrices$experiment$file_accession %in% filesId)
        temp2 = subset(matrices$dataset, matrices$dataset$file_accession %in% filesId)
        urls = c(as.character(temp$href), as.character(temp2$href))
        md5sums = c(as.character(temp$md5sum), as.character(temp2$md5sum))
          
        for (i in seq_along(urls)) {
          url = urls[i]
          md5 = md5sums[i]
          
          fileName = strsplit(x = url, split = "@@download",fixed = T)[[1]][2]
          download.file(url = paste0(encode_root,url), quiet = T,
                              destfile = paste0(dir,fileName), method = "wget")
          md5sum_file = tools::md5sum(paste0(dir,fileName))
          if(md5sum_file != md5) {
            warning(paste0("Error while downloading the file : ", fileName), call. = F)
            NULL
          }
          else
          {
            print(paste0("Success downloading the file : ", fileName))
          }
        }
        
      }
      else
      {
        warning(paste0("Can't write in ", dir), call. = F)
        NULL
      }
    }
    # origin farfelue 
    else
    {
      warning("You have to provide a valid results set origin to use the download function : search or query", call. = F)
      NULL
    }
  }
}

getFileId <- function(resultSet, resultOrigin, format = "all") {
  
  d = NULL
  
  if(resultOrigin == "search") {
    if(class(resultSet) == "data.frame")
    {
      d = getFileDetails(resultSet)
    }
    else
    {
      warning("Unexpected format for a result set coming from our search function", 
              call. = F)
      NULL
    }
    
  }
  else
  {
    if(class(resultSet) == "list" && length(resultSet) == 2)
    {
      d = resultSet
    }
    else
    {
      warning("Unexpected format for a result set coming from our query function", 
              call. = F)
      NULL
    }
  }
  
  if (! is.null(d)) {
    r = c()
    formats = unique(c(as.character(matrices$experiment$file_format),
                       as.character(matrices$dataset$file_format)))
    if(format != "all") {
      if(!(format %in% formats)) {
        warning("Unknown file format", call. = F)
        NULL
      }
      else
      {
        avail_format =  unique(c(as.character(d$experiment$file_format),
                                 as.character(d$dataset$file_format)))
        if(!(format %in% avail_format)) {
          warning("This file format is not available in your dataset", call. = F)
          NULL
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
}
# to use with search results
getFileDetails <- function(resultSet) {
  details = list(
    experiment = getExperimentDetails(resultSet),
    dataset = getDatasetDetails(resultSet)
  )
  details
}

# to use with search results
getExperimentDetails <- function(resultSet) {
  exp = resultSet$accession
  subset(matrices$experiment,matrices$experiment$accession %in% exp)
}

# to use with search results
getDatasetDetails <- function(resultSet) {
  ds = resultSet$accession
  subset(matrices$dataset,matrices$dataset$accession %in% ds)
}

# check if the needed dataset is avalable
can_run <- function() {
  return(exists("matrices"))
}


