#' Download files from the Internet.
#'
#' After processing to a basic search with the \code{searchEncode} function or a
#' precise search thanks to the \code{queryEncode} function, you can proceed to
#' the downloading of all the corresponding files.
#'
#' This function can be used to download a set of files by providing the results
#' set, its origin (searchEncode or queryEncode), the file format and finally
#' the destination directory.
#' 
#' @param  df \code{list} of two \code{data.frame} containing ENCODE experiment
#' and dataset metadata.
#' @param resultSet the results set.
#' @param resultOrigin name of the function used to generate the result set
#' (\code{searchEncode} or \code{queryEncode})
#' @param format file format, default = all
#' @param dir the name of the directory where the downloaded file will be saved.
#' Default = current directory
#' @param force Download file is it already exists? Default: TRUE.
#'
#' @return void
#' @examples
#'   resultSet <- queryEncode(biosample = "A549", file_format = "bam")
#'   \dontrun{
#'   downloadEncode(resultSet = resultSet, dir = ".")
#'   }
#' @import tools
#' 
#' @export
downloadEncode <- function(df = NULL, resultSet = NULL , resultOrigin = NULL,
                     format = "all", dir = ".", force = TRUE) {
  
  if(is.null(df)) {data(encode_df, envir = environment())} else {encode_df = df}
  
  if(is.null(resultSet) || is.null(resultOrigin)) {
    warning("You have to provide both results set and its origin to use the downloadEncode function", call. = FALSE)
    NULL
  }
  else
  {
    if(resultOrigin %in% c("searchEncode", "queryEncode"))
    {
      encode_root = "https://www.encodeproject.org"
      if(file.access(dir, mode = 2) == 0) {
        filesId = getFileId(encode_df, resultSet = resultSet, 
                            resultOrigin = resultOrigin, format = format)
        
        temp = subset(encode_df$experiment, encode_df$experiment$file_accession %in% filesId)
        temp2 = subset(encode_df$dataset, encode_df$dataset$file_accession %in% filesId)
        hrefs = c(as.character(temp$href), as.character(temp2$href))
        md5sums = c(as.character(temp$md5sum), as.character(temp2$md5sum))
        
        current_dir = getwd()
        
        for (i in seq_along(hrefs)) {
          
          setwd(dir)
          fileName = strsplit(x = hrefs[i], split = "@@download/",fixed = TRUE)[[1]][2]
	  if (force == TRUE | !(file.exists(fileName))) {
            download.file(url = paste0(encode_root,hrefs[i]), quiet = TRUE,
                                destfile = fileName, method =  "curl", extra = "-L" )
	  }
          md5sum_file = tools::md5sum(paste0(fileName))
          if(md5sum_file != md5sums[i]) {
            warning(paste0("Error while downloading the file : ", fileName), call. = FALSE)
            NULL
          }
          else
          {
            print(paste0("Success downloading the file : ", fileName))
          }
        }
        
        setwd(current_dir)
      }
      else
      {
        warning(paste0("Can't write in ", dir), call. = FALSE)
        NULL
      }
    }
    # origin farfelue 
    else
    {
      warning("You have to provide a valid results set origin to use the downloadEncode function : searchEncode or queryEncode", call. = FALSE)
      NULL
    }
  }
}

getFileId <- function(encode_df, resultSet, resultOrigin, format = "all") {
  d = NULL
  
  if(resultOrigin == "searchEncode") {
    if(class(resultSet) == "data.frame")
    {
      d = getFileDetails(encode_df,resultSet)
    }
    else
    {
      warning("Unexpected format for a result set coming from our searchEncode function",
              call. = FALSE)
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
      warning("Unexpected format for a result set coming from our queryEncode function",
              call. = FALSE)
      NULL
    }
  }
  
  if (! is.null(d)) {
    r = c()
    formats = unique(c(as.character(encode_df$experiment$file_format),
                       as.character(encode_df$dataset$file_format)))
    if(format != "all") {
      if(!(format %in% formats)) {
        warning("Unknown file format", call. = FALSE)
        NULL
      }
      else
      {
        avail_format =  unique(c(as.character(d$experiment$file_format),
                                 as.character(d$dataset$file_format)))
        if(!(format %in% avail_format)) {
          warning("This file format is not available in your dataset", call. = FALSE)
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
getFileDetails <- function(encode_df,resultSet) {
  details = list(
    experiment = getExperimentDetails(encode_df,resultSet),
    dataset = getDatasetDetails(encode_df,resultSet)
  )
  details
}

# to use with search results
getExperimentDetails <- function(encode_df,resultSet) {
  exp = resultSet$accession
  subset(encode_df$experiment,encode_df$experiment$accession %in% exp)
}

# to use with search results
getDatasetDetails <- function(encode_df,resultSet) {
  ds = resultSet$accession
  subset(encode_df$dataset,encode_df$dataset$accession %in% ds)
}

