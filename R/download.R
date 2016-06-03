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
#' @param     df \code{data.frame} containing ENCODE files
#' @param resultSet the results set.
#' @param resultOrigin name of the function used to generate the result set
#' (\code{searchEncode} or \code{queryEncode} or \code{fuzzySearch})
#' @param format file format, default = all
#' @param dir the name of the directory where the downloaded file will be saved.
#' Default = current directory
#' @param force Download file is it already exists and md5sums is valid?
#'                                   Default: TRUE.
#'
#' @return The downloaded file names, if download worked correctly.
#' @examples
#'      resultSet <- queryEncode(biosample = "A549", file_format = "bam")
#'      \dontrun{
#'      downloadEncode(resultSet = resultSet, dir = ".")
#'      }
#' @import tools
#' @import downloader
#' 
#' @export
downloadEncode <- function(df = NULL, resultSet = NULL , resultOrigin = NULL,
                           format = "all", dir = ".", force = TRUE) {
  
  if(is.null(df)) {
    load(file=system.file("../data/encode_df.rda", package="ENCODExplorer"))
  } else {
    encode_df = df
  }
  
  if(is.null(resultSet) || is.null(resultOrigin)) {
    warning_msg <- "You have to provide both results set and its origin to use"
    warning_msg <- paste0(warning_msg, " the downloadEncode function")
    warning(warning_msg, call. = FALSE)
    NULL
  } else {
    if(resultOrigin %in% c("searchEncode", "queryEncode", "fuzzySearch")) {
      encode_root = "https://www.encodeproject.org"
      if(file.access(dir, mode = 2) == 0) {
        filesId = getFileId(encode_df, resultSet = resultSet, 
                            resultOrigin = resultOrigin, format = format)
        
        temp = subset(encode_df, encode_df$file_accession %in% filesId)
        hrefs = c(as.character(temp$href))
        md5sums = c(as.character(temp$md5sum))
        
        downloaded <- character()
        for (i in seq_along(hrefs)) {
          
          fileName = strsplit(x = hrefs[i], 
                              split = "@@download/",fixed = TRUE)[[1]][2]
          fileName <- paste(dir, fileName, sep = "/")
          md5sum_file = tools::md5sum(paste0(fileName))
          if (force == TRUE | !(file.exists(fileName)) |
              (file.exists(fileName) & md5sum_file != md5sums[i])) {
            download(url = paste0(encode_root,hrefs[i]), destfile = fileName)
            md5sum_file = tools::md5sum(paste0(fileName))
          }
          if(md5sum_file != md5sums[i]) {
            warning(paste0("Error while downloading the file : ", fileName), 
                    call. = FALSE)
            NULL
          }
          else
          {
            print(paste0("Success downloading the file : ", fileName))
            downloaded <- c(downloaded, fileName)
          }
        }
        downloaded
      }
      else
      {
        warning(paste0("Can't write in ", dir), call. = FALSE)
        NULL
      }
    } else {
      # origin farfelue 
      warning_msg <- "You have to provide a valid results set origin to use the"
      warning_msg <- paste0(warning_msg, " downloadEncode function : ")
      warning_msg <- paste0(warning_msg, "searchEncode or queryEncode")
      warning(warning_msg, call. = FALSE)
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
      warning("Unexpected format for a result set coming from our searchEncode
                                   function", call. = FALSE)
      NULL
    }
    
  }
  else if (resultOrigin %in% c("queryEncode","fuzzySearch)")){
      d = resultSet
  }else{
      warning("Unexpected format for a result set coming from our queryEncode 
                                   function", call. = FALSE)
      NULL
  }
  
  
  if (! is.null(d)) {
    r = c()
    formats = unique(c(as.character(encode_df$file_format)))
    if(format != "all") {
      if(!(format %in% formats)) {
        warning("Unknown file format", call. = FALSE)
        NULL
      }
      else
      {
        avail_format =     unique(c(as.character(d$file_format)))
        if(!(format %in% avail_format)) {
          warning("This file format is not available in your dataset", 
                  call. = FALSE)
          NULL
        }
        else
        {
          temp = subset(d, dt$file_format == format)
          r = c(as.character(temp$file_accession))
        }
      }
    }
    else {
      r = c(as.character(d$file_accession))
    }
    
    r
  }
}
# to use with search results
getFileDetails <- function(encode_df,resultSet) {
  acc = resultSet$accession
  subset(encode_df,encode_df$accession %in% acc)
}


