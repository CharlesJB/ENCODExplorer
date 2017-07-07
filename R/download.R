#' download_single_file Downloads a single file and checks if md5 checksums match.
#' @param file_url A \code{character} giving the URL of the file to be downloaded.
#' @param file_md5 A \code{character} giving the expected md5 checksum hash of the
#'   file to be downloaded.
#' @param dir The directory where the downloaded file should be saved. Default: "."
#' @param experiment_name An optional experiment name to be displayed with the 
#'   status reports.
#' @param force \code{boolean} indicating if existing files should be downloaded 
#'   again. Default : TRUE
#' 
#' @return A \code{character} with the name of the downloaded file.
#' 
#' @import tools
download_single_file <- function(file_url, file_md5, dir=".", experiment_name=NULL, force=TRUE) {
  # Determine the output filename.
  fileName = strsplit(x = file_url, split = "@@download/", fixed = TRUE)[[1]][2]
  fileName <- paste0(dir,"/", fileName, sep="")
  
  # Identify the target URL.
  href <- as.character(file_url)
  
  # Calculate the md5 of the file if it already exists.
  md5sum_file <- tools::md5sum(fileName)
  md5sum_encode <- as.character(file_md5)
  
  # If the file does not exist, md5 hashes do not match or force=TRUE, download the file.
  if (force == TRUE | !(file.exists(fileName)) |
      (file.exists(fileName) & md5sum_file != md5sum_encode)){
    encode_root = "https://www.encodeproject.org"
    download_ret = download.file(url=paste0(encode_root, href), quiet=TRUE,
                             destfile=fileName, method = "curl",
                             extra = "-L" )
    
    # Determine if the download was a success and calculate the md5 hash.
    if(download_ret != 0 || !file.exists(fileName)) {
      warning(paste0("Error while downloading ", fileName, " (", download_ret, ")"), 
            call. = FALSE)
    } else {
      md5sum_file = tools::md5sum(paste0(fileName))
    }
  }
  
  #Validating the download
  if(is.na(md5sum_file) || (md5sum_file != md5sum_encode)) {
    warning(paste0("No md5sum match for : ", fileName), 
            call. = FALSE)
    return(NULL)
  }else{
    if(!is.null(experiment_name)) {
      print(paste0("Success downloading experiment :", experiment_name,
            ",file :", fileName))
    } else {
      print(paste0("Success downloading file : ", fileName))
    }
    return(fileName)
  }
}

#' Downloads all files inside a data.table.
#'
#' @param input_dt The data.table to be looped over for determining which
#'   files should be downloaded.
#' @param dir The path of the directory where the downloaded files should 
#'   be saved.
#' @param force If TRUE, existing files are downloaded again.
#' @param show_experiment If TRUE, the name of the experiment is extracted
#'   from the data table and displayed in status messages.
#'
#' @return The name of the files which were downloaded.
download_dt_file <- function(input_dt, dir, force, show_experiment=FALSE) {
  # Track downloaded files.
  downloaded <- c()
  if(nrow(input_dt) > 0){
    for(i in 1:nrow(input_dt)) {
      if(show_experiment) {
        experiment_name = input_dt$accession[[i]]
      } else {
        experiment_name = NULL
      }
    
      dl.file = download_single_file(file_url=input_dt$href[[i]], 
                                     file_md5=input_dt$md5sum[[i]],
                                     dir=dir,
                                     force=force,
                                     experiment_name=experiment_name)
      downloaded <- c(downloaded, dl.file)
    }
  }
  
  return(downloaded)
}

#' downloadEncode is used to download a serie of files or datasets 
#' using their accession. 
#' @param file_acc A \code{character} of ENCODE file or 
#' experiment accessions. Can also be a data.table coming from any ENCODExplorer
#' search function.
#' @param df The reference \code{data.table} used to find the download. Files 
#' that are not available will be searched directly through the current
#' ENCODE database. 
#' @param format The specific file format to download.
#' Default : all
#' @param dir The directory to locate the downloaded files
#' @param force \code{boolean} to allow downloading a file even if it already exists
#' in the directory. 
#' Default : TRUE
#' 
#' @return A \code{character} with the downloaded files
#' 
#' @examples 
#'  fuzzy_result <- fuzzySearch("ENCSR396EAG", encode_df, filterVector = "accession")
#'  \dontrun{downloadEncode(fuzzy_result, format="tsv")}
#' 
#' @import data.table
#' @import tools
#' @importFrom dplyr filter
#' @importFrom dplyr setdiff
#' 
#' @export
downloadEncode <- function (file_acc = NULL, df = NULL, format ="all", dir= ".",
                             force = TRUE) {
  #Extract accession if the user pass a data.table as input
  if(is.data.frame(file_acc)){
    if (!is.null(file_acc$file_accession)){
      file_acc <- as.character(file_acc$file_accession)
    } else if (!is.null(file_acc$accession)) {  
      file_acc <- as.character(file_acc$accession)
    } else if (!is.null(file_acc$File)){ #input is a design
      file_acc <- sapply(file_acc$File, function(i){
        unlist(strsplit(i,"[.]"))[1]
      })
    }
  }
  #If the input is a list of design (split option set to TRUE)
  if(class(file_acc) == "list"){
    if(all(sapply(file_acc, is.data.table))){
     file_acc <- rbindlist(file_acc)
     file_acc <- sapply(file_acc$File, function(i){
       unlist(strsplit(i,"[.]"))[1]
     })
    }
  }
  
  
  stopifnot(is.character(file_acc))
  stopifnot(length(file_acc) > 0)
  
  if(is.null(df)) {
    data(encode_df, envir = environment())
    df <- encode_df
  }
  
  stopifnot(is.data.table(df))
  
  #Checking format argument
  if (format!="all" & !(format %in% unique(df$file_format))){
    msg <- paste0("Unavailable format ", format)
    warning(msg, call. = FALSE)
    return(NULL)
  }
  
  #Step 1 : Handling available file accession and experience accession via encode_df
  
  avail_file <- file_acc[file_acc %in% df$file_accession]
  avail_ds <- file_acc[file_acc %in% df$accession]
  unavail <- dplyr::setdiff(file_acc, c(avail_file,avail_ds))
  

  
  #Downloading available file from encode_df
  
  #Subsetting encode_df for given acc and format
  file_dt <- dplyr::filter(df, file_accession %in% avail_file)
  if(format != "all"){
      file_wrong_format <- dplyr::filter(file_dt, file_format != format)
      file_dt <- dplyr::setdiff(file_dt, file_wrong_format)
      if(nrow(file_wrong_format) > 0){
          msg <- paste0("Format ", format, " not available for files : ",
                    file_wrong_format$file_accession)
          warning(msg, call. = FALSE)
      }
  }
  
  exp_dt <- dplyr::filter(df, accession %in% avail_ds)
  if(nrow(exp_dt)>0 & format != "all"){
      temp <- unique(exp_dt$accession)
      exp_dt <- dplyr::filter(exp_dt, file_format == format)
      
      if(length(temp) != length(unique(exp_dt$accession))){
        exp_wrong_format <- dplyr::setdiff(temp, unique(exp_dt$accession))
        msg <- paste0("No ", format, " files within experiment ", exp_wrong_format)
        warning(msg, call. = FALSE)
      }
  }
  
  
  if(file.access(dir, mode = 2) == 0) {
    #Downloading files
    downloaded <- download_dt_file(file_dt, dir, force, show_experiment=FALSE)
    downloaded <- c(downloaded, download_dt_file(exp_dt, dir, force, show_experiment=TRUE))
    
    # Step 2 : Handling unavailable files via ENCODE rest-api
    url_search <- "https://www.encodeproject.org/search/?searchTerm="
    url_file <- "https://www.encodeproject.org/search/?type=file&title="
    url_ds <- "https://www.encodeproject.org/search/?type=file&dataset=/"
    filter <- "/&frame=object&format=json&limit=all"
    
    #Simple rest-api query to
    
    if(length(unavail) > 0) {
      
      for(i in 1:length(unavail)){
        #Step 2.1 : Handling files
        if(RCurl::url.exists(paste0(url_file,unavail[[i]],filter))){ 
          res <- jsonlite::fromJSON(paste0(url_file,unavail[[i]],filter))
          if (res[["notification"]] == "Success") {
            results <- res[["@graph"]]
          
            if(!is.null(results$href)){
              #Checking the format
              if(format != "all" & results$file_format != format){
                msg <- paste0("Format ",format," not available for file : ",unavail[[i]])
                warning(msg, call. = FALSE)
                next
              }
              
              downloaded <- c(downloaded, download_single_file(results$href, results$md5sum, dir, force))
              
            }else{
              msg <- paste0("No result found for ", unavail[[i]]," with format ", format)
              warning(msg, call. = FALSE)
            }
          }else{
            msg <- paste0("No result found for ", unavail[[i]])
            warning(msg, call. = FALSE)
          }
          #Step 2.2 Handling dataset-
        } else if (RCurl::url.exists(paste0(url_search, unavail[[i]],
                                            "&format=json&limit=all"))) {
          exp_json <- jsonlite::fromJSON(paste0(url_search, unavail[[i]],
                                                  "&format=json&limit=all"))
          exp_dataset <- exp_json[["@graph"]][["@id"]]
          exp_dataset <- gsub(exp_dataset, pattern="/(.*)/.*/", replacement =
                                  "\\1")
          
          if(RCurl::url.exists(paste0(url_ds, exp_dataset,"/", unavail[[i]],
                                      filter))){
            exp_tab <- jsonlite::fromJSON(paste0(url_ds, exp_dataset,"/",
                                                 unavail[[i]],filter))
            if (exp_tab[["notification"]] == "Success") {
              results <- exp_tab[["@graph"]]
              
              for(y in 1:nrow(results)){
                if(!is.null(results$href[[y]])){
                  #checking the format
                  if(format != "all" & results$file_format[[y]] != format){next}
                  
                  downloaded = download_single_file(file_url=results$href[[y]], 
                                                    file_md5=results$md5sum[[y]], 
                                                    dir=dir,
                                                    force=force)
                }
              }
            }
          }
        
        } else {
          msg <- paste0("No result found for ", unavail[[i]])
          warning(msg, call. = FALSE)
        }
      }
    }
    if(length(downloaded) > 0){
        print(paste0("Files can be found at ", getwd()))
        return(invisible(downloaded))
    }
  }else{
    msg <- paste0("Can't write in ", dir)
    warning(msg, call. = FALSE)
  }
}
