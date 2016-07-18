#' downloadEncode is use to download  a serie of files or dataset 
#' by their accession. 
#' @param file_acc A \code{character} of ENCODE file accession or 
#' experiment accession. Can also be a data.table coming from any ENCODExplorer
#' research function.
#' @param df The reference \code{data.table} use to find the download. File 
#' that are not available will be search directly throught the current
#' ENCODE database. 
#' @param format The specific file format to download.
#' Default : all
#' @param dir The directory to locate the downloaded files
#' @param force \code{boolean} the allow to download a file when it already exist
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
    load(file=system.file("../data/encode_df.rda", package="ENCODExplorer"))
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
  
  encode_root = "https://www.encodeproject.org"
  
  #Downloading available file from encode_df
  
  #Subsetting encode_df for given acc and format
  file_dt <- dplyr::filter(df, file_accession %in% avail_file)
  if(format != "all"){
      file_wrong_format <- dplyr::filter(file_dt, file_format != format)
      file_dt <- dplyr::setdiff(file_dt, file_wrong_format)
      # if(nrow(file_wrong_format) > 0){
      #     msg <- paste0("Format ", format, " not available for files : ",
      #               file_wrong_format$file_accession)
      #     warning(msg, call. = FALSE)
      # }
  }
  
  exp_dt <- dplyr::filter(df, accession %in% avail_ds)
  if(nrow(exp_dt)>0 & format != "all"){
      temp <- unique(exp_dt$accession)
      exp_dt <- dplyr::filter(exp_dt, file_format == format)
      
      if(length(temp) != length(unique(exp_dt$accession))){
        exp_wrong_format <- dplyr::setdiff(temp, unique(exp_dt$accession))
        # msg <- paste0("No ", format, " files within experiment ", exp_wrong_format)
        # warning(msg, call. = FALSE)
      }
  }
  
  
  if(file.access(dir, mode = 2) == 0) {
    #Downloading files
    downloaded <- character()
    if(nrow(file_dt) > 0){
      for(i in 1:nrow(file_dt)){
        fileName = strsplit(x = file_dt$href[[i]],split = "@@download/",fixed = TRUE)[[1]][2]
        fileName <- paste0(dir,"/", fileName, sep="")
        href <- as.character(file_dt$href[[i]])
        md5sum_file <- tools::md5sum(paste(dir, fileName, sep="/"))
        md5sum_encode <- as.character(file_dt$md5sum[[i]])
        #Downloading the file
        if (force == TRUE | !(file.exists(fileName)) |
            (file.exists(fileName) & md5sum_file != md5sum_encode)){
          download.file(url=paste0(encode_root, href), quiet=TRUE,
                                   destfile=fileName, method = "curl",
                                   extra = "-O -L" )
          md5sum_file = tools::md5sum(paste0(fileName))
        }
        #Validating the download
        if(md5sum_file != md5sum_encode) {
          warning(paste0("No md5sum match for : ", fileName), 
                  call. = FALSE)
          #Si on as une erreur de telechargement, on tente de supprimer le fichier ?
        }else{
          print(paste0("Success downloading file : ", fileName))
          downloaded <- c(downloaded, fileName)
        }
        
      }
    }
    
    #Downloading experience
    if(nrow(exp_dt) > 0) {
      for(i in 1:nrow(exp_dt)){
        
        fileName = strsplit(x = exp_dt$href[[i]],split = "@@download/",fixed = TRUE)[[1]][2]
        fileName <- paste0(dir,"/", fileName, sep="")
        href <- as.character(exp_dt$href[[i]])
        md5sum_file <- tools::md5sum(paste(dir, fileName, sep="/"))
        md5sum_encode <- as.character(exp_dt$md5sum[[i]])
        
        if (force == TRUE | !(file.exists(fileName)) |
            (file.exists(fileName) & md5sum_file != md5sum_encode)){
          download.file(url=paste0(encode_root, href), quiet=TRUE,
                        destfile=fileName, method = "curl",
                        extra = "-O -L" )
          md5sum_file = tools::md5sum(paste0(fileName))
        }
        if(md5sum_file != md5sum_encode) {
          warning(paste0("No md5sum match for : ", fileName),
                  call. = FALSE)
          
        }else{
          print(paste0("Success downloading experiment :", unique(exp_dt$accession[[i]]),
                       ",file :", fileName))
          downloaded <- c(downloaded, fileName)
        }
  
      }
    }
    
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
              
              fileName = strsplit(x = results$href,split = "@@download/",fixed = TRUE)[[1]][2]
              fileName <- paste0(dir,"/", fileName, sep="")
              href <- results$href
              md5sum_file <- tools::md5sum(paste(dir, fileName, sep="/"))
              md5sum_restapi <- as.character(results$md5sum)
              #Downloading the file
              if (force == TRUE | !(file.exists(fileName)) |
                  (file.exists(fileName) & md5sum_file != md5sum_restapi)){
                download.file(url=paste0(encode_root, href), quiet=TRUE,
                              destfile=fileName, method = "curl",
                              extra = "-O -L" )
                md5sum_file = tools::md5sum(paste0(fileName))
              }
              #Validating the download
              if(md5sum_file != md5sum_restapi) {
                warning(paste0("No md5sum match for : ", fileName),
                        call. = FALSE)
              }else{
                print(paste0("Success downloading file :", fileName))
                downloaded <- c(downloaded, fileName)
              }
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
                  fileName = strsplit(x = results$href[[y]],split = "@@download/",fixed = TRUE)[[1]][2]
                  fileName <- paste0(dir,"/", fileName, sep="")
                  href <- as.character(results$href[[y]])
                  md5sum_file <- tools::md5sum(paste(dir, fileName, sep="/"))
                  md5sum_restapi <- as.character(results$md5sum[[y]])
                  if (force == TRUE | !(file.exists(fileName)) |
                      (file.exists(fileName) & md5sum_file != md5sum_restapi)){
                    download.file(url=paste0(encode_root, href), quiet=TRUE,
                                  destfile=fileName, method = "curl",
                                  extra = "-O -L" )
                    md5sum_file = tools::md5sum(paste0(fileName))
                  }
                  #Validating the download
                  if(md5sum_file != md5sum_restapi) {
                    warning(paste0("No md5sum match for : ", fileName),
                            call. = FALSE)
                  }else{
                    print(paste0("Success downloading file :", fileName))
                    downloaded <- c(downloaded, fileName)
                  }
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