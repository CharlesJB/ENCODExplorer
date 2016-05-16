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
#' @param    df \code{list} of two \code{data.frame} containing ENCODE 
#' experiment and dataset metadata 
#' @param    set_accession character string to select the experiment or dataset 
#' accession
#' @param    assay character string to select the assay type
#' @param    biosample character string to select the biosample name
#' @param    dataset_accession character string to select the dataset accession
#' @param    file_accession character string to select the file accesion
#' @param    file_format character string to select the file format
#' @param    lab character string to select the laboratory 
#' @param    organism character string to select the donor organism
#' @param    target character string to select the experimental target
#' @param    treatment character string to select the treatment
#' @param    project character string to select the project
#' @param    file_status character string to select the file status 
#' ("released", "revoked", "all"). Default "released"
#' @param    status character string to select the dataset/experiment status
#' @param    fixed logical. If TRUE, pattern is a string to be matched as it is.
#' @param    quiet logical enables to switch off the result summary information 
#' when setting at TRUE.
#'
#' @return a \code{list} of two \code{data.frame}s containing data about ENCODE 
#' experiments and datasets
#'
#' @examples
#'     queryEncode(biosample = "A549", file_format = "bam")
#'
#' @export
queryEncode <- function(df = NULL, set_accession = NULL, assay = NULL, 
                        biosample = NULL, dataset_accession = NULL, 
                        file_accession = NULL, file_format = NULL, 
                        lab = NULL, organism = NULL, target = NULL, 
                        treatment = NULL, project = NULL,
                        file_status = "released", status = "released", 
                        fixed = TRUE, quiet = FALSE) {
  
  if(is.null(df)) {data(encode_df, envir = environment())} else {encode_df = df}
  
  if(is.null(set_accession) && is.null(assay) && is.null(biosample) && 
     is.null(dataset_accession) && is.null(file_accession) && 
     is.null(file_format) && is.null(lab) && is.null(organism) &&
     is.null(target) && is.null(treatment) && is.null(project))
  {
    warning("Please provide at least one valid criteria", call. = FALSE)
    NULL
    
  } else {
    s1 = encode_df$experiment
    s2 = encode_df$dataset
    
    ac = set_accession
    as = assay
    bs = biosample
    da = dataset_accession
    fa = file_accession
    ff = file_format
    lb = lab
    og = organism
    tg = target
    tr = treatment
    es = status
    fs = file_status
    pr = project
    
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
      
      if(fs != "all") {
        s1 <- subset(s1, s1$file_status == fs)
        s2 <- subset(s2, s2$file_status == fs)
      }
      
      if(es != "all") {
        s1 <- subset(s1, s1$status == es)
        s2 <- subset(s2, s2$status == es)
      }
      
      if(!is.null(pr)) {
        s1 <- subset(s1, s1$project == pr)
        s2 <- subset(s2, s2$project == pr)
      }
      
    } else {
      # retirer ignorer les espaces, les tirets et la casse
      # m cf 7 = MCf7 = mcf-7 = MCF-7 ... etc
      
      if(!is.null(ac)) {
        query.transfo = query_transform(ac)
        select.entries = grepl(x = s1$accession, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$accession, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(as)) {
        query.transfo = query_transform(as)
        select.entries = grepl(x = s1$assay, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$assay, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(bs)) {
        query.transfo = query_transform(bs)
        select.entries = grepl(x = s1$biosample_name, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$biosample_name, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(da)) {
        query.transfo = query_transform(da)
        select.entries = grepl(x = s1$dataset_accession, pattern =query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$accession, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(fa)) {
        query.transfo = query_transform(fa)
        select.entries = grepl(x = s1$file_accession, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$file_accession, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(ff)) {
        query.transfo = query_transform(ff)
        select.entries = grepl(x = s1$file_format, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$file_format, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(lb)) {
        query.transfo = query_transform(lb)
        select.entries = grepl(x = s1$lab, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$lab, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(og)) {
        query.transfo = query_transform(og)
        select.entries = grepl(x = s1$organism, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$organism, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(tg)) {
        query.transfo = query_transform(tg)
        select.entries = grepl(x = s1$target, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$target, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(tr)) {
        query.transfo = query_transform(tr)
        select.entries = grepl(x = s1$treatment, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$treatment, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(fs != "all") {
        query.transfo = query_transform(fs)
        select.entries = grepl(x = s1$file_status, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$file_status, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(es != "all") {
        query.transfo = query_transform(es)
        select.entries = grepl(x = s1$status, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$status, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
      
      if(!is.null(pr)) {
        query.transfo = query_transform(pr)
        select.entries = grepl(x = s1$project, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s1 = s1[select.entries,]
        
        select.entries = grepl(x = s2$project, pattern = query.transfo, 
                               ignore.case =TRUE, perl =TRUE)
        s2 = s2[select.entries,]
      }
    }
    
    
    
    if((nrow(s1) + nrow(s2)) == 0) {
      warning_message <- "No result found in encode_df. 
      You can try the <searchEncode> function or set the fixed option to FALSE."
      
      s3 <- NULL
      if(!is.null(ac)){
        s3 <- resolveEncodeAccession(ac)
      } else {
        if(!is.null(da)){
          s3 <- resolveEncodeAccession(da)
        }
      }
      
      if(!is.null(s3)) {
        return(s3)
      } else {
        warning(warning_message, call. = FALSE)
        NULL
      }
    }
    else
    {
      query_results = list(experiment = s1, dataset = s2)
      if(!quiet) 
        cat(paste0("experiment results : ",
                   nrow(query_results$experiment),
                   " files / ",length(unique(query_results$experiment$accession)),
                   " experiments ; dataset results : ",nrow(query_results$dataset),
                   " files / ",length(unique(query_results$dataset$accession)),
                   " experiments\n"))
      query_results
    }
    
  }
}

query_transform <- function(my.term) {
  my.term = gsub(my.term, pattern = " ", replacement = "", fixed =TRUE)
  my.term = gsub(my.term, pattern = "-", replacement = "", fixed =TRUE)
  my.term = gsub(my.term, pattern = ",", replacement = "", fixed =TRUE)
  my.term = strsplit(my.term, split = "")[[1]]
  my.term.4.grep = paste0("^",paste(my.term, collapse = "[ ,-]?"))
  
  my.term.4.grep
}


#' Convert searchEncode output in queryEncode output.
#'
#' After processing to a basic search with the \code{searchEncode} function 
#' you can convert your result in a queryEncode output. Thus you can benefit 
#' from the collected metadata. 
#' 
#' The output is compatible with the dowload function.
#' 
#' @param    df \code{list} of two \code{data.frame} containing ENCODE 
#' experiment and dataset metadata.
#' @param searchResults the results set generated from \code{searchEncode}
#' @param    quiet logical enables to switch off the result summary information 
#' when setting at TRUE.
#'
#' @return a \code{list} of two \code{data.frame}s containing data about ENCODE 
#' experiments and datasets
#' 
#' @examples
#' search_res <- searchEncode(searchTerm = "switchgear elavl1", limit = "1")
#' res <- searchToquery(searchResults = search_res, quiet = TRUE)
#' 
#' @export
searchToquery <- function(df = NULL, searchResults, quiet = FALSE){
  
  if(is.null(df)) {data(encode_df, envir = environment())} else {encode_df = df}
  
  res = list(experiment = NULL, 
             dataset = NULL)
  
  if(nrow(searchResults) > 0){
    ids <- as.character(searchResults$id)
    ids <- ids[grepl(x = ids, pattern = '/experiments/')]
    accessions <- gsub(x = ids, pattern = '/experiments/([A-Z0-9]+)/',
                       replacement = "\\1")
    
    for(i in seq_along(accessions)){
      
      accession <- accessions[i]
      
      r <- queryEncode(df = encode_df, set_accession = accession, fixed = TRUE, 
                       quiet = quiet)
      if(!is.null(r)){
        
        if(!is.null(res$experiment))
          res$experiment <- rbind(res$experiment, r$experiment)
        else
          res$experiment <- r$experiment
        
        if(!is.null(res$dataset))
          res$dataset <- rbind(res$dataset, r$dataset)
        else
          res$dataset <- r$dataset
      }
    }
  }
  
  if(!quiet) 
    cat(paste0("Total : experiment results : ",
               nrow(res$experiment),
               " files / ",length(unique(res$experiment$accession)),
               " experiments ; dataset results : ",nrow(res$dataset),
               " files / ",length(unique(res$dataset$accession)),
               " experiments\n"))
  return(res)
}


#' Return a \code{data.frame} containing basic datasets information from an 
#' accession number
#'
#' @param accession character, dataset accession number
#'
#' @return a \code{data.frame} containing basic datasets information for the 
#' requested accession number
#' 
#' @examples
#' res <- resolveEncodeAccession(accession = 'ENCSR361ONJ')$accession
#' 
#' @export
resolveEncodeAccession <- function(accession){
  data(accession_df, envir = environment())
  ret <- accession_df[accession_df$accession == accession,]
  files <-  gsub(x = as.character(ret$files[[1]]), pattern = "/files/(.*)/", replacement = "\\1")
  i <- encode_df$dataset$file_accession %in% files
  j <- encode_df$experiment$file_accession %in% files
  
  list(experiment = encode_df$experiment[j,],
       dataset = encode_df$dataset[i,])
}
