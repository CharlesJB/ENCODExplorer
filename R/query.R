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
#' @param    df \code{data.frame} containing ENCODE 
#' experiment and dataset metadata 
#' @param    set_accession character string to select the accession
#' @param    assay character string to select the assay type
#' @param    biosample_name character string to select the biosample name
#' @param    dataset_accession character string to select the dataset accession
#' @param    file_accession character string to select the file accesion
#' @param    file_format character string to select the file format
#' @param    lab character string to select the laboratory 
#' @param    organism character string to select the donor organism
#' @param    target character string to select the experimental target
#' @param    treatment character string to select the treatment
#' @param    project character string to select the project
#' @param    biosample_type character string to select the biosample type
#' @param    file_status character string to select the file status 
#' ("released", "revoked", "all"). Default "released"
#' @param    status character string to select the dataset/experiment status
#' @param    fixed logical. If TRUE, pattern is a string to be matched as it is.
#' @param    quiet logical enables to switch off the result summary information 
#' when setting at TRUE.
#' @param    fuzzy Search for substring or alternate hyphenations. Default: TRUE
#'
#' @return a \code{data.frame}s containing data about ENCODE 
#' experiments and datasets
#'
#' @examples
#'     \dontrun{
#'     queryEncode(biosample_name = "A549", file_format = "bam")
#'     }
#' @import data.table
#' @export
queryEncode <- function(df = NULL, set_accession = NULL, assay = NULL, 
                        biosample_name = NULL, dataset_accession = NULL, 
                        file_accession = NULL, file_format = NULL, 
                        lab = NULL, organism = NULL, target = NULL, 
                        treatment = NULL, project = NULL, biosample_type = NULL,
                        file_status = "released", status = "released", 
                        fixed = TRUE, quiet = FALSE, fuzzy=FALSE) {
  
  # Do some parameter fixing to account for "all" values and preserve bahaviour.
  if(file_status=="all") {
      file_status = NULL;
  }
  
  if(status == "all") {
      status = NULL;
  }
  
  queryEncodeGeneric(df=df, fixed=fixed, quiet=quiet, fuzzy=fuzzy, 
                     accession = set_accession, assay = assay, 
                     biosample_name = biosample_name, dataset_accession = dataset_accession, 
                     file_accession = file_accession, file_format = file_format, 
                     lab = lab, organism = organism, target = target, 
                     treatment = treatment, project = project, biosample_type = biosample_type,
                     file_status = file_status, status = status)
}

#' Produce a subset of data following predefined criteria.
#'
#' After running the \code{prepare_ENCODEDb} function, this function will allow 
#' you to extract a subset of the files it describes. Search terms are passed in
#' as named parameters, where the parameter's name indicates the field, and its 
#' value the terms to be searched for. Each term may be a vector of values,
#' which are processed using the OR logical operation (the function will return
#' all results matching at least one of the terms). In contrast, separate
#' search fields are subjected to the AND logical operation.
#'
#' Possible search fields include the following: accession, assay name,
#' biosample, dataset accession, file accession, file format, laboratory,
#' donor organism, target and treatment.
#' 
#' By default, the query is made using exact matches. Set \code{fixed} to
#' \code{FALSE} to use regular expression matching, and \code{fuzzy} to
#' \code{TRUE} to search for substring or alternate hyphenations. These
#' options cannot be combined.
#' 
#' @param    df \code{data.frame} containing ENCODE 
#' experiment and dataset metadata 
#' @param    fixed logical. If TRUE, pattern is a string to be matched as it is.
#'   If FALSE, case insensitive perl regular expression matching is used.
#' @param    quiet logical enables to switch off the result summary information 
#' @param    fuzzy logical. If TRUE while fixed is also TRUE, allows
#'   searching by substrings and alternate space or hyphenation spellings.
#'   For example, "MCF7" will match "MCF-7" or "RNA-Seq" will match "polyA mRNA
#'   RNA-Seq".
#' @param    ... All other named parameters are used as terms to be searched for,
#'   with the parameter name naming the field (biosample_name, assay, etc.) and 
#'   the value being the terms that are searched for.
#'
#' @return a \code{data.frame}s containing data about ENCODE 
#' experiments and datasets
#'
#' @examples
#'     \dontrun{
#'     # Will return all bam files from biosample A549.
#'     queryEncodeGeneric(biosample_name = "A549", file_format = "bam")
#'
#'     # Will return all bam files from biosamples A549 and HeLA-S3.
#'     queryEncodeGeneric(biosample_name = c("A549", "HeLa-S3", file_format = "bam")
#'
#'     # Will return all fles where the assay contains RNA-Seq or a substrings
#'     # thereof, such as "polyA mRNA RNA-Seq" or "small RNA-Seq".
#'     queryEncodeGeneric(assay="RNA-Seq", fuzzy=TRUE)
#'     }
#' @import data.table
#' @export
queryEncodeGeneric <- function(df = NULL, fixed = TRUE, quiet = FALSE, 
                               fuzzy=FALSE, ...) {
  
  # Make sure that parameters are valid. Fuzzy searches are only possible
  # when the term is a fixed string and not a regular expression.
  if(!fixed && fuzzy) {
    stop(paste("Cannot perform fuzzy search using regular expression terms.",
               "Set fixed to TRUE or fuzzy to FALSE."))
  }
  
  # If no data.frame is provided, use the default one.
  if(is.null(df)) {
    # Load encode_df
    df = ENCODExplorer::encode_df
  }
  
  # Gather all search parameters, which are all arguments not explicitly in
  # the formal argument list. Do not keep unnamed arguments or aguments
  # set to NULL
  query_args = list(...)
  keep_args = rep(TRUE, length(query_args))
  for(i in 1:length(query_args)) {
    if(names(query_args)[i] == "" || is.null(query_args[[i]])) {
      keep_args[i] = FALSE
    }
  }
  query_args = query_args[keep_args]
  
  # Loop over all search fields, and update the selected_indices as we go.
  # We'll keep selected_indices to NULL as long as no valid search field
  # has occured so that we can differentiate between a search which contained
  # no terms and a search which returned no results.
  selected_indices = NULL
  for(query_field in names(query_args)) {
    field_hits = queryOneField(df, query_field, query_args[[query_field]],
                               fixed, fuzzy)
    
    # If the term was valid, update selected indices.
    if(!is.null(field_hits)) {
      if(!is.null(selected_indices)) {
        selected_indices = selected_indices & field_hits
      } else {
        selected_indices = field_hits
      }
    }
  }
  
  # Check if at least one valid term was provided.
  if(is.null(selected_indices)) {
    warning("Please provide at least one valid search criteria", call. = FALSE)
    return(NULL)
  }
  
  # Subset the data-frame and output status.
  query_results = df[selected_indices,]
  if(!quiet) {
    if(sum(selected_indices, na.rm=TRUE) == 0) {
      cat(paste("No result found in encode_df. You can try the <searchEncode>",
                "function or set the fuzzy option to TRUE.\n"))
    } else {
      cat(paste0("Results : ", nrow(query_results), " files, ", 
                 length(unique(query_results$accession)), " datasets", "\n"))   
    }
  }
  
  return(query_results)
}

# Search the encode_df for a single term within a single field and returns
# the indices of the subset of elements which are a match.
queryOneTerm <- function(df, field_name, individual_term, fixed, fuzzy) {
  results = FALSE
  if(fixed) {
    if(fuzzy) {
      results = grepl(x = df[[field_name]], pattern = query_transform(individual_term), 
                        ignore.case=TRUE, perl=TRUE)              
    } else {
      results = df[[field_name]] == individual_term
    }
  } else {
    results = grepl(x = df[[field_name]], pattern = individual_term, 
                      ignore.case=TRUE, perl=TRUE)
  }
  
  return(results)
}

# Search the encode_df for one or more terms within a single field and returns
# the indices of the subset of elements which are a match to any one of the
# terms, or NULL if the field is not a valid column of the passed-in data-frame.
queryOneField <- function(df, field_name, query_term, fixed, fuzzy) {
  # Make sure the given column exists.
  if(!(field_name %in% names(df))) {
    warning(paste(field_name, " is not a column of the passed data-frame. It will be ignored."))
    return(NULL)
  } else {
    # Make sure the given term are strings.
    if(!is.character(query_term)) {
      stop(paste0(query_term, " is not a character object."))
    }
    
    # Loop over all terms within the field.
    selected_indices = FALSE
    for(individual_term in query_term) {
      selected_indices = selected_indices | queryOneTerm(df, field_name, individual_term, fixed, fuzzy)
    }
    return(selected_indices)
  }
}

# Transforms a query term into a regular expression matching
# alternate spacing/hyphenation as well as sub-strings.
query_transform <- function(my.term) {
  my.term = gsub(my.term, pattern = " ", replacement = "", fixed =TRUE)
  my.term = gsub(my.term, pattern = "-", replacement = "", fixed =TRUE)
  my.term = gsub(my.term, pattern = ",", replacement = "", fixed =TRUE)
  my.term = strsplit(my.term, split = "")[[1]]
  my.term.4.grep = paste(my.term, collapse = "[ ,-]?")
  
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
searchToquery <- function(df = NULL, searchResults, quiet = TRUE){
  
  if(is.null(df)) {
    df = ENCODExplorer::encode_df
  }
  
  res = data.frame()
  
  if(nrow(searchResults) > 0){
    ids <- as.character(searchResults$id)
    ids <- ids[grepl(x = ids, pattern = '/experiments/')]
    accessions <- gsub(x = ids, pattern = '/experiments/([A-Z0-9]+)/',
                       replacement = "\\1")
    accessions <-unique(accessions)
        
    for(i in seq_along(accessions)){
      
      accession <- accessions[i]
      
      r <- queryEncode(df = df, set_accession = accession, fixed = TRUE, 
                       quiet = quiet)
      if(!is.null(r)){
        res <- rbind(res,r)
        
      }
    }
  }
  
  if(!quiet){
    cat(paste0("Results : ", nrow(res)," files , ",
               length(unique(res$accession)), " datasets", "\n"))
  }
  return(res)
}
