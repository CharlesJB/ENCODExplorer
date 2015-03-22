#' Create the RSQLite databse for the tables in ENCODE
#' 
#'
#' @return is a \code{list} with selected tables from ENCODE that were used to
#' create the \code{RSQLite} database.
#'
#' @param database_filename The name of the file to save the database into.
#' Default: \code{\"ENCODEdb.sqlite\"}.
#' 
#' @param type The names of the tables to extract from ENCODE rest api.
#' 
#' @param overwrite Should tables already present in database be overwrited?
#' Default: \code{FALSE}.
#'
#' @examples
#' \dontrun{
#'   lab <- prepare_ENCODEdb("encode.sqlite")
#'  }
#'  
#'  @export
prepare_ENCODEdb <- function(database_filename = "inst/extdata/ENCODEdb.sqlite",
                             types = get_encode_types(), overwrite = FALSE) {
  
  if(file.exists(database_filename)) {
    warning(paste0("The file ", database_filename, " already exists. Please delete it before re-run the data preparation"))
  }
  else
  {
    print(format(Sys.time(), "%a %b %d %X %Y"))
    T1<-Sys.time()
    # Extract the tables from the ENCODE rest api
    extract_type <- function(type) {
      
      table <- extract_table(type)
      table <- clean_table(table)
      
      if (all(dim(table) > 0)) {
        RSQLite::dbWriteTable(con, type, table, overwrite = overwrite)
      } else {
        table <- NULL
      }
      table
    }
    con <- RSQLite::dbConnect(RSQLite::SQLite(), database_filename)
    tables <- lapply(types, extract_type)
    RSQLite::dbDisconnect(con)
    
    # Return the named tables
    names(tables) <- types
    tables[sapply(tables, is.null)] <- NULL
    
    Tdiff = Sys.time() - T1
    print(paste0("Extract the tables from the ENCODE rest api : ",Tdiff, " min"))
    
    
    # Extract data from the DB
    if(length(tables) > 0) {
      invisible(tables)
    }
    else
    {
      warning(paste0("Some goes wrong during the data preparation. 
            Please erase the database ",database_filename," and re-run the whole process.
            If the problem persists, please contact us"))
      list()
    }
    
  }
}

#' Extract essential informations from the RSQLite databse in a \code{list} of 
#' \code{data.frame}s
#' 
#'
#' @return a \code{list} containing two elements. The first one 'experiment' is 
#' a \code{data.frame} containing essential informations for each file part of  
#' an experiment ; the second one 'dataset' is a \code{data.frame} containing 
#' essential informations for each file part of a dataset.
#'
#' @param database_filename The name of the file to save the database into.
#' Default: \code{\"ENCODEdb.sqlite\"}.
#'
#' @examples
#' \dontrun{
#'   matrices <- export_ENCODEdb_matrix("encode.sqlite")
#'  }
#'  
#'  @export
export_ENCODEdb_matrix <- function(database_filename) {
  T2 = Sys.time()
  con <- RSQLite::dbConnect(RSQLite::SQLite(), database_filename)
  query_exp = "select e.accession as accession, target.name as target, e.possible_controls as controls, l.title as lab, e.date_released, e.status, e.assay_term_name as assay, e.biosample_type, e.biosample_term_name as biosample_name, e.assembly, e.run_type, f.accession as file_accession, f.file_format, f.paired_end, f.paired_with, f.platform, f.replicate as replicate_list, f.href, f.md5sum FROM experiment as e, file as f, lab as l LEFT JOIN target ON e.target = target.id where f.dataset=e.id AND f.lab=l.id;"
  query_ds = "select d.accession, l.title as lab, d.date_released, d.status as status, d.assembly, f.accession as file_accession, f.file_format, d.related_files, f.href, f.md5sum from dataset as d, file as f, lab as l where f.dataset=d.id AND f.lab=l.id;"
  
  rs <- RSQLite::dbSendQuery(con, query_exp)
  encode_exp <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  
  rs <- RSQLite::dbSendQuery(con, query_ds)
  encode_ds <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  
  ############################ REQUETES SECONDAIRES ############################ 
  
  organism  = c()
  treatment = c()
  biological_replicate_number = c()
  technical_replicate_number = c()
  
  for(replicate in encode_exp$replicate_list) {
    if(!is.na(replicate)) {
      
      ### GET library to get ORGANISM and TREATMENT
      query_lib = paste0("select library as lib from replicate where id=\"",replicate,"\"")
      rs <- RSQLite::dbSendQuery(con, query_lib)
      res <- RSQLite::dbFetch(rs, n = -1)
      RSQLite::dbClearResult(rs)
      
      ## if there is no library, there is no way to get the organism neither the treatment infos 
      if(length(res$lib) == 0) {
        organism = c(organism,NA)
        treatment = c(treatment,NA)
      }
      else
      {
        rep.library = res$lib
        ### GET ORGANISM 
        query_org = paste0("select o.scientific_name as organism from organism as o where id= (select organism from biosample where id=(select biosample from library where id=\"",rep.library,"\"));")
        rs <- RSQLite::dbSendQuery(con, query_org)
        res2 <- RSQLite::dbFetch(rs, n = -1)
        RSQLite::dbClearResult(rs)
        
        if(length(res2$organism) == 0) {
          organism = c(organism,NA)
        }
        else
        {
          organism = c(organism,res2$organism)
        }
        
        ### GET TREATMENT 
        query_treat = paste0("select treatment_term_name from treatment where id = (select treatments from library where id=\"",rep.library,"\");")
        rs <- RSQLite::dbSendQuery(con, query_treat)
        res2 <- RSQLite::dbFetch(rs, n = -1)
        RSQLite::dbClearResult(rs)
        
        if(length(res2$treatment_term_name) == 0) {
          treatment = c(treatment,NA)
        }
        else
        {
          treatment = c(treatment,res2$treatment_term_name)
        }
      }
      ### GET REPLICATE INFOS 
      query_rep = paste0("select technical_replicate_number,biological_replicate_number from replicate where id=\"",replicate,"\";")
      rs <- RSQLite::dbSendQuery(con, query_rep)
      res2 <- RSQLite::dbFetch(rs, n = -1)
      RSQLite::dbClearResult(rs)
      technical_replicate_number = 
        c(technical_replicate_number,res2$technical_replicate_number)
      biological_replicate_number = 
        c(biological_replicate_number,res2$biological_replicate_number)
      
    } else {
      
      organism = c(organism,NA)
      treatment = c(treatment,NA) 
      technical_replicate_number = c(technical_replicate_number,NA)
      biological_replicate_number = c(biological_replicate_number,NA)
      
    }
  }
  encode_exp = cbind(encode_exp, organism, treatment, 
                     technical_replicate_number, biological_replicate_number)
  
  RSQLite::dbDisconnect(con)
  
  
  ############################ AJOUT LIENS AVEC DATASET ############################ 
  
  dataset_accession = rep(NA, nrow(encode_exp))
  names(dataset_accession) = as.character(encode_exp$file_accession)
  dataset_with_file = subset(encode_ds, ! is.na(encode_ds$related_files), 
                             select = c(accession, related_files))
  dataset_with_file = unique(dataset_with_file)
  
  for (i in (1:nrow(dataset_with_file))) {
    ds = dataset_with_file[i,]
    dsId = as.character(ds$accession)
    related_files = strsplit(ds$related_files, split = ";")[[1]]
    for (f in related_files) {
      f = strsplit(f, split = "/")[[1]][3]
      if(f %in% names(dataset_accession)) {
        dataset_accession[[f]] = dsId
      }
    }    
  }
  
  encode_exp = cbind(encode_exp, dataset_accession)
  
  print(format(Sys.time(), "%a %b %d %X %Y"))
  Tdiff = Sys.time() - T2
  print(paste0("Extract data from the DB : ",Tdiff, " min"))
  
  list(experiment = encode_exp, dataset = encode_ds)
}



#' A list of known tables from ENCODE database.
#'
#' The type (table) names are extracted from the schema list from ENCODE-DCC
#' github repository:
#'   https://github.com/ENCODE-DCC/encoded/tree/master/src/encoded/schemas
#'
#' The data is extracted using the github api:
#'   https://developer.github.com/guides/getting-started/
#'
#' @return a vector of \code{character} with the names of the known tables in
#'   the ENCODE database.
#'
#' @examples
#'   types <- get_encode_types()
#'
#'  @export
get_encode_types <- function() {
  encode_api_url <- "https://api.github.com/repos"
  encoded_repo <- "encode-dcc/encoded"
  schemas <- "src/encoded/schemas"
  url <- paste(encode_api_url, encoded_repo, "contents", schemas, sep = "/")
  tools::file_path_sans_ext(jsonlite::fromJSON(url)$name)
}
