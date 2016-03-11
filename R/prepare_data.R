#' Create the RSQLite databse for the tables in ENCODE
#' 
#' @return is a \code{list} with selected tables from ENCODE that were used to
#' create the \code{RSQLite} database.
#'
#' @param database_filename The name of the file to save the database into.
#' Default: \code{ENCODEdb.sqlite}.
#' 
#' @param types The names of the tables to extract from ENCODE rest api.
#' 
#' @param overwrite Should tables already present in database be overwrited?
#' Default: \code{FALSE}.
#' 
#' @examples
#' prepare_ENCODEdb(database_filename = "platform.sql", types = "platform")
#' file.remove("platform.sql")
#'   \dontrun{
#'     prepare_ENCODEdb("ENCODEdb.sqlite")
#'   }
#'   
#' @import jsonlite
#' @export
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
#' Default: \code{ENCODEdb.sqlite}.
#'
#' @examples
#'   database_filename <- system.file("extdata/ENCODEdb.sqlite",
#'                                    package = "ENCODEdb")
#'   \dontrun{
#'     export_ENCODEdb_matrix(database_filename = database_filename)
#'   }
#' @import RSQLite
#' 
#' @export
export_ENCODEdb_matrix <- function(database_filename) {
  T2 = Sys.time()
  con <- RSQLite::dbConnect(RSQLite::SQLite(), database_filename)
  query_exp = "select e.accession as accession, target.name as target, e.possible_controls as controls, l.title as lab, e.date_released, e.status, e.assay_term_name as assay, e.biosample_type, e.biosample_term_name as biosample_name, e.assembly, f.run_type, f.accession as file_accession, f.file_format, f.status as file_status, f.paired_end, f.paired_with, f.platform, f.replicate as replicate_list, f.href, f.md5sum FROM experiment as e, file as f, lab as l LEFT JOIN target ON e.target = target.id where f.dataset=e.id AND f.lab=l.id;"
  query_ds = "select d.accession, l.title as lab, d.date_released, d.status as status, d.assembly, f.accession as file_accession, f.file_format, f.status as file_status, d.related_files, f.href, f.md5sum from dataset as d, file as f, lab as l where f.dataset=d.id AND f.lab=l.id;"
  
  rs <- RSQLite::dbSendQuery(con, query_exp)
  encode_exp <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  
  rs <- RSQLite::dbSendQuery(con, query_ds)
  encode_ds <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  
  ############################ REQUETES SECONDAIRES ############################ 
  
  # pre-fetch the tables
  qry <- "select library, technical_replicate_number, biological_replicate_number, id from replicate"
  tbl_replicate<- RSQLite::dbFetch(RSQLite::dbSendQuery(con, qry), n = -1)
  RSQLite::dbClearResult(con)
  qry <- "select organism, id from biosample"
  tbl_biosample <- RSQLite::dbFetch(RSQLite::dbSendQuery(con, qry), n = -1)
  RSQLite::dbClearResult(con)
  qry <- "select biosample, treatments, id from library"
  tbl_library <- RSQLite::dbFetch(RSQLite::dbSendQuery(con, qry), n = -1)
  RSQLite::dbClearResult(con)
  qry <- "select scientific_name as organism, id from organism"
  tbl_organism <- RSQLite::dbFetch(RSQLite::dbSendQuery(con, qry), n = -1)
  RSQLite::dbClearResult(con)
  qry <- "select treatment_term_name, id from treatment"
  tbl_treatment <- RSQLite::dbFetch(RSQLite::dbSendQuery(con, qry), n = -1)
  RSQLite::dbClearResult(con)

  # Initialize empty vectors
  len <- length(encode_exp$replicate_list)
  organism <- character(len)
  treatment <- character(len)
  technical_replicate_number <- integer(len)
  biological_replicate_number <- integer(len)
  
  for (k in seq_along(encode_exp$replicate_list)) {
    id_replicate <- encode_exp$replicate_list[k]
    if (id_replicate %in% tbl_replicate$id & !is.na(id_replicate)) {
      
      ### GET library to get ORGANISM and TREATMENT
      id_library <- tbl_replicate$library[tbl_replicate$id == id_replicate]

      ## if there is no library, there is no way to get the organism neither the treatment infos 
      if (is.na(id_library)) {
        organism[k] <- NA
        treatment[k] <- NA
      } else {
	id_biosample <- tbl_library$biosample[tbl_library$id == id_library]

        ### GET ORGANISM 
	if (is.na(id_biosample) | ! id_biosample %in% tbl_biosample$id) {
	  organism[k] <- NA
	} else {
	  id_organism <- tbl_biosample$organism[tbl_biosample$id == id_biosample]
	  organism[k] <- tbl_organism$organism[tbl_organism$id == id_organism]
	}
        
        ### GET TREATMENT 
	id_treatment <- tbl_library$treatments[tbl_library$id == id_library]
        
	if (is.na(id_treatment)) {
	  treatment[k] <- NA
        } else {
          j <- tbl_treatment$id == id_treatment
	  if (grepl(";", id_treatment)) {
	    id_treatment <- unlist(strsplit(id_treatment, ";"))
	    j <- do.call("|", lapply(id_treatment, function(x) {
	      tbl_treatment$id == x}))
	  }
	  stopifnot(sum(j) >= 1)
	  if (sum(j) > 1) {
	    treatment[k] <- paste(tbl_treatment$treatment_term_name[j], collapse = ";")
	  } else {
	    treatment[k] <- tbl_treatment$treatment_term_name[j]
	  }
        }
      }
      ### GET REPLICATE INFOS 
      j <- tbl_replicate$id == id_replicate
      technical_replicate_number[k] <- tbl_replicate$technical_replicate_number[j]
      biological_replicate_number[k] <- tbl_replicate$biological_replicate_number[j]
    } else {
      organism[k] <- NA
      treatment[k] <- NA
      technical_replicate_number[k] <- NA
      biological_replicate_number[k] <- NA
    }
  }
  encode_exp = cbind(encode_exp, organism, treatment, 
                     technical_replicate_number, biological_replicate_number)
  
  RSQLite::dbDisconnect(con)
  
  
  ############################ AJOUT LIENS AVEC DATASET ############################ 
  
  dataset_accession = rep(NA, nrow(encode_exp))
  names(dataset_accession) = as.character(encode_exp$file_accession)
  dataset_with_file = subset(encode_ds, ! is.na(encode_ds$related_files), 
                             select = c("accession", "related_files" ))
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
  
  Tdiff = Sys.time() - T2
  print(paste0("Extract data from the DB : ",Tdiff, " min"))
  
  list(experiment = encode_exp, dataset = encode_ds)
}
