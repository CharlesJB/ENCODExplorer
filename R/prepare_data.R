cores <- 1

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
      # upper case for the first letter
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
#' @param mc.cores The number of cores to use, i.e. at most how many child processes will be run simultaneously. Default 1
#' Default: \code{ENCODEdb.sqlite}.
#'
#' @examples
#'   database_filename <- system.file("extdata/ENCODEdb.sqlite",
#'                                    package = "ENCODEdb")
#'   \dontrun{
#'     export_ENCODEdb_matrix(database_filename = database_filename)
#'   }
#' @import RSQLite
#' @import parallel
#' 
#' @export
export_ENCODEdb_matrix <- function(database_filename, mc.cores = NULL) {

  T1<-Sys.time()
  print(format(T1, "%a %b %d %X %Y"))
  
  if(!is.null(mc.cores))
    cores <<- mc.cores
  
  Tables <- step1(database_filename = database_filename)
  Tables$files <- step2(files = Tables$files)
  Tables$files <- step3(files = Tables$files, awards = Tables$awards, labs = Tables$labs, platforms = Tables$platforms)
  
  # suppression des tables inutiles
  Tables$awards = NULL
  Tables$labs = NULL
  Tables$platforms = NULL
  
  Tables$files <- step4(files = Tables$files, replicates = Tables$replicates, libraries = Tables$libraries, treatments = Tables$treatments)
  
  # suppression des tables inutiles
  Tables$replicates = NULL
  Tables$libraries = NULL
  Tables$treatments = NULL
  
  Tables$files <- step5(files = Tables$files)
  
  ### les experiences seront toutes reunies par encode_df$experiment et les autres entites seront ajoutee dans dataset
  ### split the dataframe
  
  not_experiments_idx <- which(!grepl(x = Tables$files$dataset, pattern = 'experiments'))
  experiments_idx <- which(grepl(x = Tables$files$dataset, pattern = 'experiments'))
  
  encode_df <- list(experiment = Tables$files[experiments_idx,],
                    dataset = Tables$files[not_experiments_idx,])
  
  # suppression des tables inutiles
  Tables$files = NULL
  
  # creation des nouvelles colonnes dans experiments
  empty_vector <- rep(x = NA, times = nrow(encode_df$experiment))
  encode_df$experiment <- cbind(encode_df$experiment, target = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, date_released = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, status = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, assay = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, biosample_type = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, biosample_name = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, controls = empty_vector)
  
  encode_df$experiment$target <- step6_target(experiments_files_acc = encode_df$experiment$accession, experiments = Tables$experiments)
  encode_df$experiment$date_released <- step6_date_released(files_acc = encode_df$experiment$accession, set = Tables$experiments)
  encode_df$experiment$status <- step6_status(files_acc = encode_df$experiment$accession, set = Tables$experiments)
  encode_df$experiment$assay <- step6_assay(experiments_files_acc = encode_df$experiment$accession, experiments = Tables$experiments)
  encode_df$experiment$biosample_type <- step6_biosample_type(experiments_files_acc = encode_df$experiment$accession, experiments = Tables$experiments)
  encode_df$experiment$biosample_name <- step6_biosample_name(experiments_files_acc = encode_df$experiment$accession, experiments = Tables$experiments)
  encode_df$experiment$controls <- step6_control(experiments_files_acc = encode_df$experiment$accession, experiments = Tables$experiments)
  
  # suppression des tables inutiles
  Tables$experiments = NULL
  
  encode_df$experiment <- cbind(encode_df$experiment, organism = empty_vector)
  encode_df$experiment <- step7(encode_df$experiment, targets = Tables$targets)
  encode_df$experiment <- step8(encode_df$experiment, targets = Tables$targets)
  encode_df$experiment <- step9(encode_df$experiment, organisms = Tables$organisms)
  
  # creation des nouvelles colonnes dans dataset
  empty_vector <- rep(x = NA, times = nrow(encode_df$dataset))
  encode_df$dataset <- cbind(encode_df$dataset, date_released = empty_vector)
  encode_df$dataset <- cbind(encode_df$dataset, status = empty_vector)
  
  encode_df$dataset$date_released <- step6_date_released(files_acc = encode_df$dataset$accession, set = Tables$datasets)
  encode_df$dataset$status <- step6_status(files_acc = encode_df$dataset$accession, set = Tables$datasets)
  
  # suppression des tables inutiles
  Tables$datasets = NULL
  
  # suppression des tables
  remove(Tables)
  
  Tdiff = Sys.time() - T1
  print(paste0("Building ENCODE_DF : ",Tdiff, " min"))
  
  encode_df
}

step1 <- function(database_filename){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), database_filename)
  
  ### Step 1 : fetch all needed data
  cat('Step 1 : fetch all needed data\n')
  rs <- RSQLite::dbSendQuery(con, 'select * from file;')
  all_files <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_files)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from experiment;')
  all_experiments <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_experiments)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from dataset;')
  all_datasets <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_datasets)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from matched_set;')
  all_matched_sets <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_matched_sets)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from lab;')
  all_labs <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_labs)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from award;')
  all_awards <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_awards)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from target;')
  all_targets <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_targets)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from biosample;')
  all_biosamples <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_biosamples)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from library;')
  all_librarys <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_librarys)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from organism;')
  all_organisms <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_organisms)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from treatment;')
  all_treatments <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_treatments)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from replicate;')
  all_replicates <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_replicates)
  
  rs <- RSQLite::dbSendQuery(con, 'select * from platform;')
  all_platforms <- RSQLite::dbFetch(rs, n = -1)
  RSQLite::dbClearResult(rs)
  nrow(all_platforms)
  
  
  RSQLite::dbDisconnect(con)
  
  return(list(
    files = all_files,
    experiments = all_experiments,
    datasets = all_datasets,
    matched_sets = all_matched_sets,
    labs = all_labs,
    awards = all_awards,
    targets = all_targets,
    biosamples = all_biosamples,
    libraries = all_librarys,
    organisms = all_organisms,
    treatments = all_treatments,
    replicates = all_replicates,
    platforms = all_platforms
  ))
}

step2 <- function(files){
  cat('Step 2 : renommage\n')
  ### Step 2 : renommage
  #### files
  names(files)[names(files) == 'status'] <- 'file_status'
  names(files)[names(files) == 'accession'] <- 'file_accession'
  names(files)[names(files) == 'award'] <- 'project'
  names(files)[names(files) == 'replicate'] <- 'replicate_list'
  
  return(files)
}

step3 <- function(files, awards, labs, platforms){
  cat('Step 3 : remplacement des references simple par leur valeur : project (anc. award), platform, lab, paired_with\n')
  ### Step 3 : remplacement des references simple par leur valeur : project (anc. award), platform, lab, paired_with
  #### il y a des awards inconnus....
  files$project <- unlist(
    mclapply(
      X = files$project, 
      FUN = function(x) {
        p <- subset(x = awards, awards$id == x)$project ;
        if(length(p)) return(p) else return(gsub(x = x, 
                                                 pattern = "/awards/(.*)/", 
                                                 replacement = '\\1'))
      },
      mc.cores =  cores
    )
  )
  
  files$paired_with <- gsub(x = files$paired_with, pattern = "/files/(.*)/", replacement = '\\1')
  
  files$platform <- unlist(
    mclapply(
      X = files$platform, 
      FUN = function(x) {
        p <- subset(x = platforms, platforms$id == x)$title ;
        if(length(p)) return(p) else return(gsub(x = x, 
                                                 pattern = "/platforms/(.*)/", 
                                                 replacement = '\\1'))
      },
      mc.cores =  cores
    )
  )
  
  files$lab <- unlist(
    mclapply(
      X = files$lab, 
      FUN = function(x) {
        p <- subset(x = labs, labs$id == x)$title ;
        if(length(p)) return(p) else return(gsub(x = x, 
                                                 pattern = "/labs/(.*)/", 
                                                 replacement = '\\1'))
      },
      mc.cores =  cores
    )
  )
  
  return(files)
}

step4 <- function(files, replicates, libraries, treatments){
  cat('Step 4.1 : remplacement des references complexes par les valeurs : replicate_list (anc.replicate)\n')
  ### Step 4 : remplacement des references complexes par les valeurs : replicate_list (anc.replicate) 
  ### => replicate$id + ajout de 2 colonnes technical_replicate_number, biological_replicate_number
  
  biological_replicate_number <- unlist(
    mclapply(
      X = files$replicate_list, 
      FUN = function(x) {
        p <- subset(x = replicates, replicates$id == x) ;
        if(length(p) && nrow(p) > 0) 
          p$biological_replicate_number 
        else 
          NA
      },
      mc.cores =  cores
    ))
  
  technical_replicate_number <- unlist(
    mclapply(
      X = files$replicate_list, 
      FUN = function(x) {
        p <- subset(x = replicates, replicates$id == x) ;
        if(length(p) && nrow(p) > 0) 
          p$technical_replicate_number 
        else 
          NA
      },
      mc.cores =  cores
    ))
  
  #all_files <- cbind(all_files, biological_replicate_number, technical_replicate_number)
  
  cat('Step 4.2 : treatment first get 1 replicate from replicate_list, from replicate get library, from library get treatment\n')
  # ### => treatment first get 1 replicate from replicate_list, from replicate get library, from library get treatment
  treatment <- unlist(mclapply(
    #X = as.character(all_files_experiments$replicate_list), # dataset accession
    X = as.character(files$replicate_list), # dataset accession
    FUN = function(x) {
      if(!is.na(x)) {
        p <- subset(x = replicates, replicates$id == x)$library
        if(length(p)) {
          p <- subset(x = libraries, libraries$id == p)$treatment
          if(length(p)) {
            p <- subset(x = treatments, treatments$id == p)$treatment_term_name
            if(length(p)) {
              return(p)
            } else {
              return(NA)
            }
          } else {
            return(NA)
          }
        } else {
          return(NA)
        }
      } else {
        return(NA)
      }
    },
    mc.cores =  cores
  ))
  
  #all_files_experiments <- cbind(all_files_experiments, treatment)
  
  files <- cbind(files, biological_replicate_number, technical_replicate_number, treatment)
  
  remove(treatment)
  remove(biological_replicate_number)
  remove(technical_replicate_number)
  
  return(files)
}

step5 <- function(files){
  cat('Step 5 : remplacement des references dataset\n')
  
  dataset_types <- gsub(x = files$dataset, pattern = "/(.*)s/.*/", replacement = "\\1")
  
  cat('Step 5.1 : accessions\n')
  ### => accession
  dataset_accessions <- gsub(x = files$dataset, pattern = "/.*/(.*)/", replacement = "\\1")
  
  files <- cbind(accession = dataset_accessions, dataset_type = dataset_types, files)
  
  remove(dataset_accessions)
  
  return(files)
}

step6_target <- function(experiments_files_acc, experiments) {
  cat('Step 6 : target\n')
  
  out <- unlist(mclapply(
    X = as.character(experiments_files_acc), # dataset accession
    FUN = function(x) {
      p <- subset(x = experiments, experiments$accession == x)$target
      if(length(p)) {
        p
      } else {
        NA
      }
    },
    mc.cores =  cores
  ))
  
  return(out)
}

step6_date_released <- function(files_acc, set) {
  cat('Step 6 : date_released\n')
  
  out <- unlist(mclapply(
    X = as.character(files_acc), # dataset accession
    FUN = function(x) {
      p <- subset(x = set, set$accession == x)$date_released
      if(length(p)) {
        p
      } else {
        NA
      }
    },
    mc.cores =  cores
  ))
  
  return(out)
}

step6_status <- function(files_acc, set) {
  cat('Step 6 : status\n')
  
  out <- unlist(mclapply(
    X = as.character(files_acc), # dataset accession
    FUN = function(x) {
      p <- subset(x = set, set$accession == x)$status
      if(length(p)) {
        p
      } else {
        NA
      }
    },
    mc.cores =  cores
  ))
  
  return(out)
}

step6_assay <- function(experiments_files_acc, experiments) {
  cat('Step 6 : assay\n')
  
  out <- unlist(mclapply(
    X = as.character(experiments_files_acc), # dataset accession
    FUN = function(x) {
      p <- subset(x = experiments, experiments$accession == x)$assay
      if(length(p)) {
        p
      } else {
        NA
      }
    },
    mc.cores =  cores
  ))
  
  return(out)
}

step6_biosample_type <- function(experiments_files_acc, experiments) {
  cat('Step 6 : biosample_type\n')
  
  out <- unlist(mclapply(
    X = as.character(experiments_files_acc), # dataset accession
    FUN = function(x) {
      p <- subset(x = experiments, experiments$accession == x)$biosample_type
      if(length(p)) {
        p
      } else {
        NA
      }
    },
    mc.cores =  cores
  ))
  
  return(out)
}

step6_biosample_name <- function(experiments_files_acc, experiments) {
  cat('Step 6 : biosample_name\n')
  
  out <- unlist(mclapply(
    X = as.character(experiments_files_acc), # dataset accession
    FUN = function(x) {
      p <- subset(x = experiments, experiments$accession == x)$biosample_term_name
      if(length(p)) {
        p
      } else {
        NA
      }
    },
    mc.cores =  cores
  ))
  
  return(out)
}

step6_control <- function(experiments_files_acc, experiments) {
  cat('Step 6 : controls\n')
  
  out <- unlist(mclapply(
    X = as.character(experiments_files_acc), # dataset accession
    FUN = function(x) {
      p <- subset(x = experiments, experiments$accession == x)$possible_controls
      if(length(p)) {
        p
      } else {
        NA
      }
    },
    mc.cores =  cores
  ))
  
  return(out)
}

step7 <- function(experiments_files, targets){
  cat('Step 7 : prepare organism\n')
  
  experiments_files$organism <- unlist(
    mclapply(
      X = experiments_files$target,
      FUN = function(x) {
        if(!is.na(x)) {
          p <- subset(x = targets, targets$id == x)$organism
          if(length(p)) {
            return(p)
          } else {
            return(NA)
          }
        } else {
          return(NA)
        }
      },
      mc.cores =  cores
    )
  )
  
  return(experiments_files)
}

step8 <- function(experiments_files, targets){
  cat('Step 8 : target id -> target name\n')
  
  experiments_files$target <- unlist(
    mclapply(
      X = experiments_files$target,
      FUN = function(x) {
        if(!is.na(x)) {
          p <- subset(x = targets, targets$id == x)$label
          if(length(p)) {
            return(p)
          } else {
            return(gsub(x = x, pattern = "/targets/(.*)/", replacement = '\\1'))
          }
        } else {
          return(NA)
        }
      },
      mc.cores =  cores
    )
  )
  
  return(experiments_files)
}

step9 <- function(experiments_files, organisms){
  cat('Step 9 : organism id -> organism name\n')
  ### organism id -> organism name
  
  experiments_files$organism <- unlist(
    mclapply(
      X = experiments_files$organism,
      FUN = function(x) {
        if(!is.na(x)) {
          p <- subset(x = organisms, organisms$id == x)$scientific_name
          if(length(p)) {
            return(p)
          } else {
            return(gsub(x = x, pattern = "/organisms/(.*)/", replacement = '\\1'))
          }
        } else {
          return(NA)
        }
      },
      mc.cores =  cores
    )
  )
  
  return(experiments_files)
}
