#' Create or update all the needed data for ENCODExplorer
#' 
#' This function creates or updates ENCODExplorer data according the following 
#' steps :
#' 1) Create the RSQLite databse for the tables in ENCODE
#' 2) Extract essential informations from the RSQLite databse in encode_df
#' 3) Extract accession numbers from all the datasets of RSQLite databse in 
#' accession_df
#' 4) if overwrite = TRUE, the new encode_df will overwrite the former one else 
#' return the newly generated objets.
#' 
#' @return none if \code{overwrite} is set to TRUE or return a \code{list} 
#' containg two objects encode_df and accession_df.
#'
#' @param database_filename The name of the file to save the database into.
#' @param overwrite Should tables already present in database be overwrited
#' @param mc.cores The number of cores to use. Default 1
#' Default: \code{FALSE}.
#' 
#' @examples
#'
#'     \dontrun{
#'         update_ENCODExplorer("ENCODEdb.sqlite")
#'     }
#'     
#' @import jsonlite
#' @export
update_ENCODExplorer <- function(database_filename = "inst/extdata/ENCODEdb.sqlite", overwrite = FALSE, mc.cores = 1){
  
  # 1) Create the RSQLite databse for the tables in ENCODE
  ret <- prepare_ENCODEdb(database_filename = database_filename, overwrite = overwrite)
  
  if(is.null(ret)){
    return(NULL)
  }
  
  # 2) Extract essential informations from the RSQLite databse in encode_df
  new_encode_df <- export_ENCODEdb_matrix(database_filename = database_filename,
                                          mc.cores = mc.cores)
  
  if(length(new_encode_df) != 2){
    return(NULL)
  }
  
  # 3) Extract accession numbers from all the datasets of RSQLite databse in accession_df
  new_accession_df <- export_ENCODEdb_accession(new_encode_df,database_filename)
  
  if(length(new_accession_df) != 1){
    return(NULL)
  }
  
  # 4) if overwrite = TRUE, the new encode_df will overwrite the former one
  if(overwrite){
    encode_df <- new_encode_df
    accession_df <- new_accession_df
    save(encode_df, file = 'data/encode_df.rda' )
    save(accession_df, file = 'data/accession_df.rda' )
  } else { # else return the update object
    invisible(list(
      encode_df = new_encode_df,
      accession_df = new_accession_df
    ))
  }
  
}


#' Create the RSQLite databse for the tables in ENCODE
#' 
#' @return is a \code{list} with selected tables from ENCODE that were used to
#' create the \code{RSQLite} database.
#'
#' @param database_filename The name of the file to save the database into.
#' @param types The names of the tables to extract from ENCODE rest api.
#' @param overwrite Should tables already present in database be overwrited
#' Default: \code{FALSE}.
#' 
#' @examples
#' prepare_ENCODEdb(database_filename = "platform.sql", types = "platform")
#' file.remove("platform.sql")
#'     \dontrun{
#'         prepare_ENCODEdb("ENCODEdb.sqlite")
#'     }
#'     
#' @import jsonlite
#' @import data.table
#' @export
prepare_ENCODEdb <- function(database_filename = "inst/extdata/tables.RDA",
                             types = get_encode_types(), overwrite = FALSE) {
  
  if(file.exists(database_filename) && !overwrite) {
    warning(paste0("The file ", database_filename, " already exists. Please delete it before re-run the data preparation"))
    NULL
  } else {
    T1<-Sys.time()
    # Extract the tables from the ENCODE rest api
    extract_type <- function(type) {
      table <- extract_table(type)
      table_clean <- clean_table(table)
      table <- as.data.table(table_clean)
    }

    tables <- lapply(types, extract_type)
    
    
    # Return the named tables
    names(tables) <- types
    tables[sapply(tables, is.null)] <- NULL
    
    Tdiff = Sys.time() - T1
    print(paste0("Extract the tables from the ENCODE rest api : ",Tdiff, " sec"))
    
    # Extract data from the DB
    if(length(tables) > 0) {
      invisible(tables)
    }
    else
    {
      warning(paste0("Some goes wrong during the data preparation. 
                        Please erase the database ",database_filename," and re-run the whole process.
                        If the problem persists, please contact us"))
      NULL
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
#' @param mc.cores The number of cores to use. Default 1
#'
#' @examples
#'     database_filename <- system.file("extdata/ENCODEdb.sqlite",
#'                                                                        package = "ENCODEdb")
#'     \dontrun{
#'         export_ENCODEdb_matrix(database_filename = database_filename)
#'     }
#' @import RSQLite
#' @import parallel
#' 
#' @export
export_ENCODEdb_matrix <- function(database_filename, mc.cores = 1) {
  
  T1<-Sys.time()
  
  Tables <- step1(database_filename = database_filename)
  Tables$files <- step2(files = Tables$files)
  Tables$files <- step3(files = Tables$files, awards = Tables$awards, 
                        labs = Tables$labs, platforms = Tables$platforms, 
                        mc.cores)
  
  # suppression des tables inutiles
  Tables$awards = NULL
  Tables$labs = NULL
  Tables$platforms = NULL
  
  Tables$files <- step4(files = Tables$files, replicates = Tables$replicates, 
                        libraries = Tables$libraries, 
                        treatments = Tables$treatments,mc.cores)
  
  # suppression des tables inutiles
  Tables$replicates = NULL
  Tables$libraries = NULL
  Tables$treatments = NULL
  
  Tables$files <- step5(files = Tables$files)
  
  ### les experiences seront toutes reunies par encode_df$experiment et les autres entites seront ajoutee dans dataset
  ### split the dataframe
  
  not_experiments_idx <- which(!grepl(x = Tables$files$dataset, 
                                      pattern = 'experiments'))
  experiments_idx <- which(grepl(x = Tables$files$dataset, 
                                 pattern = 'experiments'))
  
  encode_df <- list(experiment = Tables$files[experiments_idx,],
                    dataset = Tables$files[not_experiments_idx,])
  
  # suppression des tables inutiles
  Tables$files = NULL
  
  # creation des nouvelles colonnes dans experiments
  empty_vector <- rep(x = NA, times = nrow(encode_df$experiment))
  encode_df$experiment <- cbind(encode_df$experiment, target = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, 
                                date_released = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, status = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, assay = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, 
                                biosample_type = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, 
                                biosample_name = empty_vector)
  encode_df$experiment <- cbind(encode_df$experiment, controls = empty_vector)
  
  encode_df$experiment$target <- step6_target(encode_df$experiment$accession, 
                                              Tables$experiments,mc.cores)
  encode_df$experiment$date_released <- step6_date_released(encode_df$experiment$accession, 
                                                            Tables$experiments, mc.cores)
  encode_df$experiment$status <- step6_status(encode_df$experiment$accession, 
                                              Tables$experiment, mc.cores)
  encode_df$experiment$assay <- step6_assay(encode_df$experiment$accession, 
                                            Tables$experiments,mc.cores)
  encode_df$experiment$biosample_type <- step6_biosample_type(encode_df$experiment$accession, 
                                                              Tables$experiments,mc.cores)
  encode_df$experiment$biosample_name <- step6_biosample_name(encode_df$experiment$accession, 
                                                              Tables$experiments,mc.cores)
  encode_df$experiment$controls <- step6_control(encode_df$experiment$accession, 
                                                 Tables$experiments,mc.cores)
  
  # suppression des tables inutiles
  Tables$experiments = NULL
  
  encode_df$experiment <- cbind(encode_df$experiment, organism = empty_vector)
  encode_df$experiment <- step7(encode_df$experiment, 
                                targets = Tables$targets,mc.cores)
  encode_df$experiment <- step8(encode_df$experiment, 
                                targets = Tables$targets,mc.cores)
  encode_df$experiment <- step9(encode_df$experiment, 
                                organisms = Tables$organisms,mc.cores)
  
  # creation des nouvelles colonnes dans dataset
  empty_vector <- rep(x = NA, times = nrow(encode_df$dataset))
  encode_df$dataset <- cbind(encode_df$dataset, date_released = empty_vector)
  encode_df$dataset <- cbind(encode_df$dataset, status = empty_vector)
  
  encode_df$dataset$date_released <- step6_date_released(encode_df$dataset$accession, 
                                                         set = Tables$datasets,mc.cores)
  encode_df$dataset$status <- step6_status(encode_df$dataset$accession,
                                           set = Tables$datasets,mc.cores)
  
  # suppression des tables inutiles
  Tables$datasets = NULL
  
  # suppression des tables
  remove(Tables)
  
  Tdiff = Sys.time() - T1
  print(paste0("Building ENCODE_DF : ",Tdiff, " min"))
  
  encode_df
}


#' Extract accession numbers from all the datasets of RSQLite databse in a 
#' \code{data.frame}
#' 
#'
#' @return a \code{data.frame} composed of 3 fields : accession, 
#' files (\code{list} of files accessions) and dataset_type.
#' 
#' @param    df \code{list} of two \code{data.frame} containing ENCODE 
#' experiment and dataset metadata. Default
#' @param database_filename The name of the file to save the database into.
#'
#' @examples
#'     database_filename <- system.file("extdata/ENCODEdb.sqlite",
#'                                                                        package = "ENCODEdb")
#'     \dontrun{
#'         export_ENCODEdb_accession(database_filename = database_filename)
#'     }
#' @import RSQLite
#' 
#' @export
export_ENCODEdb_accession <- function(df = NULL, database_filename){
  
  if(is.null(df)) {data(encode_df, envir = environment())} else {encode_df = df}
  
  con <- RSQLite::dbConnect(RSQLite::SQLite(), database_filename)
  
  dataset_types <- unique(c(as.character(encode_df$experiment$dataset_type), 
                            as.character(encode_df$dataset$dataset_type)))
  dataset_types <- gsub(x = dataset_types, pattern = '-', replacement = '_')
  accession_df <- data.frame(accession = c(), files = list(), 
                             dataset_type = c(), stringsAsFactors = FALSE)
  
  for(dataset_type in dataset_types){
    rs <- RSQLite::dbSendQuery(con, paste0('select accession, files from ',
                                           dataset_type, ' ;'))
    results <- RSQLite::dbFetch(rs, n = -1)
    RSQLite::dbClearResult(rs)
    file_list <- strsplit(x = results$files, split = ';')
    results <- cbind(accession = results$accession, files = file_list, 
                     dataset_type = rep(x = dataset_type, 
                                        times = nrow(results)))
    accession_df <- rbind(accession_df, results)
  }
  
  RSQLite::dbDisconnect(con)
  
  accession_df$accession <- unlist(accession_df$accession)
  accession_df$dataset_type <- unlist(accession_df$dataset_type)

  invisible(accession_df)
}

step1 <- function(database_filename){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), database_filename)
  
  ### Step 1 : fetch all needed data
  cat('Step 1 : fetch all needed data\n')
  all_files <- RSQLite::dbReadTable(con, 'file')
  all_experiments <- RSQLite::dbReadTable(con, 'experiment')
  all_datasets <- RSQLite::dbReadTable(con, 'dataset')
  all_matched_sets <- RSQLite::dbReadTable(con, 'matched_set')
  all_labs <- RSQLite::dbReadTable(con, 'lab')
  all_awards <- RSQLite::dbReadTable(con, 'award')
  all_targets <- RSQLite::dbReadTable(con, 'target')
  all_biosamples <- RSQLite::dbReadTable(con, 'biosample')
  all_libraries <- RSQLite::dbReadTable(con, 'library')
  all_organisms <- RSQLite::dbReadTable(con, 'organism')
  all_treatments <- RSQLite::dbReadTable(con, 'treatment')
  all_replicates <- RSQLite::dbReadTable(con, 'replicate')
  all_platforms <- RSQLite::dbReadTable(con, 'platform')
  
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
    libraries = all_libraries,
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

step3 <- function(files, awards, labs, platforms, cores){
  cat('Step 3 : remplacement des references simple par leur valeur : ',
      'project (anc. award), platform, lab, paired_with\n')
  
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
      mc.cores =    cores
    )
  )
  
  files$paired_with <- gsub(x = files$paired_with, pattern = "/files/(.*)/", 
                            replacement = '\\1')
  
  files$platform <- unlist(
    mclapply(
      X = files$platform, 
      FUN = function(x) {
        p <- subset(x = platforms, platforms$id == x)$title ;
        if(length(p)) return(p) else return(gsub(x = x, 
                                                 pattern = "/platforms/(.*)/", 
                                                 replacement = '\\1'))
      },
      mc.cores =    cores
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
      mc.cores =    cores
    )
  )
  
  return(files)
}

step4 <- function(files, replicates, libraries, treatments, cores){
  cat('Step 4.1 : remplacement des references complexes par les valeurs : ',
      'replicate_list (anc.replicate)\n')
  
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
      mc.cores =    cores
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
      mc.cores =    cores
    ))
  
  cat('Step 4.2 : treatment first get 1 replicate from replicate_list, ',
      'from replicate get library, from library get treatment\n')
  
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
    mc.cores =    cores
  ))
  
  files <- cbind(files, biological_replicate_number, technical_replicate_number,
                 treatment)
  
  remove(treatment)
  remove(biological_replicate_number)
  remove(technical_replicate_number)
  
  return(files)
}

step5 <- function(files){
  cat('Step 5 : remplacement des references dataset\n')
  
  dataset_types <- gsub(x = files$dataset, pattern = "/(.*)s/.*/", 
                        replacement = "\\1")
  
  cat('Step 5.1 : accessions\n')
  ### => accession
  dataset_accessions <- gsub(x = files$dataset, pattern = "/.*/(.*)/", 
                             replacement = "\\1")
  
  files <- cbind(accession = dataset_accessions, dataset_type = dataset_types, 
                 files)
  
  remove(dataset_accessions)
  
  return(files)
}

step6_target <- function(experiments_files_acc, experiments, cores) {
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
    mc.cores =    cores
  ))
  
  return(out)
}

step6_date_released <- function(files_acc, set, cores) {
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
    mc.cores =    cores
  ))
  
  return(out)
}

step6_status <- function(files_acc, set, cores) {
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
    mc.cores =    cores
  ))
  
  return(out)
}

step6_assay <- function(experiments_files_acc, experiments, cores) {
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
    mc.cores =    cores
  ))
  
  return(out)
}

step6_biosample_type <- function(experiments_files_acc, experiments, cores) {
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
    mc.cores =    cores
  ))
  
  return(out)
}

step6_biosample_name <- function(experiments_files_acc, experiments, cores) {
  cat('Step 6 : biosample_name\n')
  
  out <- unlist(mclapply(
    X = as.character(experiments_files_acc), # dataset accession
    FUN = function(x) {
      p <- subset(x = experiments, 
                  experiments$accession == x)$biosample_term_name
      if(length(p)) {
        p
      } else {
        NA
      }
    },
    mc.cores =    cores
  ))
  
  return(out)
}

step6_control <- function(experiments_files_acc, experiments, cores) {
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
    mc.cores =    cores
  ))
  
  return(out)
}

step7 <- function(experiments_files, targets, cores){
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
      mc.cores =    cores
    )
  )
  
  return(experiments_files)
}

step8 <- function(experiments_files, targets, cores){
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
      mc.cores =    cores
    )
  )
  
  return(experiments_files)
}

step9 <- function(experiments_files, organisms, cores){
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
            return(gsub(x = x, pattern = "/organisms/(.*)/", 
                        replacement = '\\1'))
          }
        } else {
          return(NA)
        }
      },
      mc.cores =    cores
    )
  )
  
  invisible(experiments_files)
}
