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
#' @import plyr
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
    }
    cat('The tables have been extracted ...\n')
    # List of data.frame
    tables <- lapply(types, extract_type)
    
    # Return the named tables
    names(tables) <- types
    tables[sapply(tables, is.null)] <- NULL
    cat('Converting format ...\n')
    tables <- lapply(tables, as.data.table)
    save(tables, file=database_filename)
    
    
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
#' @import parallel
#' 
#' @export
export_ENCODEdb_matrix <- function(database_filename) {
  
  T1<-Sys.time()
  
  Tables <- step1(database_filename = database_filename)
  Tables$files <- step2(files = Tables$files)
  Tables$files <- step3(files = Tables$files, awards = Tables$awards, 
                        labs = Tables$labs, platforms = Tables$platforms)
  
  # suppression des tables inutiles
  Tables$awards = NULL
  Tables$labs = NULL
  Tables$platforms = NULL
  
  Tables$files <- step4(files = Tables$files, replicates = Tables$replicates, 
                        libraries = Tables$libraries, 
                        treatments = Tables$treatments,
                        biosamples = Tables$biosamples)
  
  # suppression des tables inutiles
  Tables$replicates <- NULL
  Tables$libraries <- NULL
  Tables$treatments <- NULL
  
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
  encode_df$experiment <- cbind(encode_df$experiment, target = empty_vector,
           date_released = empty_vector, status = empty_vector,
           assay = empty_vector, biosample_type = empty_vector,
           biosample_name = empty_vector, controls = empty_vector)

  
  encode_df$experiment$target <- step6_target(encode_df$experiment, 
                                              Tables$experiments)
  encode_df$experiment$date_released <- step6_date_released(encode_df$experiment, 
                                                            Tables$experiments)
  encode_df$experiment$status <- step6_status(encode_df$experiment, 
                                              Tables$experiment)
  encode_df$experiment$assay <- step6_assay(encode_df$experiment, 
                                            Tables$experiments)
  encode_df$experiment$biosample_type <- step6_biosample_type(encode_df$experiment, 
                                                              Tables$experiments)
  encode_df$experiment$biosample_name <- step6_biosample_name(encode_df$experiment, 
                                                              Tables$experiments)
  encode_df$experiment$controls <- step6_control(encode_df$experiment, 
                                                 Tables$experiments)
  
  # suppression des tables inutiles
  Tables$experiments = NULL
  
  encode_df$experiment <- cbind(encode_df$experiment, organism = empty_vector)
  encode_df$experiment$organism <- step7(encode_df$experiment, Tables$targets)
  encode_df$experiment$target <- step8(encode_df$experiment, Tables$targets)
  encode_df$experiment$organism <- step9(encode_df$experiment, Tables$organisms)
  
  # creation des nouvelles colonnes dans dataset
  empty_vector <- rep(x = NA, times = nrow(encode_df$dataset))
  encode_df$dataset <- cbind(encode_df$dataset, date_released = empty_vector,
                             status = empty_vector)
  encode_df$dataset$date_released <- step6_date_released(encode_df$dataset, 
                                                         Tables$datasets)
  encode_df$dataset$status <- step6_status(encode_df$dataset, Tables$datasets)
  
  # suppression des tables inutiles
  Tables$datasets = NULL
  
  # suppression des tables
  remove(Tables)
  
  Tdiff = Sys.time() - T1
  print(paste0("Building ENCODE_DF : ",Tdiff, " sec"))
  
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
  
  ### Step 1 : fetch all needed data
  all_files <- tables$file
  all_experiments <- tables$experiment
  all_datasets <- tables$dataset
  all_matched_sets <- tables$matched_set
  all_labs <- tables$lab
  all_awards <- tables$award
  all_targets <- tables$target
  all_biosamples <- tables$biosample
  all_libraries <- tables$library
  all_organisms <- tables$organism
  all_treatments <- tables$treatment
  all_replicates <- tables$replicate
  all_platforms <- tables$platform
  
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
  ### Step 2 : renommage
  #### files
  names(files)[names(files) == 'status'] <- 'file_status'
  names(files)[names(files) == 'accession'] <- 'file_accession'
  names(files)[names(files) == 'award'] <- 'project'
  names(files)[names(files) == 'replicate'] <- 'replicate_list'
  
  return(files)
}

step3 <- function(files, awards, labs, platforms){
  # Updating files$project with awards$project
  match_vector <- match(files$project, awards$id)
  no_match <- is.na(match_vector)
  match_vector = match_vector[!is.na(match_vector)]
  
  files[files$project %in% awards$id, project := awards$project[match_vector]]
  files[no_match, project := gsub(project, pattern = "/.*/(.*)/", 
                                  replacement = '\\1')]
  
  # Updating files$paired_with
  files$paired_with <- gsub(x = files$paired_with, pattern = "/files/(.*)/", 
                            replacement = '\\1')
  
  # Updating files$platform with platform$title
  match_vector <- match(files$platform, platforms$id)
  no_match <- is.na(match_vector)
  match_vector = match_vector[!is.na(match_vector)]
  
  files[files$platform %in% platforms$id, platform := platforms$title[match_vector]]
  files[no_match, platform := gsub(platform, pattern = "/.*/(.*)/", 
                                  replacement = '\\1')]
  
  # Updating files$lab with labs$title
  match_vector <- match(files$lab, labs$id)
  no_match <- is.na(match_vector)
  match_vector = match_vector[!is.na(match_vector)]
  
  files[files$lab %in% labs$id, lab := labs$title[match_vector]]
  files[no_match, lab := gsub(lab, pattern = "/.*/(.*)/", 
                                   replacement = '\\1')]
  return(files)
}

step4 <- function(files, replicates, libraries, treatments, biosamples){
 
  # Updating biological_replicate_list with replicates$biological_replicate_number
  
  match_vector <- match(files$replicate_list, replicates$id)
  match_vector = match_vector[!is.na(match_vector)]
  suppressWarnings(files$biological_replicate_number <- rep(NULL,nrow(files)))
  files[files$replicate_list %in% replicates$id, biological_replicate_number := 
                  as.character(replicates$biological_replicate_number[match_vector])]

  # Updating technical_replicate_list with replicates$technical_replicate_number
  match_vector <- match(files$replicate_list, replicates$id)
  match_vector = match_vector[!is.na(match_vector)]
  suppressWarnings(files$technical_replicate_number <- rep(NULL, nrow(files)))
  files[files$replicate_list %in% replicates$id, technical_replicate_number := 
          replicates$technical_replicate_number[match_vector]]
 
  # replicate_list->library->biosample->treatment
  # updating treatment_col with replicate$library using the link :
  # Tables$files$replicate_list <---> replicate$id
  match_vector <- match(files$replicate_list, replicates$id)
  match_vector <- match_vector[!is.na(match_vector)]
  suppressWarnings(files$treatment <- rep(NULL, row(files)))
  files[files$replicate_list %in% replicates$id, treatment := 
           replicates$library[match_vector]]
  # Link library$biosample <---> biosample$id
  match_vector <- match(files$treatment,libraries$id)
  match_vector <- match_vector[!is.na(match_vector)]
  files[files$treatment %in% libraries$id, treatment :=
           libraries$biosample[match_vector]]
  # Link biosample$treatment <---> treatment$id
  match_vector <- match(files$treatment, biosamples$id)
  match_vector <- match_vector[!is.na(match_vector)]
  files[files$treatment %in% biosamples$id, treatment :=
          biosamples$treatments[match_vector]]
  # Accessing treatment$treatment_term_name
  match_vector <- match(files$treatment, treatments$id)
  match_vector <- match_vector[!is.na(match_vector)]
  files[files$treatment %in% treatments$id, treatment :=
           treatments$treatment_term_name[match_vector]]
  
  return(files)
}

step5 <- function(files){
  dataset_types <- gsub(x = files$dataset, pattern = "/(.*)s/.*/", 
                        replacement = "\\1")
  ### => accession
  dataset_accessions <- gsub(x = files$dataset, pattern = "/.*/(.*)/", 
                             replacement = "\\1")
  
  files <- cbind(accession = dataset_accessions, dataset_type = dataset_types, 
                 files)
  
  remove(dataset_accessions)
  
  return(files)
}

step6_target <- function(encode_exp, experiments) {
  #Updating target column of encode_df with target column of Tables$experiments
  match_target <- match(encode_exp$accession, experiments$accession)
  match_target <- match_target[!is.na(match_target)]
  encode_exp$target <- as.character(encode_exp$target) 
  encode_exp[encode_exp$accession %in% experiments$accession, target :=
               experiments$target[match_target]]
  # encode_exp$target <- gsub(encode_exp$target, pattern = "/.*/(.*)/", 
  #                           replacement = '\\1')
  return(encode_exp$target)
}

step6_date_released <- function(encode_exp, experiments) {
  #Updating date_released with the date_released of Tables$experiments
  
  match_date <- match(encode_exp$accession, experiments$accession)
  match_date <- match_date[!is.na(match_date)]
  encode_exp$date_released <- as.character(encode_exp$date_released) 
  encode_exp[encode_exp$accession %in% experiments$accession, date_released :=
               experiments$date_released[match_date]]
  return(encode_exp$date_released)
  
}

step6_status <- function(encode_exp, experiments) {
  match_status <- match(encode_exp$accession, experiments$accession)
  match_status <- match_status[!is.na(match_status)]
  encode_exp$status <- as.character(encode_exp$status) 
  encode_exp[encode_exp$accession %in% experiments$accession, status :=
               experiments$status[match_status]]
  return(encode_exp$status)
}

step6_assay <- function(encode_exp, experiments) {
  match_assay <- match(encode_exp$accession, experiments$accession)
  match_assay <- match_assay[!is.na(match_assay)]
  encode_exp$assay <- as.character(encode_exp$assay) 
  encode_exp[encode_exp$accession %in% experiments$accession, assay :=
               experiments$assay_title[match_assay]]
  return(encode_exp$assay)
}

step6_biosample_type <- function(encode_exp, experiments) {
  match_bio_type <- match(encode_exp$accession, experiments$accession)
  match_bio_type <- match_bio_type[!is.na(match_bio_type)]
  encode_exp$biosample_type <- as.character(encode_exp$biosample_type) 
  encode_exp[encode_exp$accession %in% experiments$accession, biosample_type :=
               experiments$biosample_type[match_bio_type]]
  return(encode_exp$biosample_type)
}

step6_biosample_name <- function(encode_exp, experiments) {
  # Updating biosample_name with Tables$experiments$biosample_name 
  match_bio_name <- match(encode_exp$accession, experiments$accession)
  match_bio_name <- match_bio_name[!is.na(match_bio_name)]
  encode_exp$biosample_name <- as.character(encode_exp$biosample_name) 
  encode_exp[encode_exp$accession %in% experiments$accession, biosample_name :=
               experiments$biosample_term_name[match_bio_name]]
  return(encode_exp$biosample_name)
}

step6_control <- function(encode_exp, experiments) {
  #Updating control with the possible_controls column from Tables$experiment
  match_control <- match(encode_exp$accession, experiments$accession)
  match_control <- match_control[!is.na(match_control)]
  encode_exp$controls <- as.character(encode_exp$controls) 
  encode_exp[encode_exp$accession %in% experiments$accession, controls :=
               experiments$possible_controls[match_control]]
  return(encode_exp$controls)
}

step7 <- function(encode_exp, tables_target){

  # Updating organism with the column organism from Tables$targets
  tables_target$id <- gsub(tables_target$id, pattern = "/.*/(.*)/", 
                           replacement = '\\1')
  match_target <- match(encode_exp$target, tables_target$id)
  match_target <- match_target[!is.na(match_target)]
  encode_exp$organism <- as.character(encode_exp$organism)
  encode_exp[encode_exp$target %in% tables_target$id, organism :=
               tables_target$organism[match_target]]

  return(encode_exp$organism)
}

step8 <- function(encode_exp, tables_target){
  # Updating target with the label columns of Tables$targets
  match_target <- match(encode_exp$target, tables_target$id)
  match_target <- match_target[!is.na(match_target)]
  encode_exp[encode_exp$target %in% tables_target$id, target :=
               tables_target$label[match_target]]
  return(encode_exp$target)
}

step9 <- function(encode_exp, tables_org){
  # Updating organism with scientific_name column from Tables$organism
  
  match_org <- match(encode_exp$organism, tables_org$id)
  match_org <- match_org[!is.na(match_org)]
  encode_exp[encode_exp$organism %in% tables_org$id, organism :=
               tables_org$scientific_name[match_org]]
  return (encode_exp$organism)
}
