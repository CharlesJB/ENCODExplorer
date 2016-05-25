
#' Create the list of data.table for the tables in ENCODE
#' 
#' @return is a \code{list} with selected tables from ENCODE that were used to
#' create the list of \code{data.table} .
#'
#' @param database_filename The name of the file to save the database into.
#' @param types The names of the tables to extract from ENCODE rest api.
#' @param overwrite Should tables already present in database be overwrited
#' Default: \code{FALSE}.
#' 
#' @examples
#' prepare_ENCODEdb(database_filename = "tables.RDA", types = "platform")
#' file.remove("platform.RDA")
#'     \dontrun{
#'         prepare_ENCODEdb("ENCODEdb.RDA")
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
    # Extract the tables from the ENCODE rest api
    extract_type <- function(type) {
      table <- extract_table(type)
      table_clean <- clean_table(table)
    }
    # List of data.frame
    tables <- lapply(types, extract_type)
    
    # Return the named tables
    names(tables) <- types
    tables[sapply(tables, is.null)] <- NULL
    tables <- lapply(tables, as.data.table)
    save(tables, file=database_filename)
   
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

#' Extract essential informations from a list of data.table in a \code{list} of 
#' \code{data.table}s
#' 
#'
#' @return a \code{list} containing two elements. The first one 'experiment' is 
#' a \code{data.table} containing essential informations for each file part of    
#' an experiment ; the second one 'dataset' is a \code{data.table} containing 
#' essential informations for each file part of a dataset.
#'
#' @param database_filename The name of the file to save the database into.
#'
#' @examples
#'     database_filename <- system.file("extdata/tables",
#'                                                                        package = "ENCODEdb")
#'     \dontrun{
#'         export_ENCODEdb_matrix(database_filename = database_filename)
#'     }
#' @import parallel
#' 
#' @export
export_ENCODEdb_matrix <- function(database_filename) {
  # Step 1 : fetch all needed data.
  Tables <- step1(database_filename = database_filename)
  # Step 2 : renaming specific column.
  Tables$files <- step2(files = Tables$files)
  # Step 3 : updating project, paired-with, plateform and lab column.
  Tables$files <- step3(files = Tables$files, awards = Tables$awards, 
                        labs = Tables$labs, platforms = Tables$platforms)
  # Step 4 : Uptading replicate_list and treatment column.
  Tables$files <- step4(files = Tables$files, replicates = Tables$replicates, 
                        libraries = Tables$libraries, 
                        treatments = Tables$treatments,
                        biosamples = Tables$biosamples)
  # Step 5 : Splitting dataset column into column : accession & dataset_types.
  Tables$files <- step5(files = Tables$files)
  
  # Grouping into two table depending on the type of the dateset.
  
  not_experiments_idx <- which(!grepl(x = Tables$files$dataset, 
                                      pattern = 'experiments'))
  experiments_idx <- which(grepl(x = Tables$files$dataset, 
                                 pattern = 'experiments'))
  
  encode_df <- list(experiment = Tables$files[experiments_idx,],
                    dataset = Tables$files[not_experiments_idx,])
  
  # Creating the new column for experiment table.
  empty_vector <- rep(x = NA, times = nrow(encode_df$experiment))
  
  encode_df$experiment <- cbind(encode_df$experiment, target = empty_vector,
           date_released = empty_vector, status = empty_vector,
           assay = empty_vector, biosample_type = empty_vector,
           biosample_name = empty_vector, controls = empty_vector)

  # Step 6 to step 9: Updating the content of the new column.
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
  
  encode_df$experiment <- cbind(encode_df$experiment, organism = empty_vector)
  encode_df$experiment$organism <- step7(encode_df$experiment, Tables$targets)
  encode_df$experiment$target <- step8(encode_df$experiment, Tables$targets)
  encode_df$experiment$organism <- step9(encode_df$experiment, Tables$organisms)
  
  # Creating and updating new columns for dataset
  empty_vector <- rep(x = NA, times = nrow(encode_df$dataset))
  encode_df$dataset <- cbind(encode_df$dataset, date_released = empty_vector,
                             status = empty_vector)
  encode_df$dataset$date_released <- step6_date_released(encode_df$dataset, 
                                                         Tables$datasets)
  encode_df$dataset$status <- step6_status(encode_df$dataset, Tables$datasets)
  
  remove(Tables)
  encode_df
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
  # Updating treatment column with trreatment$treatment_term_name
  # replicate_list->library->biosample->treatment
  
  # Updating treatment_col with replicate$library using the link :
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
  # Step 5 : Splitting dataset column into two column
  dataset_types <- gsub(x = files$dataset, pattern = "/(.*)s/.*/", 
                        replacement = "\\1")
  dataset_accessions <- gsub(x = files$dataset, pattern = "/.*/(.*)/", 
                             replacement = "\\1")
  
  files <- cbind(accession = dataset_accessions, dataset_type = dataset_types, 
                 files)
  
  remove(dataset_accessions)
  remove(dataset_types)
  
  return(files)
}

step6_target <- function(encode_exp, experiments) {
  #Updating target column of encode_df with target column of Tables$experiments
  match_target <- match(encode_exp$accession, experiments$accession)
  match_target <- match_target[!is.na(match_target)]
  encode_exp$target <- as.character(encode_exp$target) 
  encode_exp[encode_exp$accession %in% experiments$accession, target :=
               experiments$target[match_target]]
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
