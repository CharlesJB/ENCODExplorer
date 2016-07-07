
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
  Tables <- step1(database_file = database_filename)
  # Step 2 : renaming specific column.
  Tables$files <- step2(files = Tables$files)
  # Step 3 : updating project, paired-with, plateform and lab column.
  Tables$files <- step3(files = Tables$files, awards = Tables$awards, 
                        labs = Tables$labs, platforms = Tables$platforms)
  # Step 4 : Uptading replicate_list and treatment column.
  Tables$files <- step4(files = Tables$files, replicates = Tables$replicates, 
                        libraries = Tables$libraries, 
                        treatments = Tables$treatments,
                        biosamples = Tables$biosamples,
                        antibody_lot = Tables$antibody,
                        antibody_charac = Tables$antibody_charac)
  
  Tables$files <- step5(files = Tables$files)
  
  encode_df <- Tables$files
  
  # Creating the new columns
  empty_vector <- rep(x = NA, times = nrow(encode_df))
  
  encode_df <- cbind(encode_df, target = empty_vector,
           date_released = empty_vector, status = empty_vector,
           assay = empty_vector, biosample_type = empty_vector,
           biosample_name = empty_vector, controls = empty_vector)

  # Step 6 to step 9: Updating the content of the new column.
  encode_df$target <- step6_target(encode_df, Tables$experiments)
  encode_df$date_released <- step6_date_released(encode_df, Tables$experiments)
  encode_df$status <- step6_status(encode_df, Tables$experiments)
  encode_df$assay <- step6_assay(encode_df, Tables$experiments)
  encode_df$biosample_type <- step6_biosample_type(encode_df, Tables$experiments)
  encode_df$biosample_name <- step6_biosample_name(encode_df, Tables$experiments)
  encode_df$controls <- step6_control(encode_df, Tables$experiments)
  
  encode_df <- cbind(encode_df, organism = empty_vector)
  encode_df$organism <- step7(encode_df, Tables$targets)
  encode_df <- step8(encode_df, Tables$targets)
  encode_df$organism <- step9(encode_df, Tables$organisms)
  # Step 10 : Adjusting file_size
  encode_df <- step10(encode_df)
  encode_df$file_size <- as.character(encode_df$file_size)
  encode_df$submitted_by <- step11(encode_df, Tables$user)
  # Creating and updating new columns for dataset
  empty_vector <- rep(x = NA, times = nrow(encode_df))
  
  encode_df$status <- step6_status(encode_df, Tables$datasets)
  
  remove(Tables)
  #Ordering the table by the accession column
  encode_df <- encode_df[order(accession),]
  
  #reordering the table, we want to have the colunm below as the first colunm
  #to be display fellowed by the rest the remaining column available.
  colNamesList <- c("accession", "file_accession", "file_type", "file_format",
                    "file_size","output_category", "output_type", "target", "investigated_as",
                    "nucleic_acid_term", "assay", "treatment", "biosample_type","biosample_name", 
                    "replicate_library","replicate_antibody", "antibody_target",
                    "antibody_characterization", "antibody_caption", 
                    "organism", "dataset_type", "assembly","status", 'controls', "controlled_by",
                    "lab","run_type", "read_length", "paired_end",
                    "paired_with", "platform", "notes","href", "biological_replicates",
                    "biological_replicate_number","technical_replicate_number","replicate_list")
  encode_df$controls <- gsub(encode_df$controls,
                              pattern = "/.*/(.*)/", replacement = "\\1")
  encode_df$controlled_by <- gsub(encode_df$controlled_by,
                              pattern = "/.*/(.*)/", replacement = "\\1")
  encode_df$replicate_list <- gsub(encode_df$replicate_list ,
                              pattern = "/.*/(.*)/", replacement = "\\1")
  
  colEncode <- colnames(encode_df)
  colEncode <- colEncode[!(colEncode %in% colNamesList)]
  colEncode <- c(colNamesList,colEncode)
  setcolorder(encode_df, colEncode)
  
}

step1 <- function(database_file){
  
  ### Step 1 : fetch all needed data
  all_files <- database_file$file
  all_experiments <- database_file$experiment
  all_datasets <- database_file$dataset
  all_matched_sets <- database_file$matched_set
  all_labs <- database_file$lab
  all_awards <- database_file$award
  all_targets <- database_file$target
  all_biosamples <- database_file$biosample
  all_libraries <- database_file$library
  all_organisms <- database_file$organism
  all_treatments <- database_file$treatment
  all_replicates <- database_file$replicate
  all_platforms <- database_file$platform
  all_user <- database_file$user
  all_antibody <- database_file$antibody_lot
  all_antibody_info <- database_file$antibody_characterization
  
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
    platforms = all_platforms,
    users = all_user,
    antibody = all_antibody,
    antibody_charac = all_antibody_info
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

step4 <- function(files, replicates, libraries, treatments, biosamples,
                  antibody_lot, antibody_charac){
 
  # Updating biological_replicate_list with replicates$biological_replicate_number
  
  match_vector <- match(files$replicate_list, replicates$id)
  match_vector = match_vector[!is.na(match_vector)]
  suppressWarnings(files$biological_replicate_number <- rep(NULL,nrow(files)))
  files[files$replicate_list %in% replicates$id, biological_replicate_number := 
                  as.character(replicates$biological_replicate_number[match_vector])]
  # Creating replicate library column
  suppressWarnings(files$replicate_library <- rep(NULL,nrow(files)))
  files[files$replicate_list %in% replicates$id, replicate_library := 
            as.character(replicates$library[match_vector])]
  # Creating replicate antibody column
  suppressWarnings(files$replicate_antibody <- rep(NULL,nrow(files)))
  files[files$replicate_list %in% replicates$id, replicate_antibody := 
            as.character(replicates$antibody[match_vector])]

  # Creating antibody target
  suppressWarnings(files$antibody_target <- rep(NULL, nrow(files)))
  match_antibody <- match(files$replicate_antibody, antibody_lot$id)
  match_antibody = match_antibody[!is.na(match_antibody)]
  files[files$replicate_antibody %in% antibody_lot$id, antibody_target :=
            as.character(antibody_lot$targets[match_antibody])]
  
  # Creating antibody_characterization
  suppressWarnings(files$antibody_characterization <- rep(NULL, nrow(files)))
  # pasting the ID
  files[files$replicate_antibody %in% antibody_lot$id, antibody_characterization :=
            as.character(antibody_lot$characterizations[match_antibody])]
  
  match_antibodyC <- match(files$antibody_characterization, antibody_charac$id)
  match_antibodyC = match_antibodyC[!is.na(match_antibodyC)]
  # Creating antibody_caption
  suppressWarnings(files$antibody_caption <- rep(NULL, nrow(files)))
  files[files$antibody_characterization %in% antibody_charac$id ,
        antibody_caption := as.character(antibody_charac$caption[match_antibodyC])]
  #updating antibody_characterization
  files[files$antibody_characterization %in% antibody_charac$id ,
        antibody_characterization := antibody_charac$characterization_method[match_antibodyC]]
  
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
  
  #Creating nucleic_acid_term from tables$library$nucleic_acid_term_name
  suppressWarnings(files$nucleic_acid_term <- rep(NULL, nrow(files)))
  match_library <- match(files$replicate_library, libraries$id)
  match_library <- match_library[!is.na(match_library)]
  files[files$replicate_library %in% libraries$id, nucleic_acid_term :=
            libraries$nucleic_acid_term_name[match_library]]
  
  
  files$replicate_library <- gsub(files$replicate_library, pattern = "/.*/(.*)/",
                                  replacement = "\\1")
  files$replicate_antibody <- gsub(files$replicate_antibody,
                                   pattern = "/.*/(.*)/", replacement = "\\1")
  files$antibody_target <- gsub(files$antibody_target,
                                pattern = "/.*/(.*)/", replacement = "\\1")
  return(files)
}

step5 <- function(files){
  # Step 5 : Splitting dataset column into two column
  dataset_types <- gsub(x = files$dataset, pattern = "/(.*)/.*/", 
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
  match_target <- match(encode_exp$target, tables_target$id)
  match_target <- match_target[!is.na(match_target)]
  encode_exp$organism <- as.character(encode_exp$organism)
  encode_exp[encode_exp$target %in% tables_target$id, organism :=
               tables_target$organism[match_target]]

  return(encode_exp$organism)
}

step8 <- function(encode_exp, tables_target){
  
  match_target <- match(encode_exp$target, tables_target$id)
  match_target <- match_target[!is.na(match_target)]
  #Adding investigated_as column comming from Tables$target.
  suppressWarnings(encode_exp$investigated_as <- rep(NULL,(nrow(encode_exp))))
  encode_exp[encode_exp$target %in% tables_target$id, investigated_as :=
                 tables_target$investigated_as[match_target]]
  # Updating target with the label columns of Tables$targets
  encode_exp[encode_exp$target %in% tables_target$id, target :=
               tables_target$label[match_target]]
  return(encode_exp)
}

step9 <- function(encode_exp, tables_org){
  # Updating organism with scientific_name column from Tables$organism
  
  match_org <- match(encode_exp$organism, tables_org$id)
  match_org <- match_org[!is.na(match_org)]
  encode_exp[encode_exp$organism %in% tables_org$id, organism :=
               tables_org$scientific_name[match_org]]
  return (encode_exp$organism)
}

step10 <- function(encode_exp) {
    # Converting the file size from byte to the Kb, Mb or Gb
    encode_exp$file_size <- sapply(encode_exp$file_size, function(size){
        
        if(!(is.na(size))){
            if(size < 1024){
                paste(size,"b") 
            }else if ((size >= 1024) & (size < 1048576)){
                paste(round(size/1024,digits = 1), "Kb")
            }else if ((size >= 1048576) & (size < 1073741824)){
                paste(round(size/(1048576),digits = 1), "Mb")
            }else{
                paste(round(size/1073741824, digits = 2), "Gb")
            }
        }
    })
    encode_exp
    
}

step11 <- function(encode_exp, tables_user){
    #Updating submitted_by column with Tables$user
    match_user <- match(encode_exp$submitted_by, tables_user$id)
    match_user <- match_user[!is.na(match_user)]
    encode_exp[encode_exp$submitted_by %in% tables_user$id, submitted_by :=
                   tables_user$title[match_user]]
    return (encode_exp$submitted_by)
}