
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
prepare_ENCODEdb <- function(database_filename = "tables.RDA",
                             types = get_encode_types(), overwrite = FALSE) {
  if(file.exists(database_filename) && !overwrite) {
    warning(paste0("The file ", database_filename, " already exists and will not be overwritten.\n",
                   "Please delete or set overwrite = TRUE before re-running the data preparation"))
    NULL
  } else {
    # Extract the tables from the ENCODE rest api
    extract_type <- function(type) {
      cat("Extracting table", type, "\n")
      table <- extract_table(type)
      cat("Cleaning table", type, "\n")
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
      warning(paste0("Something went wrong during data preparation. ",
                     "Please erase the database ", database_filename, " and re-run the whole process.",
                     "If the problem persists, please contact us"))
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
#'     database_filename <- system.file("extdata/tables",package = "ENCODExplorer")
#'     \dontrun{
#'         export_ENCODEdb_matrix(database_filename = database_filename)
#'     }
#' @import parallel
#' 
#' @export
export_ENCODEdb_matrix <- function(database_filename) {
  # Step 1 : fetch all needed data.
  Tables <- fetch_tables(database_file = database_filename)

  # Step 2 : renaming specific column.
  Tables$files <- rename_file_columns(files = Tables$files)

  # Step 3 : updating project, paired-with, platform and lab column.
  Tables$files <- update_project_platform_lab(files = Tables$files, awards = Tables$awards, 
                                              labs = Tables$labs, platforms = Tables$platforms)

  # Step 4 : Updating replicate_list and treatment column.
  Tables$files <- update_replicate_treatment(files = Tables$files,
                                             replicates = Tables$replicates, 
                                             libraries = Tables$libraries, 
                                             treatments = Tables$treatments,
                                             biosamples = Tables$biosamples,
                                             antibody_lot = Tables$antibody,
                                             antibody_charac = Tables$antibody_charac)
  
  Tables$files <- split_dataset_column(files = Tables$files)
  
  encode_df <- Tables$files
  
  # Step 6 to step 9: Creating new columns
  encode_df$target <-   file_match(encode_df, Tables$experiments, "accession", "accession", "target")
  encode_df$date_released <- file_match(encode_df, Tables$experiments, "accession", "accession", "date_released")
  encode_df$status <- file_match(encode_df, Tables$experiments, "accession", "accession", "status")
  encode_df$assay <- file_match(encode_df, Tables$experiments, "accession", "accession", "assay_title")
  encode_df$biosample_type <- file_match(encode_df, Tables$experiments, "accession", "accession", "biosample_type")
  encode_df$biosample_name <- file_match(encode_df, Tables$experiments, "accession", "accession", "biosample_term_name")
  encode_df$controls <- file_match(encode_df, Tables$experiments, "accession", "accession", "possible_controls")
  
  # Step 7
  encode_df$organism <- file_match(encode_df, Tables$targets, "target", "id", "organism")
  
  # Step 8
  encode_df$investigated_as = file_match(encode_df, Tables$targets, "target", "id", "investigated_as")                 
  encode_df$target = file_match_update(encode_df, Tables$targets, "target", "id", "label", "target")

  # Step 9
  encode_df$organism <- file_match_update(encode_df, Tables$organisms, "organism", "id", "scientific_name", "organism")
  
  # Step 10 : Adjusting file_size
  encode_df <- file_size_conversion(encode_df)
  encode_df$file_size <- as.character(encode_df$file_size)
  
  # Step 11
  encode_df$submitted_by <- file_match_update(encode_df, Tables$users, "submitted_by", "id", "title", "submitted_by")
  
  # Creating and updating new columns for dataset
  encode_df$status <- file_match_update(encode_df, Tables$datasets, "accession", "accession", "status", "status")
  
  remove(Tables)
  #Ordering the table by the accession column
  encode_df <- encode_df[order(accession),]
  
  #reordering the table, we want to have the column below as the first column
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
  encode_df$controls <- remove_id_prefix(encode_df$controls)
  encode_df$controlled_by <- remove_id_prefix(encode_df$controlled_by)
  encode_df$replicate_list <- remove_id_prefix(encode_df$replicate_list)
  
  colEncode <- colnames(encode_df)
  colEncode <- colEncode[!(colEncode %in% colNamesList)]
  colEncode <- c(colNamesList,colEncode)
  data.table::setcolorder(encode_df, colEncode)
  
}

### Step 1 : fetch all needed data
fetch_tables <- function(database_file){
  return(list(
    files = database_file$file,
    experiments = database_file$experiment,
    datasets = database_file$dataset,
    matched_sets = database_file$matched_set,
    labs = database_file$lab,
    awards = database_file$award,
    targets = database_file$target,
    biosamples = database_file$biosample,
    libraries = database_file$library,
    organisms = database_file$organism,
    treatments = database_file$treatment,
    replicates = database_file$replicate,
    platforms = database_file$platform,
    users = database_file$user,
    antibody = database_file$antibody_lot,
    antibody_charac = database_file$antibody_characterization
  ))
}
### Step 2 : renaming
rename_file_columns <- function(files){
  names(files)[names(files) == 'status'] <- 'file_status'
  names(files)[names(files) == 'accession'] <- 'file_accession'
  names(files)[names(files) == 'award'] <- 'project'
  names(files)[names(files) == 'replicate'] <- 'replicate_list'
  
  return(files)
}

update_project_platform_lab <- function(files, awards, labs, platforms){
  # Updating files$project with awards$project
  project = file_match(files, awards, "project", "id", "project")
  no_prefix = remove_id_prefix(files$project)
  files$project = ifelse(is.na(project), no_prefix, project)
  
  # Updating files$paired_with
  files$paired_with <- gsub(x = files$paired_with, pattern = "/files/(.*)/", 
                            replacement = '\\1')
  
  # Updating files$platform with platform$title
  platform = file_match(files, platforms, "platform", "id", "title")
  no_prefix = remove_id_prefix(files$platform)
  files$platform = ifelse(is.na(platform), no_prefix, platform)
  
  # Updating files$lab with labs$title
  lab = file_match(files, labs, "lab", "id", "title")
  no_prefix = remove_id_prefix(files$lab)
  files$lab = ifelse(is.na(lab), no_prefix, lab)

  return(files)
}

file_match <- function(files, table2, file_id, table_id, table_value) {
  return(table2[[table_value]][match(files[[file_id]], table2[[table_id]])])
}

file_match_c <- function(files, table2, file_id, table_id, table_value) {
  return(as.character(file_match(files, table2, file_id, table_id, table_value)))
}

file_match_update <- function(files, table2, file_id, table_id, table_value, file_value) {
  retval = file_match(files, table2, file_id, table_id, table_value)
  retval = ifelse(is.na(match(files[[file_id]], table2[[table_id]])), files[[file_value]], retval)
  return(retval)
}

file_match_update_c <- function(files, table2, file_id, table_id, table_value, file_value) {
  return(as.character(file_match_update(files, table2, file_id, table_id, table_value, file_value)))  
}

remove_id_prefix <- function(ids) {
    return(gsub("/.*/(.*)/", "\\1", ids))
}

update_replicate_treatment <- function(files, replicates, libraries, treatments, biosamples,
                                       antibody_lot, antibody_charac){
 
  # Updating biological_replicate_list with replicates$biological_replicate_number
  files$biological_replicate_number = file_match_c(files, replicates, "replicate_list", "id", "biological_replicate_number")
  files$replicate_library = file_match_c(files, replicates, "replicate_list", "id", "library")
  files$replicate_antibody = file_match_c(files, replicates, "replicate_list", "id", "antibody")
  files$technical_replicate_number = file_match(files, replicates, "replicate_list", "id", "technical_replicate_number")
  
  # Creating antibody target
  files$antibody_target = file_match_c(files, antibody_lot, "replicate_antibody", "id", "targets")
  files$antibody_characterization = file_match_c(files, antibody_lot, "replicate_antibody", "id", "characterizations")

  files$antibody_caption = file_match_c(files, antibody_charac, "antibody_characterization", "id", "caption")
  files$antibody_characterization = file_match_update_c(files, antibody_charac, "antibody_characterization", "id", "characterization_method", "antibody_characterization")
  
  files$treatment = files$replicate_library
  files$treatment = file_match_update(files, libraries, "treatment", "id", "biosample", "treatment")
  files$treatment = file_match_update(files, biosamples, "treatment", "id", "treatments", "treatment")
  files$treatment = file_match_update(files, treatments, "treatment", "id", "treatment_term_name", "treatment")
  
  files$nucleic_acid_term = file_match(files, libraries, "replicate_library", "id", "nucleic_acid_term_name")
  
  # Remove ID prefixes
  files$replicate_library <- remove_id_prefix(files$replicate_library)
  files$replicate_antibody <- remove_id_prefix(files$replicate_antibody)
  files$antibody_target <- remove_id_prefix(files$antibody_target)
  
  return(files)
}

split_dataset_column <- function(files){
  # Step 5 : Splitting dataset column into two column
  dataset_types <- gsub(x = files$dataset, pattern = "/(.*)/.*/", 
                        replacement = "\\1")
  dataset_accessions <- gsub(x = files$dataset, pattern = "/.*/(.*)/", 
                             replacement = "\\1")
  
  files <- cbind(accession = dataset_accessions, dataset_type = dataset_types, 
                 files)
  
  return(files)
}

file_size_conversion <- function(encode_exp) {
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