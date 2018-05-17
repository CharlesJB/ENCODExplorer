
#' Create the list of data.table for the tables in ENCODE
#' 
#' @return is a \code{list} with selected tables from ENCODE.
#'
#' @param database_filename The name of the file to save the database into.
#' @param types The names of the tables to extract using the ENCODE rest api.
#' @param overwrite If database_filename already exists, should it be overwritten?
#'   Default: \code{FALSE}.
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
                   "Please delete it or set overwrite = TRUE before re-running the data preparation"))
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

#' Extract file metadata from the full set of ENCODE metadata tables.
#'
#' @return a \code{data.table} containing relevant metadata for all
#'   ENCODE files.
#'
#' @param database_filename A list of ENCODE metadata tables as loaded by
#'   prepare_ENCODEdb.
#'
#' @examples
#'     \dontrun{
#'         tables = prepare_ENCODEdb()
#'         export_ENCODEdb_matrix(database_filename = tables)
#'     }
#' @import parallel
#' 
#' @export
export_ENCODEdb_matrix <- function(database_filename) {
  db = database_filename
  encode_df = db$file
  
  # Step 2 : renaming specific column.
  encode_df <- rename_file_columns(encode_df)

  # Step 3 : updating project, paired-with, platform and lab column.
  encode_df <- update_project_platform_lab(files = encode_df, awards = db$award, 
                                           labs = db$lab, platforms = db$platform)

  # Step 4 : Updating replicate_list and treatment column.
  encode_df <- update_replicate_treatment(files = encode_df,
                                          replicates = db$replicate, 
                                          libraries = db$library, 
                                          treatments = db$treatment,
                                          biosamples = db$biosample,
                                          antibody_lot = db$antibody_lot,
                                          antibody_charac = db$antibody_characterization)
  
  encode_df <- split_dataset_column(files = encode_df)
  
  # Step 6 to step 9: Creating new columns
  exp_colmap = c("target", "date_released", "status", "assay"="assay_title", "biosample_type",
                 "biosample_name"="biosample_term_name", "controls"="possible_controls")
  encode_df = pull_columns_append(encode_df, db$experiment, "accession", "accession", exp_colmap)
  
  # Step 7
  encode_df$organism <- pull_column(encode_df, db$target, "target", "id", "organism")
  
  # Step 8
  encode_df$investigated_as = pull_column(encode_df, db$target, "target", "id", "investigated_as")                 
  encode_df$target = pull_column_merge(encode_df, db$target, "target", "id", "label", "target")

  # Step 9
  encode_df$organism <- pull_column_merge(encode_df, db$organism, "organism", "id", "scientific_name", "organism")
  
  # Step 10 : Adjusting file_size
  encode_df <- file_size_conversion(encode_df)
  encode_df$file_size <- as.character(encode_df$file_size)
  
  # Step 11
  encode_df$submitted_by <- pull_column_merge(encode_df, db$user, "submitted_by", "id", "title", "submitted_by")
  
  # Creating and updating new columns for dataset
  encode_df$status <- pull_column_merge(encode_df, db$dataset, "accession", "accession", "status", "status")
  
  #Ordering the table by the accession column
  encode_df <- encode_df[order(accession),]
  
  #reordering the table, we want to have the column below as the first column
  #to be display fellowed by the rest the remaining column available.
  colNamesList <- c("accession", "file_accession", "file_type", "file_format",
                    "file_size", "output_category", "output_type", "target", "investigated_as",
                    "nucleic_acid_term", "assay", "treatment", "treatment_amount",
                    "treatment_amount_unit", "treatment_duration", "treatment_duration_unit",
                    "biosample_type", "biosample_name", 
                    "replicate_library", "replicate_antibody", "antibody_target",
                    "antibody_characterization", "antibody_caption", 
                    "organism", "dataset_type", "assembly","status", 'controls', "controlled_by",
                    "lab","run_type", "read_length", "paired_end",
                    "paired_with", "platform", "notes", "href", "biological_replicates",
                    "biological_replicate_number","technical_replicate_number","replicate_list")
  encode_df$controls <- remove_id_prefix(encode_df$controls)
  encode_df$controlled_by <- remove_id_prefix(encode_df$controlled_by)
  encode_df$replicate_list <- remove_id_prefix(encode_df$replicate_list)
  
  colEncode <- colnames(encode_df)
  colEncode <- colEncode[!(colEncode %in% colNamesList)]
  colEncode <- c(colNamesList,colEncode)
  data.table::setcolorder(encode_df, colEncode)
  
}

### Step 2 : renaming
rename_file_columns <- function(files){
  names(files)[names(files) == 'status'] <- 'file_status'
  names(files)[names(files) == 'accession'] <- 'file_accession'
  names(files)[names(files) == 'award'] <- 'project'
  names(files)[names(files) == 'replicate'] <- 'replicate_list'
  
  return(files)
}

pull_column_no_prefix <- function(table1, table2, id1, id2, pull_value, prefix_value) {
    pulled_val = pull_column(table1, table2, id1, id2, pull_value)
    no_prefix = remove_id_prefix(table1[[prefix_value]])
    return(ifelse(is.na(pulled_val), no_prefix, pulled_val))
}

update_project_platform_lab <- function(files, awards, labs, platforms){
  # Updating files$project with awards$project
  files$project = pull_column_no_prefix(files, awards, "project", "id", "project", "project")
  
  # Updating files$paired_with
  files$paired_with <- remove_id_prefix(files$paired_with)
  
  # Updating files$platform with platform$title
  files$platform = pull_column_no_prefix(files, platforms, "platform", "id", "title", "platform")
  
  # Updating files$lab with labs$title
  files$lab = pull_column_no_prefix(files, labs, "lab", "id", "title", "lab")

  return(files)
}

# Matches the entries of table1 to table2, using id1 and id2, then returns
# the values from pulled_column in table2.
pull_column <- function(table1, table2, id1, id2, pulled_column) {
  return(table2[[pulled_column]][match(table1[[id1]], table2[[id2]])])
}

# Matches the entries of table1 to table2, using id1 and id2, then returns
# a merged vector containing the values from pulled_column in table2 when
# a match exists, or table1$updated_value if it does not.
pull_column_merge <- function(table1, table2, id1, id2, pulled_column, updated_value) {
  retval = pull_column(table1, table2, id1, id2, pulled_column)
  retval = ifelse(is.na(match(table1[[id1]], table2[[id2]])), table1[[updated_value]], retval)
  return(retval)
}

# Remove the type prefix from ENCODE URL-like identifiers.
# Example: /files/ENC09345TXW/ becomes ENC09345TXW.
remove_id_prefix <- function(ids) {
    return(gsub("/.*/(.*)/", "\\1", ids))
}

# Matches the entries of table1 to table2, using id1 and id2,
# then creates a new data.table from the column pairings described in
# value_pairs. Ex: c("antibody_target"="target") will create a column
# named "antibody_target" from table2$target. (Similar to dplyr::*_join)
pull_columns <- function(table1, table2, id1, id2, value_pairs) {
    retval <- NULL
    for(i in 1:length(value_pairs)) {
        value_name = value_pairs[i]
        out_name = ifelse(is.null(names(value_pairs)), value_name, names(value_pairs)[i])
        out_name = ifelse(out_name=="", value_name, out_name)
        if(is.null(retval)) {
            retval = data.table::data.table(pull_column(table1, table2, id1, id2, value_name))
            colnames(retval) = out_name
        } else {
            retval[[out_name]] = pull_column(table1, table2, id1, id2, value_name)
        }
    }
    return(retval)
}

# Calls pull_columns, and append the results to table1.
pull_columns_append <- function(table1, table2, id1, id2, value_pairs) {
    pulled_columns = pull_columns(table1, table2, id1, id2, value_pairs)
    return(cbind(table1, pulled_columns))
}

update_replicate_treatment <- function(files, replicates, libraries, treatments, biosamples,
                                       antibody_lot, antibody_charac){
 
  # Updating biological_replicate_list with replicates$biological_replicate_number
  replicate_col_map = c("biological_replicate_number", "replicate_library"="library",
                        "replicate_antibody"="antibody","technical_replicate_number")
  files = pull_columns_append(files, replicates, "replicate_list", "id", replicate_col_map)
  
  # Creating antibody target
  antibody_col_map = c("antibody_target"="targets", "antibody_characterization"="characterizations")
  files = pull_columns_append(files, antibody_lot, "replicate_antibody", "id", antibody_col_map)
  
  files$antibody_caption = pull_column(files, antibody_charac, "antibody_characterization", "id", "caption")
  files$antibody_characterization = pull_column_merge(files, antibody_charac, "antibody_characterization", "id", "characterization_method", "antibody_characterization")
  
  files$treatment = files$replicate_library
  files$treatment = pull_column_merge(files, libraries, "treatment", "id", "biosample", "treatment")
  files$treatment = pull_column_merge(files, biosamples, "treatment", "id", "treatments", "treatment")
  files$treatment = pull_column_merge(files, treatments, "treatment", "id", "treatment_term_name", "treatment")
  
  files$treatment_id = files$replicate_library
  files$treatment_id = pull_column(files, libraries, "treatment", "id", "biosample")
  files$treatment_id = pull_column_merge(files, biosamples, "treatment", "id", "treatments", "treatment")

  treatment_col_map = c("treatment_amount"="amount", "treatment_amount_unit"="amount_units", 
                        "treatment_duration"="duration", "treatment_duration_unit"="duration_units")
  files = pull_columns_append(files, treatments, "treatment_id", "id", treatment_col_map)
  
  files$nucleic_acid_term = pull_column(files, libraries, "replicate_library", "id", "nucleic_acid_term_name")
  
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