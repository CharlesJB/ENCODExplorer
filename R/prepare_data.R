#' Create the RSQLite databse for the tables in ENCODE
#'
#' @return a \code{list} with selected tables from ENCODE that were used to
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
}

#' A list of known tables from ENCODE database.
#'
#' The list was extracted from:
#'   https://github.com/ENCODE-DCC/encoded/tree/master/src/encoded/schemas
#'
#' @return a vector of \code{character} with the names of the known tables in
#'   the ENCODE database.
#'
#' @examples
#'   types <- get_encode_types()
#'
#'  @export
# TODO: find a way to fetch this info from the github
get_encode_types <- function() {
  c("access_key",
    "analysis_step",
    "analysis_step_run",
    "antibody_approval",
    "antibody_characterization",
    "antibody_lot",
    "award",
    "biosample",
    "biosample_characterization",
    "characterization",
    "construct",
    "construct_characterization",
    "dataset",
    "document",
    "donor",
    "donor_characterization",
    "experiment",
    "file",
    "fly_donor",
    "human_donor",
    "image",
    "lab",
    "library",
    "mixins",
    "mouse_donor",
    "namespaces",
    "organism",
    "page",
    "pipeline",
    "platform",
    "publication",
    "quality_metric",
    "replicate",
    "rnai",
    "rnai_characterization",
    "software",
    "software_version",
    "source",
    "talen",
    "target",
    "treatment",
    "user",
    "workflow_run",
    "worm_donor")
}
