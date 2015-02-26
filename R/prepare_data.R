#' Create the RSQLite databse for the tables in ENCODE
#'
#' The following table are extracted by default:
#' \describe{
#'   \item{biosample}{}
#'   \item{experiment}{}
#'   \item{lab}{}
#'   \item{dataset}{}
#'   \item{files}{}
#'   \item{target}{}
#'   \item{organism}{}
#'   \item{replicate}{}
#'   \item{award}{}
#'   \item{treatment}{}
#'   \item{human_donor}{}
#'   \item{library}{}
#'   \item{platform}{}
#'   \item{document}{}
#' }
#'
#' @return a \code{list} with selected tables from ENCODE that were used to
#' create the \code{RSQLite} database.
#'
#' @param database_filename The name of the file to save the database into.
#' Default: \code{\"ENCODEdb.sqlite\"}.
#' 
#' @param type The tables to extract from ENCODE rest api.
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
                             types = c("biosample","experiment", "lab",
                                       "dataset", "file", "target", "organism",
                                       "replicate", "award", "treatment",
                                       "human_donor", "library", "platform",
                                       "document"),
                             overwrite = FALSE) {
  # Extract the tables from the ENCODE rest api
  extract_type <- function(type) {
    table <- extract_table(type)
    table <- clean_table(table)
    dbWriteTable(con, type, table, overwrite = overwrite)
    table
  }
  con <- dbConnect(RSQLite::SQLite(), database_filename)  
  tables <- lapply(types, extract_type)
  dbDisconnect(con)
  
  # Return the named tables
  names(tables) <- types
  tables
}
