full_db_cache_env=new.env()

#' Concatenates all available file metadata into a single data table.
#'
#' @return a \code{data.table} containing relevant metadata for all
#'   ENCODE files.
#'
#' @examples
#'     my_full_encode_df = get_encode_df_full()
#'
#' @import AnnotationHub
#'
#' @export
get_encode_df_full <- function() {
    if(!exists("full_db_cache", envir=full_db_cache_env)) {
        ah = AnnotationHub()
        full_db = query(ah, "ENCODExplorerData")[["AH75132"]]
        assign("full_db_cache", full_db, envir=full_db_cache_env)
    }
    return(get("full_db_cache", envir=full_db_cache_env))
}

#' Returns a "light" version of ENCODE file metadata.
#'
#' @return a \code{data.table} containing the most relevant
#'   metadata for all ENCODE files.
#'
#' @examples
#'     my_encode_df = get_encode_df()
#'
#' @import AnnotationHub
#'
#' @export
get_encode_df <- function() {
    if(!exists("light_db_cache", envir=full_db_cache_env)) {
	ah = AnnotationHub()
	light_db = query(ah, "ENCODExplorerData")[["AH75131"]]
        assign("light_db_cache", light_db, envir=full_db_cache_env)
    }
    return(get("light_db_cache", envir=full_db_cache_env))
}
