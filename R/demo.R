#' Get a demo encode_df
#'
#' @return A vector of regions filenames
#'
#' @examples
#'
#' encode_df_demo <- get_encode_df_demo()
#'
#' @import data.table
#'
#' @export
get_encode_df_demo <- function() {
    filename <- system.file("extdata/encode_df_demo.csv", package = "ENCODExplorer")
    fread(filename)
}
