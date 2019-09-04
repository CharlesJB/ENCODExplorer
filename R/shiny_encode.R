#' Launch a shiny interface for ENCODExplorer
#'
#' @return None
#'
#' @examples
#'  \dontrun{shinyEncode}
#'
#' @export
shinyEncode <- function(){
  if(!(requireNamespace("shiny") && 
       requireNamespace("shinythemes") &&
       requireNamespace("DT"))) {
    stop("shinyEncode requires the shiny, shinythemes and DT packages.\n",
         'Please install the packages by running ',
         'install.packages(c("shiny", "shinyThemes", "DT")) and try again.')
  }

  source(file = system.file("shiny/ui.R", package = "ENCODExplorer"))
  source(file = system.file("shiny/server.R", package = "ENCODExplorer"))
    shiny::runApp(appDir = shiny::shinyApp(ui,server))
}
