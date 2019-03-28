#' Launch a shiny interface for ENCODExplorer
#'
#' @return None
#'
#' @examples
#'  \dontrun{shinyEncode}
#'
#' @export
#' @import shiny
#' @import shinythemes
#'
#' @export
shinyEncode <- function(){
  source(file = system.file("shiny/ui.R", package = "ENCODExplorer"))
  source(file = system.file("shiny/server.R", package = "ENCODExplorer"))
    runApp(appDir = shinyApp(ui,server))
}
