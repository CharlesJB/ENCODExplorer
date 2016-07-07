

shinyEncode <- function(){
  source(file = system.file("inst/shiny/ui.R", package = "ENCODExplorer"))
  source(file = system.file("shiny/server.R", package = "ENCODExplorer"))
  
  shinyApp(ui,server)
}