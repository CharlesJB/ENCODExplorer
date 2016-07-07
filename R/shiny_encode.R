shinyEncode <- function(){
  load(file = system.file("shiny/ui.rda", package = "ENCODExplorer"))
  load(file = system.file("shiny/server.rda", package = "ENCODExplorer"))
  shinyApp(ui,server)
}