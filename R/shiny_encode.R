
shinyEncode <- function(){
  browser_p <- "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
  source(file = system.file("inst/shiny/ui.R", package = "ENCODExplorer"))
  source(file = system.file("shiny/server.R", package = "ENCODExplorer"))
  options(browser = getOption("browser"))
  runApp(appDir = shinyApp(ui,server),host="127.0.0.1",port=8888, launch.browser = T)
  
}