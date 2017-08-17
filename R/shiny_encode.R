#' Launch a shiny interface for ENCODExplorer
#'
#' @return None
#'
#' @examples
#'  \dontrun{shinyEncode}
#'
#' @export
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom shiny actionButton
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny column
#' @importFrom shiny conditionalPanel
#' @importFrom shiny eventReactive
#' @importFrom shiny fileInput
#' @importFrom shiny fluidPage
#' @importFrom shiny fluidRow
#' @importFrom shiny navbarPage
#' @importFrom shiny numericInput
#' @importFrom shiny mainPanel
#' @importFrom shiny observeEvent
#' @importFrom shiny outputOptions
#' @importFrom shiny radioButtons
#' @importFrom shiny reactive
#' @importFrom shiny renderPrint
#' @importFrom shiny renderUI
#' @importFrom shiny selectInput
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny shinyApp
#' @importFrom shiny tabPanel
#' @importFrom shiny tagList
#' @importFrom shiny textInput
#' @importFrom shiny uiOutput
#' @importFrom shiny verbatimTextOutput
#' @importFrom shinythemes shinytheme
#'
#' @export
shinyEncode <- function(){
  source(file = system.file("shiny/ui.R", package = "ENCODExplorer"))
  source(file = system.file("shiny/server.R", package = "ENCODExplorer"))
    runApp(appDir = shinyApp(ui,server))
}
