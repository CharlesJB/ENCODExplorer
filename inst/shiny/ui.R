#' @import shiny
#' @import shinythemes
#' @import DT

library(shiny)
library(shinythemes)
library(DT)
ui <- fluidPage(theme = shinytheme("cerulean"),
        navbarPage("ENCODExplorer",
#////----------------------------fuzzySearch-------------------------------////
        tabPanel("Search",sidebarLayout(
            sidebarPanel(
                    actionButton("searchAction", "Search"),
                    #fileInput("df", "Import your database (optional)"),
                    radioButtons("typeInput","Select your type of search",
                        choices = list("Single element"=1, "Mutiple element"=2),
                        selected=1),
                    textInput("searchTerm", "Searching for ...", value=
                              "Seperate term with a comma"),
                    checkboxGroupInput("filter","Select your filter...",
                        choices = list("Accession"=1, "File Accession"=13, "Dataset type"=2,
                                    "Target"=14,"Lab"=3, "Title"=4, "File Type"=6, "Platform"=16,
                                    "Project"=7, "Type"=8, "Control"=9, "Biosample Type"=10,
                                    "Biosample Name"=16,"Replicate"=11, "Organism"=12,
                                    "Lab"=3,"Assay"=15,"File format"=5)),
                    width = 3),
            mainPanel(
                conditionalPanel(
                    condition = "input.searchAction > 0",
                    fluidRow(column(3,actionButton("designFromSearch",
                                                   "Create a design")),
                            column(5, checkboxInput("splitFromSearch",
                                "Split the result per experiment", value=FALSE)),
                            column(4, radioButtons("formatFromSearch", "Design format",
                                choices=list("long"=1, "wide"=2 ),inline=TRUE,
                                selected=1))),
                    fluidRow(column(3, selectInput("fileFromSearch", "File Format",
                                choices=list(
                                "bam"=1, "fastq"=2, "sam"=3, "bed"=4,
                                "bigBed"=5, "bigWig"=6))),
                            column(3, selectInput("datatypeFromSearch","Data Type",
                                choices=list(
                                "experiments"=1,"ucsc browser composite"=2,
                                "annotations"=3,"matched sets"=4, "projects"=5,
                                "reference epigenomes"=6,"references"=7)
                                )),
                            column(3,numericInput("repFromSearch",
                                "Replicate ID",
                                value=1)),
                            column(3,numericInput("ctrlFromSearch",
                                "Control ID",
                                value=2))),
                    fluidRow(column(3, actionButton("downloadFromSearch", "Download")),
                             column(9, verbatimTextOutput("consoleSearch"))),
                    textOutput("fileSizeFuzzy")),
                
                
                conditionalPanel(
                    condition="!output.designVis",
                    tags$head(tags$style(".table .alignCenter {text-align:center;}")),
                    DT::dataTableOutput("searchFuzzy")),
                
                conditionalPanel(
                    condition="output.designVis & !input.splitFromSearch",
                    DT::dataTableOutput("designFuzzy")),

                conditionalPanel(
                    condition="output.designVis & input.splitFromSearch",
                    uiOutput("designSplitFuzzy"))
                )
            )
        ),
                           
#////------------------------------queryEncode-----------------------------////
        tabPanel("Advanced Search",sidebarLayout(
            sidebarPanel(
                actionButton("searchAdvanced", "Search"),
                fileInput("df", "Import your database (optional)"),
                checkboxInput("fixed", "Fixed Search", value=FALSE),
                textInput("setAccession", "Set Accession"),
                textInput("assay", "Assay"),
                textInput("biosampleName", "Biosample Name"),
                textInput("biosampleType", "Biosample Type"),
                textInput("datasetAccession", "Dataset Accession",value=NULL),
                textInput("fileAccession", "File Accession"),
                textInput("fileFormat", "File Format"),
                textInput("lab", "Lab"),
                textInput("organism", "Organism"),
                textInput("target", "Target"),
                textInput("treatment", "Treatment"),
                textInput("project", "Project"),
                selectInput("fileStatus", "FileStatus", choices = list(
                            "released"=1, "revoked"=2, "All"=3),selected=1),
                textInput("status", "Status", value="released"),
                width=3),
            
            mainPanel(
                conditionalPanel(
                    condition = "input.searchAdvanced > 0",
                    fluidRow(column(3, actionButton("designFromQuery",
                                                   "Create a design")),
                            column(5, checkboxInput("splitFromQuery",
                                  "Split the result per experiment", value=FALSE)),
                            column(4, radioButtons("formatFromQuery", "Design format",
                                 choices =list("long"=1, "wide"=2 ), selected=1))),
                    fluidRow(column(3, selectInput("fileFromQuery", "File Format",
                                choices = list("bam"=1, "fastq"=2,
                                        "sam"=3, "bed"=4,"bigBed"=5,"bigWig"=6))),
                            column(3, selectInput("datatypeFromQuery","Data Type",
                                choices =list("experiments"=1,"ucsc browser composite"=2,
                                "annotations"=3,"matched sets"=4, "projects"=5,
                                "reference epigenomes"=6,"references"=7))),
                            column(3, numericInput("repFromQuery","Replicate ID",
                                                   value=1)),
                            column(3, numericInput("ctrlFromQuery",
                              "Control ID", value=2))),
                    fluidRow(column(3,actionButton("downloadFromQuery", "Download")),
                            column(9,verbatimTextOutput("consoleQuery"))),
                    textOutput("fileSizeQuery")),
                
                    conditionalPanel(
                        condition="!output.designVis",
                        tags$head(tags$style(".table .alignCenter {text-align:center;}")),
                        DT::dataTableOutput("resultQuery")),
                
                    conditionalPanel(
                        condition="output.designVis & !input.splitFromQuery",
                        DT::dataTableOutput("designQuery")),
                
                    conditionalPanel(
                        condition="output.designVis & input.splitFromQuery",
                        uiOutput("designSplitQuery")))
        )),
                           
#////-----------------------------createDesign-----------------------------////
                           
        tabPanel("Design",sidebarLayout(
             sidebarPanel(
                actionButton("designFromDesign", "Create a design"),
                fileInput("inputTable", "Import your file as rds format"),
                fileInput("df", "Import your database (optional)"),
                checkboxInput("splitFromDesign", 
                              "Split the result per experiment",value=FALSE),
                radioButtons("formatFromDesign", "Design format", choices =list(
                             "long"=1, "wide"=2  ), selected=1),
                numericInput("repFromDesign", "Replicate ID", value=1),
                numericInput("ctrlFromDesign", "Control ID", value=2),
                selectInput("fileFromDesign", "File Format", choices = list(
                            "bam"=1, "fastq"=2, "sam"=3, "bed"=4,
                            "bigBed"=5,"bigWig"=6)),
                selectInput("datatypeFromDesign","Data Type", choices =list(
                            "experiments"=1,"ucsc browser composite"=2, 
                            "annotations"=3,"matched sets"=4, "projects"=5,
                            "reference epigenomes"=6,"references"=7))),
            mainPanel(DT::dataTableOutput("design")))
        )
                           
    )
)