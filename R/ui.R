library(shiny)
library(shinythemes)
ui <- fluidPage(theme = shinytheme("cerulean"),
        navbarPage("ENCODExplorer",
#////----------------------------fuzzySearch-------------------------------////
        tabPanel("Search",sidebarLayout(
            sidebarPanel(
                    actionButton("searchAction", "Search"),
                    fileInput("df", "Import your database (optional"),
                    radioButtons("typeInput","Select your type of search",
                        choices = list("Single element"=1, "Mutiple element"=2),
                        selected=1),
                    textInput("searchTerm", "Searching for ...", value=
                              "Seperate term with a comma"),
                    checkboxGroupInput("filter","Select your filter...",
                        choices = list("Accession"=1, "Dataset type"=2,
                                    "Lab"=3, "Title"=4, "File Type"=5, "Platform"=6,
                                    "Project"=7, "Type"=8, "Control"=9, "Biosample Type"=10,
                                    "Biosample Name"=16,"Replicate"=11, "Organism"=12,
                                    "File Accession"=13,"Target"=14,"Assay"=15,
                                    "File format"=17))),
            mainPanel(
                conditionalPanel(
                    condition = "input.searchAction > 0",
                    actionButton("designFromSearch", "Create a design"),
                    checkboxInput("splitFromSearch",
                                "Split the result per experiment", value=FALSE),
                    radioButtons("formatFromSearch", "Design format",
                                choices=list("long"=1, "wide"=2 ),
                                selected=1),
                    selectInput("fileFromSearch", "File Format", choices=list(
                                "bam"=1, "fastq"=2, "fasta"=3, "sam"=4, "bed"=5,
                                "bigBed"=6,"bigWig"=7)),
                    selectInput("datatypeFromSearch","Data Type", choices=list(
                                "experiments"=1,"ucsc browser composite"=2,
                                "annotations"=3,"matched sets"=4, "projects"=5,
                                "reference epigenomes"=6,"references"=7)),
                    textInput("repFromSearch",
                            "Numeric ID assigned to replicate file",value="1"),
                    textInput("ctrlFromSearch",
                            "Numeric ID assigned to control file", value="2")),
                
                conditionalPanel(
                    condition="!output.designVis",
                    dataTableOutput("searchResult")),
                
                conditionalPanel(
                    condition="output.designVis & !input.splitFromSearch",
                    dataTableOutput("designResultSearch")),

                conditionalPanel(
                    condition="output.designVis & input.splitFromSearch",
                    uiOutput("designSplitSearch"))
                ))),
                           
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
                textInput("status", "Status", value="released")),
            
            mainPanel(
                conditionalPanel(
                    condition = "input.searchAdvanced > 0",
                    actionButton("designFromQuery", "Create a design"),
                    checkboxInput("splitFromQuery",
                                  "Split the result per experiment", value=FALSE),
                    radioButtons("formatFromQuery", "Design format",
                                 choices =list("long"=1, "wide"=2 ), selected=1),
                    selectInput("fileFromQuery", "File Format", choices = list(
                                "bam"=1, "fastq"=2, "fasta"=3, "sam"=4, "bed"=5,
                                "bigBed"=6,"bigWig"=7)),
                    selectInput("datatypeFromQuery","Data Type", choices =list(
                                "experiments"=1,"ucsc browser composite"=2,
                                "annotations"=3,"matched sets"=4, "projects"=5,
                                "reference epigenomes"=6,"references"=7)),
                    textInput("repFromQuery",
                              "Numeric ID assigned to replicate file", value="1"),
                    textInput("ctrlFromQuery",
                              "Numeric ID assigned to control file", value="2")),
                
                    conditionalPanel(
                        condition="!output.designVis",
                        dataTableOutput("advancedResult")),
                
                    conditionalPanel(
                        condition="output.designVis & !input.splitFromQuery",
                        dataTableOutput("designAdvanced")),
                
                    conditionalPanel(
                        condition="output.designVis & input.splitFromQuery",
                        uiOutput("designSplitQuery")))
        )),
                           
#////-----------------------------createDesign-----------------------------////
                           
        tabPanel("Design",sidebarLayout(
             sidebarPanel(
                actionButton("designFromDesign", "Create a design"),
                fileInput("searchTable", "Import your file as rds format"),
                fileInput("df", "Import your database (optional)"),
                checkboxInput("splitFromDesign", 
                              "Split the result per experiment",value=FALSE),
                radioButtons("formatFromDesign", "Design format", choices =list(
                             "long"=1, "wide"=2 ), selected=1),
                textInput("repFromDesign","Numeric ID assigned to replicate file",value="1"),
                textInput("ctrlFromDesign","Numeric ID assigned to control file", value="2"),
                radioButtons("outputFromDesign", "Type of result", choices =list(
                            "data.table"=1, "data.frame"=2 ), selected=1),
                selectInput("formatFromDesign", "File Format", choices = list(
                            "bam"=1, "fastq"=2, "fasta"=3, "sam"=4, "bed"=5,
                            "bigBed"=6,"bigWig"=7)),
                selectInput("datatypeFromDesign","Data Type", choices =list(
                            "experiments"=1,"ucsc browser composite"=2, 
                            "annotations"=3,"matched sets"=4, "projects"=5,
                            "reference epigenomes"=6,"references"=7))),
            mainPanel(dataTableOutput("design")))
        ),
                           
#////-----------------------------searchEncode-----------------------------////        
        tabPanel("Search from ENCODE"),
                           
                           
#////-------------------------------About ---------------------------------////
        tabPanel("About")
    )
)