library(shiny)
emptyString <- NULL
ui <- fluidPage(
    navbarPage("ENCODExplorer",
#////------------------fuzzySearch-----------------------------////
        tabPanel("Search",sidebarLayout(
            sidebarPanel(
                actionButton("searchAction", "Search"),
                fileInput("df", "Database"),
                radioButtons("typeInput","Select your type of search",
                             choices = list("Single element"=1, "Mutiple element"=2),
                             selected=1),
                
                textInput("searchTerm", "Searching for ...", value = "Seperate term with a comma"),
                
                checkboxGroupInput("filter","Select your filter...",
                         choices = list("Accession"=1, "Dataset type"=2,
                         "Lab"=3, "Title"=4, "File Type"=5, "Platform"=6,
                         "Project"=7, "Type"=8, "Control"=9, "Biosample Type"=10,
                         "Biosample Name"=16,"Replicate"=11, "Organism"=12,
                         "File Accession"=13,"Target"=14,"Assay"=15,"File format"=17))
                
            ),
                      
            mainPanel(
                conditionalPanel(
                    condition = "input.searchAction > 0",
                    actionButton("designFromSearch", "Create a design"),
                    checkboxInput("splitFromSearch", "Split the result per experiment", value=FALSE),
                    radioButtons("formatFromSearch", "Design format", choices =list(
                        "long"=1, "wide"=2 ), selected=1),
                    selectInput("fileFromSearch", "File Format", choices = list(
                        "bam"=1, "fastq"=2, "fasta"=3, "sam"=4, "bed"=5, "bigBed"=6,
                        "bigWig"=7)),
                    selectInput("datatypeFromSearch","Data Type", choices =list(
                        "experiments"=1,"ucsc browser composite"=2, "annotations"=3,
                        "matched sets"=4, "projects"=5, "reference epigenomes"=6,
                        "references"=7)),
                    textInput("repFromSearch","Numeric ID assigned to replicate file", value="1"),
                    textInput("ctrlFromSearch","Numeric ID assigned to control file", value="2")
                ),
                dataTableOutput("searchResult")
            ))
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
                textInput("status", "Status", value="released")
                
            ),
            mainPanel(
                conditionalPanel(
                    condition = "input.searchAdvanced > 0",
                    actionButton("designFromQuery", "Create a design"),
                    checkboxInput("splitFromQuery", "Split the result per experiment", value=FALSE),
                    radioButtons("formatFromQuery", "Design format", choices =list(
                        "long"=1, "wide"=2 ), selected=1),
                    selectInput("fileFromQuery", "File Format", choices = list(
                        "bam"=1, "fastq"=2, "fasta"=3, "sam"=4, "bed"=5, "bigBed"=6,
                        "bigWig"=7)),
                    selectInput("datatypeFromQuery","Data Type", choices =list(
                        "experiments"=1,"ucsc browser composite"=2, "annotations"=3,
                        "matched sets"=4, "projects"=5, "reference epigenomes"=6,
                        "references"=7)),
                    textInput("repFromQuery","Numeric ID assigned to replicate file", value="1"),
                    textInput("ctrlFromQuery","Numeric ID assigned to control file", value="2")
                ),
                dataTableOutput("advancedResult")
            ))
        ),


#////-----------------------------createDesign-----------------------------////

        tabPanel("Design",sidebarLayout(
            sidebarPanel(
                 actionButton("actionDesign", "Create a design"),
                 fileInput("searchTable", "Import your file as rds format"),
                 fileInput("df", "Import your database (optional)"),
                 checkboxInput("split", "Split the result per experiment", value=FALSE),
                 radioButtons("formatStyle", "Design format", choices =list(
                    "long"=1, "wide"=2 ), selected=1),
                 textInput("replicateID","Numeric ID assigned to replicate file", value="1"),
                 textInput("controlID","Numeric ID assigned to control file", value="2"),
                 radioButtons("outputDesign", "Type of result", choices =list(
                     "data.table"=1, "data.frame"=2 ), selected=1),
                 selectInput("fileFormatDesign", "File Format", choices = list(
                     "bam"=1, "fastq"=2, "fasta"=3, "sam"=4, "bed"=5, "bigBed"=6,
                     "bigWig"=7)),
                 selectInput("datatypeDesign","Data Type", choices =list(
                     "experiments"=1,"ucsc browser composite"=2, "annotations"=3,
                     "matched sets"=4, "projects"=5, "reference epigenomes"=6,
                     "references"=7))
            ),
            mainPanel(
                textOutput("nameTest"),
                dataTableOutput("design")
            ))
        ),
       
        
#////-----------------------------searchEncode-----------------------------////        
        tabPanel("Search from ENCODE"),
        
        
#////-------------------------------About --------------------------------////
        tabPanel("About")
    )
)


server <- function(input, output) {
    require(dplyr)
    require(tidyr)
    require(data.table)
    load(file=system.file("../data/encode_df.rda", package="ENCODExplorer"))
    allFilter <- c("accession", "dataset_type","lab", "title", "file_type",
                   "platform","project", "type", "control", "biosample_type",
                   "replicate", "organism", "file_accession","target","assay",
                   "biosample_name","file_format")
    df <- encode_df
    resultGlobal <- NULL #Global variable for the result of a fuzzySearch
    resultQuery <- NULL #Global varaible for the result of a querySearch
    
    #////----------------------------------fuzzySearch----------------------////
    observeEvent(input$searchAction, {
        inputIsList <- FALSE
        if(input$typeInput == "2"){
            inputIsList <- TRUE
        }
        
        if(!input$split){
            if(length(input$filter) == 0){
                resultGlobal<<-fuzzySearch(input$searchTerm, inputIsList=inputIsList,
                                database=df)
                output$searchResult <- renderDataTable(resultGlobal, options=list(searching=FALSE))
            }else{
                resultGlobal<<-fuzzySearch(input$searchTerm,inputIsList=inputIsList,
                                database=df,filterVector=allFilter[as.numeric(input$filter)])
                output$searchResult <- renderDataTable(resultGlobal, options = list(searching=FALSE))
            }
        }else{#split result
            if(length(input$filter) == 0){
                resultGlobal<<-fuzzySearch(input$searchTerm, inputIsList=inputIsList,
                                           database=df)
                output$searchResult <- renderDataTable(resultGlobal, options=list(searching=FALSE))
            }else{
                resultGlobal<<-fuzzySearch(input$searchTerm,inputIsList=inputIsList,
                                           database=df,filterVector=allFilter[as.numeric(input$filter)])
                output$searchResult <- renderDataTable(resultGlobal, options = list(searching=FALSE))
            }
        }
    })
    
    #Design request from fuzzySearch
    observeEvent(input$designFromSearch,{
        IDs<-c(as.numeric(input$repFromSearch), as.numeric(input$ctrlFromSearch))
        formatType <- c("long","wide")
        formatType <- formatType[as.numeric(input$formatFromSearch)]
        fileType <- c("bam", "fastq", "fasta", "sam", "bed", "bigbed", "bigWig")
        fileType <- fileType[as.numeric(input$fileFromSearch)]
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeFromSearch)]
        
        output$searchResult <- renderDataTable(createDesign(resultGlobal, df,
                                    split=input$splitFromSearch, type_file=fileType,
                                    datatype=dataType, format=formatType, output_type=
                                    "data.table", ID=IDs), options=list(searching=F))
    })
    
#////----------------------------------queryEncode--------------------------////
    
    #Advanced search request
    observeEvent(input$searchAdvanced,{
        fileStat <- c("released","revoked", "all")
        fileStat <- fileStat[as.numeric(input$fileStatus)]
        
        #Parsing the input values
        if(input$setAccession == ""){ac <- NULL}
        else{ac <- input$setAccession}
        
        if(input$datasetAccession == ""){da <- NULL}
        else{da <- input$datasetAccession}
        
        if(input$assay == ""){as <- NULL}
        else{as <- input$assay}
        
        if(input$biosampleName == ""){bn <- NULL}
        else{bn <- input$biosampleName}
        
        if(input$biosampleType == ""){bt <- NULL}
        else{bt <- input$biosampleType}
        
        if(input$fileAccession == ""){fa <- NULL}
        else{fa <- input$fileAccession}
        
        if(input$fileFormat == ""){ff <- NULL}
        else{ff <- input$fileFormat}
        
        if(input$lab == ""){lb <- NULL}
        else{lb <- input$lab}
        
        if(input$organism == ""){og <- NULL}
        else{og <- input$organism}
        
        if(input$target == ""){tg <- NULL}
        else{tg <- input$target}
        
        if(input$treatment == ""){tr <- NULL}
        else{tr <- input$treatment}
        
        if(input$status == ""){es <- NULL}
        else{es <- input$status}
        
        if(input$project ==""){pr <- NULL}
        else{pr <- input$project}
        
        resultQuery <<- queryEncode(df=encode_df, set_accession=ac, dataset_accession=da,
                    assay=as, biosample_name=bn, biosample_type=bt, project=pr,
                    file_accession=fa, file_format=ff, lab=lb, organism=og,
                    target=tg, treatment=tr, file_status=fileStat, quiet=TRUE,
                    status=es, fixed=as.logical(input$fixed))
        output$advancedResult <- renderDataTable(resultQuery,options = list(searching=FALSE))
    })
    
    #Design request from advancedSearch
    observeEvent(input$designFromQuery,{
        require(dplyr)
        IDs<-c(as.numeric(input$repFromQuery), as.numeric(input$ctrlFromQuery))
        formatType <- c("long","wide")
        formatType <- formatType[as.numeric(input$formatFromQuery)]
        fileType <- c("bam", "fastq", "fasta", "sam", "bed", "bigbed", "bigWig")
        fileType <- fileType[as.numeric(input$fileFromQuery)]
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeFromQuery)]
        
        output$advancedResult <- renderDataTable(createDesign(resultQuery, df,
                                    split=input$splitFromQuery, type_file=fileType,
                                    datatype=dataType, format=formatType, output_type=
                                    "data.table", ID=IDs), options=list(searching=F))
    })

#////---------------------------------Design--------------------------------////  
    
    #Design request
    observeEvent(input$actionDesign,{
        a <- as.numeric(input$replicateID)
        b <- as.numeric(input$controlID)
        IDs <- c(a,b)
        fileRDS <- readRDS(input$searchTable$datapath)
        
        outputTypeVec <- c("data.table","data.frame")
        outputTypeVec <- outputTypeVec[as.numeric(input$outputDesign)]
        typeFile <- c("bam", "fastq", "fasta", "sam", "bed", "bigbed", "bigWig")
        typeFile <- typeFile[as.numeric(input$fileFormatDesign)]
        formatDesignVec <- c("long", "wide")
        formatDesignVec <- formatDesignVec[as.numeric(input$formatStyle)]
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeDesign)]
        
        output$design <- renderDataTable(createDesign(input=fileRDS, df=df,
                                split=input$split, type_file=typeFile,
                                datatype=dataType,format=formatDesignVec,
                                output_type=outputTypeVec,ID=IDs),
                                options=list(searching=FALSE))
    })
    
}

shinyApp(ui, server)