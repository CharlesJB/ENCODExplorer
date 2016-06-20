library(shiny)
library(shinythemes)

server <- function(input, output) {
    require(dplyr)
    require(tidyr)
    require(data.table)
    require(stringr)
    require(downloader)
    load(file=system.file("../data/encode_df.rda", package="ENCODExplorer"))
    allFilter <- c("accession", "dataset_type","lab", "title", "file_type",
                   "platform","project", "type", "control", "biosample_type",
                   "replicate", "organism", "file_accession","target","assay",
                   "biosample_name","file_format")
    df <- encode_df
    viewDesign <- FALSE #Global variable that indicate if a design is currently display
    resultGlobal <- NULL #Global variable for the result of a fuzzySearch
    resultQuery <- NULL #Global variable for the result of a querySearch
    #////----------------------------------fuzzySearch---------------------////
    observeEvent(input$searchAction, {
        output$designVis <- reactive({FALSE})
        viewDesign <<- FALSE
        outputOptions(output, "designVis", suspendWhenHidden=FALSE)
        multiple_Term <- FALSE
        if(input$typeInput == "2"){
            multiple_Term <- TRUE
        }
        
        if(length(input$filter) == 0){
            resultGlobal <<- fuzzySearch(input$searchTerm, 
                                    multipleTerm=multiple_Term, database=df)
            output$searchResult <- renderDataTable({
                                    addCheckbox <- paste0('<input id="fuzzyBox" type="checkbox" name="downloadBox" value="',
                                                          resultGlobal$href,'">',"")
                                    resultGlobal <- cbind(Download=addCheckbox, resultGlobal)}, escape=FALSE,
                                    options=list(searching=FALSE), 
                                    callback = "function(table) {
                                        table.on('change.dt', '#fuzzyBox:checkbox', function() {
                                    setTimeout(function () {
                                    Shiny.onInputChange('downloadBox', $('#fuzzyBox:checked').map(function() {
                                    return $(this).val();
                                    }).get())
                                    }, 10); 
                                    });
                                    }")
        }else{
            resultGlobal <<- fuzzySearch(input$searchTerm, 
                                        multipleTerm=multiple_Term, 
                                        database=encode_df, filterVector=
                                        allFilter[as.numeric(input$filter)])
            output$searchResult <- renderDataTable({
                addCheckbox <- paste0('<input id="fuzzyBox" type="checkbox" name="downloadBox" value="',
                                      resultGlobal$href,'">',"")
                resultGlobal <- cbind(Download=addCheckbox, resultGlobal)}, escape=FALSE,
                options=list(searching=FALSE), 
                callback = "function(table) {
                table.on('change.dt', '#fuzzyBox:checkbox', function() {
                setTimeout(function () {
                Shiny.onInputChange('downloadBox', $('#fuzzyBox:checked').map(function() {
                return $(this).val();
                }).get())
                }, 10); 
                });
        }")
        }
    })
    
    #Download request from fuzzySearch
    observeEvent(input$downloadFromSearch,{
        #Getting href of selected files
        file_toGet <- reactive({data.table(href=input$downloadBox)})
        fileTable <- file_toGet()
        md5sums <- vector("character", nrow(fileTable))
        
        
            #Getting md5sum
            for(i in seq_along(fileTable$href)){
                myRow <- resultGlobal[grepl(fileTable$href[i],resultGlobal$href)]
                md5sums[i] <- myRow$md5sum
                fileTable$href[i] <- paste0("https://www.encodeproject.org", fileTable$href[i])
            }
        
        fileTable <- cbind(fileTable,md5sums)
        #Downloading files
        for(i in seq_along(fileTable$href)){
            fileName <- strsplit(fileTable$href[i], split="@@download/")[[1]][2]
            fileName <- paste(".", fileName, sep="/")
            download(url=fileTable$href, destfile=fileName)
        }
    })
    
    
    
    #Design request from fuzzySearch
    observeEvent(input$designFromSearch,{
        output$designVis <- reactive({TRUE})
        viewDesign <<-TRUE
        IDs <- c(as.numeric(input$repFromSearch), as.numeric(input$ctrlFromSearch))
        formatType <- c("long","wide")
        formatType <- formatType[as.numeric(input$formatFromSearch)]
        fileType <- c("bam", "fastq", "fasta", "sam", "bed", "bigBed", "bigWig")
        fileType <- fileType[as.numeric(input$fileFromSearch)]
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeFromSearch)]
        
        if(!input$splitFromSearch){
            designResultSearch <- createDesign(resultGlobal,
                                               encode_df,split=FALSE, file_type=fileType,
                                               dataset_type=dataType, format=formatType,
                                               output_type="data.table", ID=IDs)
            output$designResultSearch <- renderDataTable({addCheckbox <- paste0('<input id="fuzzyBox" type="checkbox" name="downloadBox" value="',
                                                                               designResultSearch$file,'">',"")
                resultGlobal <- cbind(Download=addCheckbox, designResultSearch)}, escape=FALSE,
                options=list(searching=FALSE), 
                callback = "function(table) {
                table.on('change.dt', '#fuzzyBox:checkbox', function() {
                setTimeout(function () {
                Shiny.onInputChange('downloadBox', $('#fuzzyBox:checked').map(function() {
                return $(this).val();
                }).get())
                }, 10); 
                });
             }")
            
        }else{
            require(tidyr)
            require(dplyr)
            #Getting the list of experiment
            temp <- filter(resultGlobal, file_type==fileType)
            temp <- filter(temp, dataset_type == dataType)
            acc <- unique(temp$accession)
            #Creating the design
            list_design <- createDesign(resultGlobal, df, split=TRUE,
                                        file_type=fileType, dataset_type=dataType,
                                        format="long", 
                                        output_type="data.table", ID=IDs)
            lenExp <- length(list_design)
            #Creating the empty list of dataTable that will be display 
            output$designSplitSearch <- renderUI({
                table_output_list <- vector("list", lenExp)
                table_output_list <- lapply(1:lenExp, function(i){
                    tablename <- paste(acc[i])
                    dataTableOutput(tablename)
                })
                tagList(table_output_list)
                
            })
            #Filling the list with the table within the design
            for(i in 1:lenExp) {
                local({
                    my_i <- i
                    tablename <- paste(acc[my_i])
                    output[[tablename]] <- renderDataTable(list_design[[my_i]],
                                                           options=list(searching=F))
                })
            }
            
            
        }
    })
    
    
    #////----------------------------------queryEncode-------------------------////
    
    #Advanced search request
    observeEvent(input$searchAdvanced,{
        output$designVis <- reactive({FALSE})
        outputOptions(output, "designVis", suspendWhenHidden=FALSE)
        
        fileStat <- c("released","revoked", "all")
        fileStat <- fileStat[as.numeric(input$fileStatus)]
        
        #Parsing the input values of the filters
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
        
        resultQuery <<- queryEncode(df=encode_df, set_accession=ac, 
                                    dataset_accession=da,assay=as, biosample_name=bn,
                                    biosample_type=bt, project=pr,file_accession=fa,
                                    file_format=ff,lab=lb, organism=og,target=tg, treatment=tr,
                                    file_status=fileStat, quiet=TRUE,status=es,
                                    fixed=as.logical(input$fixed))
        output$advancedResult <- renderDataTable(resultQuery,options=
                                                     list(searching=FALSE))
    })
    
    #Design request from advancedSearch
    observeEvent(input$designFromQuery,{
        require(dplyr)
        output$designVis <- reactive({TRUE})
        IDs<-c(as.numeric(input$repFromQuery), as.numeric(input$ctrlFromQuery))
        formatType <- c("long","wide")
        formatType <- formatType[as.numeric(input$formatFromQuery)]
        fileType <- c("bam", "fastq", "fasta", "sam", "bed", "bigbed", "bigWig")
        fileType <- fileType[as.numeric(input$fileFromQuery)]
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeFromQuery)]
        
        designAdvanced <- createDesign(resultQuery,
                                       df,split=FALSE, file_type=fileType,
                                       dataset_type=dataType, format=formatType,
                                       output_type="data.table", ID=IDs)
        if(!input$splitFromQuery){
            output$designAdvanced <- renderDataTable(designAdvanced,
                                                     options=list(searching=F))
        }else{
            temp <- filter(resultQuery, file_type==fileType)
            temp <- filter(temp, dataset_type == "dataType")
            acc <- unique(temp$accession)
            
            list_design <- createDesign(resultQuery, df,split=TRUE, format="long",
                                        file_type=fileType, dataset_type=dataType, 
                                        output_type="data.table", ID=IDs)
            lenExp <- length(list_design)
            
            #Creating a list of empty dataTable
            output$designSplitQuery <- renderUI({
                table_output_list <- vector("list",lenExp)
                table_output_list <- lapply(1:lenExp, function(i){
                    tablename <- paste(acc[i])
                    dataTableOutput(tablename)
                })
                tagList(table_output_list)
                
            })
            #Filing the dataTable with the table within the design
            for(i in 1:lenExp) {
                local({
                    my_i <- i
                    tablename <- paste(acc[my_i])
                    output[[tablename]] <- renderDataTable(list_design[[my_i]],
                                                options=list(searching=F))
                })
            }
            
        }
        
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