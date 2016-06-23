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
    designResultSearch <- NULL
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
                # Making a checkbox column with same value as the file_accession 
                # of this row.
                addCheckbox <- paste0('<input id="fuzzyBox" type="checkbox" name="downloadBox" value="',
                                        resultGlobal$file_accession,'">',"")
                resultGlobal <- cbind(Download=addCheckbox, resultGlobal)}, escape=FALSE,
                                options=list(searching=FALSE,pageLength = 10, scrollX = TRUE,
                                             columnDefs=list(list(targets=1:ncol(resultGlobal), className="alignCenter"))),
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
                # Making a checkbox column with same value as the file_accession 
                # of this row.
                addCheckbox <- paste0('<input id="fuzzyBox" type="checkbox" name="downloadBox" value="',
                                      resultGlobal$file_accession,'">',"")
                resultGlobal <- cbind(Download=addCheckbox, resultGlobal)}, escape=FALSE,
                options=list(searching=FALSE,pageLength = 10, scrollX = TRUE,
                             columnDefs=list(list(targets=1:ncol(resultGlobal), className="alignCenter"))),
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

    #Design request from fuzzySearch
    observeEvent(input$designFromSearch,{
        output$designVis <- reactive({TRUE})
        viewDesign <<-TRUE
        IDs <- c(input$repFromSearch,input$ctrlFromSearch)
        
        formatType <- c("long","wide")
        formatType <- formatType[as.numeric(input$formatFromSearch)]
        
        fileType <- c("bam", "fastq", "fasta", "sam", "bed", "bigBed", "bigWig")
        fileType <- fileType[as.numeric(input$fileFromSearch)]
        
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeFromSearch)]
        
        if(!input$splitFromSearch){
            
            # Design
            designResultSearch <<- createDesign(resultGlobal,
                                               encode_df,split=FALSE, fileFormat=fileType,
                                               dataset_type=dataType, format=formatType,
                                               output_type="data.table", ID=IDs)
            #Fetching the list of accession from the File column
            acc_list <- gsub(pattern = "/files/(.*)/.*/.*", replacement = "\\1",
                             x = designResultSearch$File)
            
            output$designResultSearch <- renderDataTable({
                addCheckbox <- paste0('<input id="fuzzyBox" type="checkbox" name="downloadBox" value="',
                                     acc_list,'">',"")
                designResultSearch <- cbind(Download=addCheckbox, designResultSearch)}, 
                escape=FALSE,options=list(searching=FALSE,pageLength = 25, scrollX = TRUE),
                
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
            temp <- filter(resultGlobal, file_format==fileType)
            temp <- filter(temp, dataset_type == dataType)
            acc <- unique(temp$accession)
            #Creating the design
            designResultSearch <<- createDesign(resultGlobal, df, split=TRUE,
                                        fileFormat=fileType,
                                        dataset_type=dataType,format="long", 
                                        output_type="data.table", ID=IDs)
            lenExp <- length(designResultSearch)
            #Fetching file accession  in each data frame of the list
            acc_list <- vector("list", lenExp)
            acc_list <- lapply(designResultSearch, function(i){
                unique(gsub(pattern = "/files/(.*)/.*/.*", replacement = "\\1",
                                      x = i$File))
            })
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
                    
                    output[[tablename]] <- renderDataTable({
                        addCheckbox <- paste0('<input id="fuzzyBox" type="checkbox" name="downloadBox" value="',
                                              acc_list[[my_i]],'">',"")
                        designResultSearch[[my_i]] <- cbind(Download=addCheckbox,
                                                         designResultSearch[[my_i]])},
                            escape=FALSE,options=list(searching=FALSE,pageLength = 25, scrollX = TRUE),
                            columnDefs=list(list(targets=1:64, className="dt-center")),
                            callback = "function(table) {
                            table.on('change.dt', '#fuzzyBox:checkbox', function() {
                            setTimeout(function () {
                            Shiny.onInputChange('downloadBox', $('#fuzzyBox:checked').map(function() {
                            return $(this).val();
                            }).get())
                            }, 10); 
                            });
                            }"
                  )
                })
            }
            
            
        }
    })
    #Download request from fuzzySearch
    observeEvent(input$downloadFromSearch,{
        
        selected_file <- paste(input$downloadBox)
        if(!viewDesign){
            downloadEncode(df=encode_df, resultSet=resultGlobal, file_acc=selected_file,
                      resultOrigin="queryEncode")
        }else{
            if(!is.data.table(designResultSearch)){
                designResultSearch <- rbindlist(designResultSearch)
            }
            #Making a subset of encode_df with the file_accession from the design
            list_file <- gsub(pattern = "/files/(.*)/.*/.*", replacement = "\\1",
                 x = designResultSearch$File)
            temp <- encode_df[encode_df$file_accession %in% list_file]
            
            downloadEncode(df=encode_df, resultSet=temp, file_acc=selected_file,
                           resultOrigin="queryEncode")
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
                                fileFormat=ff,lab=lb, organism=og,target=tg,
                                treatment=tr,file_status=fileStat, quiet=TRUE,
                                status=es,fixed=as.logical(input$fixed))
        
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
                                       df,split=FALSE, fileFormat=fileType,
                                       dataset_type=dataType, format=formatType,
                                       output_type="data.table", ID=IDs)
        if(!input$splitFromQuery){
            output$designAdvanced <- renderDataTable(designAdvanced,
                                                     options=list(searching=F,,pageLength = 10, scrollX = T))
        }else{
            temp <- filter(resultQuery, file_format==fileType)
            temp <- filter(temp, dataset_type == "dataType")
            acc <- unique(temp$accession)
            
            list_design <- createDesign(resultQuery, df,split=TRUE, format="long",
                                        file_format=fileType, dataset_type=dataType, 
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
                                options=list(searching=F,,pageLength = 10, scrollX = T))
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
                                        utput_type=outputTypeVec,ID=IDs),
                                         options=list(searching=FALSE))
    })
    
}