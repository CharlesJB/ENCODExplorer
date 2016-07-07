#' @import shiny
#' @import shinythemes
#' @import data.table
#' @import tools
#' @import stringr
#' @import dplyr filter
#' 
library(shiny)
library(shinythemes)

server <- function(input, output) {
    require(dplyr)
    require(tidyr)
    require(data.table)
    require(stringr)
    require(tools)
    
    load(file=system.file("../data/encode_df.rda", package="ENCODExplorer"))
    allFilter <- c("accession", "dataset_type","lab", "title", "file_type",
                   "platform","project", "type", "control", "biosample_type",
                   "replicate", "organism", "file_accession","target","assay",
                   "biosample_name","file_format")
    df <- encode_df
    viewDesign <- FALSE #Global variable that indicate if a design is currently display
    resultGlobal <- NULL #Global variable for the result of a fuzzySearch
    resultQuery <- NULL #Global variable for the result of a querySearch
    designResultSearch <- NULL #Global variable for the design from fuzzySearch
    designAdvanced <- NULL #Global variable for design from queryEncode
    designSplit <- NULL
    #////----------------------------------fuzzySearch---------------------////
    observeEvent(input$searchAction, {
        output$designVis <- reactive({FALSE})
        viewDesign <<- FALSE
        outputOptions(output, "designVis", suspendWhenHidden=FALSE)
        output$consoleSearch <- renderPrint("Click on rows to select files and than use the Download button")
        multiple_Term <- FALSE
        
        if(input$typeInput == "2"){
            multiple_Term <- TRUE
        }
        
        if(length(input$filter) == 0){
            resultGlobal <<- fuzzySearch(input$searchTerm, 
                                    multipleTerm=multiple_Term, database=df)
            #Removing empty row and a few specific row
            resultGlobal <<- resultGlobal[ , !c("antibody_characterization",
                                                "uuid","notes"), with=F]
            resultGlobal <<- resultGlobal[,sapply(resultGlobal, function(i){
                !all(sapply(i,function(val){is.na(val)|identical(val,"")}))}), with=F]
            
            if(nrow(resultGlobal)>0){
                output$searchResult <- DT::renderDataTable(resultGlobal,
                    options=list(searching=FALSE, fixedHeader=T, pageLength=25,
                        scrollY="600px", scrollCollapse=T,scrollX=T, deferRender=T,
                        columnDefs=list(list(targets=1:ncol(resultGlobal),
                                             className="dt-center")))
                )
            }else{
                output$searchResult <- DT::renderDataTable(data.table())
            }
        }else{
            resultGlobal <<- fuzzySearch(input$searchTerm, 
                                        multipleTerm=multiple_Term, 
                                        database=encode_df, filterVector=
                                        allFilter[as.numeric(input$filter)])
            #Removing empty row and a few specific row
            resultGlobal <<- resultGlobal[ , !c("antibody_characterization",
                                                "uuid","notes"), with=F]
            resultGlobal <<- resultGlobal[,sapply(resultGlobal, function(i){
                !all(sapply(i,function(val){is.na(val)|identical(val,"")}))}), with=F]
            
            if(nrow(resultGlobal) > 0){
                output$searchResult <- DT::renderDataTable(resultGlobal,
                    options=list(searching=FALSE, fixedHeader=T,  pageLength=25,
                            scrollY="600px", scrollCollapse=T,
                            scrollX=T, deferRender=T, columnDefs=list(list(
                            targets=1:ncol(resultGlobal), className="dt-center")))
                )
            }else{
                output$searchResult <- DT::renderDataTable(data.table())
            }
        
    }})
    
    
    #Design request from fuzzySearch
    observeEvent(input$designFromSearch,{
        output$designVis <- reactive({TRUE})
        viewDesign <<- TRUE
        output$consoleSearch <- renderPrint("Click on rows to select files and than use the Download button")
        
        IDs <- c(input$repFromSearch,input$ctrlFromSearch)
        
        formatType <- c("long","wide")
        formatType <- formatType[as.numeric(input$formatFromSearch)]
        
        fileType <- c("bam", "fastq", "fasta", "sam", "bed", "bigBed", "bigWig")
        fileType <- fileType[as.numeric(input$fileFromSearch)]
        
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeFromSearch)]
        
        if(nrow(resultGlobal) == 0){
            output$designResultSearch <- DT::renderDataTable(data.table(
                File=character(),Experiment=character(),Value=numeric()))
        }else{
        
            if(!input$splitFromSearch){
                designSplit <<- FALSE
                # Design
                designResultSearch <<- createDesign(resultGlobal,
                                               encode_df,split=FALSE, fileFormat=fileType,
                                               dataset_type=dataType, format=formatType,
                                               output_type="data.table", ID=IDs)
                #Fetching the list of accession from the File column
                acc_list <- gsub(pattern = "/files/(.*)/.*/.*", replacement = "\\1",
                             x = designResultSearch$File)
            
                output$designResultSearch <- DT::renderDataTable(designResultSearch, 
                   options=list(searching=FALSE,pageLength = 25,searching=FALSE,
                                fixedHeader=T, scrollX=T, scrollY="600px", 
                                scrollCollapse=T, deferRender=T)
                )
            
            }else{
                designSplit <<- TRUE
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
                        DT::dataTableOutput(tablename)
                    })
                tagList(table_output_list)
                
                })
                #Filling the list with the table within the design
                for(i in 1:lenExp) {
                    local({
                        my_i <- i
                        tablename <- paste(acc[my_i])
                        output[[tablename]] <- DT::renderDataTable(designResultSearch[[my_i]],
                                options=list(searching=FALSE, paging=F))
                    })
                }
            }
        }
    })
    
    #Download request from fuzzySearch
    observeEvent(input$downloadFromSearch,{
        
        if(!viewDesign){
            selected_rows <- paste(input$searchResult_rows_selected)
            selected_files <- resultGlobal$file_accession[as.numeric(selected_rows)]
            
            downloadLog <- capture.output(downloadEncode(dt=encode_df,
                                    file_acc=selected_files))
        }else{
            if(!input$splitFromSearch){
                selected_rows <- paste(input$designResultSearch_rows_selected)
                selected_files <- designResultSearch$File[as.numeric(selected_rows)]
                
            }else{
                unsplitDesign <-rbindlist(designResultSearch)
                acc <- unique(unsplitDesign$Experiment)
                row_to_get <- vector("list",length(acc))
                for(i in 1:length(acc)){
                    row_to_get[[i]] <- input[[local({
                        my_i <- i
                        tb <- paste(acc[my_i],"_rows_selected",sep="")
                    })]]
                    
                }
                selected_rows <- (unlist(row_to_get))
                selected_files <- unsplitDesign$File[as.numeric(selected_rows)]
            }
            
            selected_files <- sapply(selected_files, function(i){
                temp <- file_path_sans_ext(basename(i))
                paste(unlist(strsplit(temp,"[.]"))[1])
            })
            
            if(length(selected_files) > 0){
                downloadLog <- capture.output(downloadEncode(dt=encode_df,
                                          file_acc=unlist(selected_files)))
            }
        }
        #Parsing the output message of the download
        downloadLog <- gsub(x=downloadLog, pattern = "\\[\\d\\] ", replacement="")
        downloadLog <- gsub(x=downloadLog, pattern='\"', replacement="")
        output$consoleSearch <- renderPrint(downloadLog)
        
    })
    
    #Convert bytes to Kb, Mb or Gb
    convert_to_byte <- function(x=NULL) {
            if(x[[2]] == "Gb") {
                as.numeric(x[[1]])*1073741824
            } else if (x[[2]] == "Mb") {
                as.numeric(x[[1]])*1048576
            } else if (x[[2]] == "Kb") {
                as.numeric(x[[1]])*1024
            }
    }
    
    #Convert Kb, Mb or Gb to B
    convert_to_all <- function(x){
        if (x >= 1073741824) {
            paste(as.character(round((x/1073741824),2))," Gb")
        } else if (x >= 1048576) {
            paste(as.character(round((x/1048576), 1)), " Mb")
        } else if (x >= 1024) {
            paste(as.character(round((x/1024), 1)), " Kb")
        } else {
            paste(as.character(x), " b")
        }
    }
    
    #Function that return the sum of selected files size
    file_size_fuzzy <- function(rows=NULL,is_design=F){
        #Getting selected row and thier size
        if(!is_design){
            selected_rows <- paste(rows)
        }else{ #Coming from a design
            selected_rows <- paste(rows)
            selected_files <- designResultSearch$File[as.numeric(selected_rows)]
            selected_files <- sapply(selected_files, function(i){
                temp <- file_path_sans_ext(basename(i))
                paste(unlist(strsplit(temp,"[.]"))[1])
            })
            as.character(selected_files)
        }
        selected_size <- resultGlobal$file_size[as.numeric(selected_rows)]
        #Converting all size in byte to apply sum
        selected_size <- sapply(selected_size,function(i){
            val <- unlist(str_split(i," "))
            convert_to_byte(val)
        })
        sum_file <- sum(selected_size)
        #Converting sum_file 
        convert_to_all(sum_file)
    }
    
    
    #Refresh the content of fileSizeFuzzy
    refresh_fuzzy_display <- eventReactive(c(input$searchResult_rows_selected,input$designResultSearch_rows_selected,
                                       input$designFromSearch, input$searchAction, input$splitFromSearch),{
        if(!(viewDesign)){ #for fuzzySearch object
            if(length(input$searchResult_rows_selected) > 0){
                paste("Number of selected files :", length(input$searchResult_rows_selected),
                      " Total size of the selected files :",file_size_fuzzy(input$searchResult_rows_selected))
            }
        }else{ #for design object
            if(!input$splitFromSearch & length(input$designResultSearch_rows_selected) > 0 ){
                paste("Number of selected files :",length(input$designResultSearch_rows_selected),
                      " Total size of the selected files :",file_size_fuzzy(input$designResultSearch_rows_selected,T))
            }
            
        }
    })
    
    output$fileSizeFuzzy <- renderText({
        refresh_fuzzy_display()
    })
    
    #////----------------------------------queryEncode-------------------------////
    
    #Advanced search request
    observeEvent(input$searchAdvanced,{
        output$designVis <- reactive({FALSE})
        outputOptions(output, "designVis", suspendWhenHidden=FALSE)
        output$consoleQuery <- renderPrint("Click on rows to select files and than use the Download button")
        
        
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
                                file_format=ff,lab=lb, organism=og,target=tg,
                                treatment=tr,file_status=fileStat, quiet=TRUE,
                                status=es,fixed=as.logical(input$fixed))
        #Removing empty specific and empty row
        resultQuery <<- resultQuery[ , !c("antibody_characterization","uuid",
                                          "notes"), with=F]
        resultQuery <<- resultQuery[,sapply(resultQuery, function(i){
            !all(sapply(i,function(val){is.na(val)|identical(val,"")}))}), with=F]
        if(nrow(resultQuery) == 0){
            output$advancedResult <- DT::renderDataTable(data.table())
        }else{
            output$advancedResult <- DT::renderDataTable(resultQuery, escape=F,
                options=list(searching=F, fixedHeader=T, pageLength=25,
                    scrollY="600px", scrollCollapse=T, scrollX=T, deferRender=T,
                    columnDefs=list(list(targets=1:ncol(resultQuery),
                                            className="dt-center")))
            )
        }
    })
    
    #Design request from advancedSearch
    observeEvent(input$designFromQuery,{
        output$consoleQuery <- renderPrint("Click on rows to select files and than use the Download button")
        output$designVis <- reactive({TRUE})
        viewDesign <<- TRUE
        IDs<-c(as.numeric(input$repFromQuery), as.numeric(input$ctrlFromQuery))
        
        formatType <- c("long","wide")
        formatType <- formatType[as.numeric(input$formatFromQuery)]
        
        fileType <- c("bam", "fastq", "fasta", "sam", "bed", "bigbed", "bigWig")
        fileType <- fileType[as.numeric(input$fileFromQuery)]
        
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeFromQuery)]
        
        if(nrow(resultQuery) == 0){DT::renderDataTable(data.table())}
        else{
            designAdvanced <<- createDesign(resultQuery,
                                       df,split=FALSE, fileFormat=fileType,
                                       dataset_type=dataType, format=formatType,
                                       output_type="data.table", ID=IDs)
            if(!input$splitFromQuery){ #Not split
                designSplit <<- FALSE
                acc_list <- gsub(pattern = "/files/(.*)/.*/.*", replacement = "\\1",
                             x = designAdvanced$File)
                output$designAdvanced <- DT::renderDataTable(designAdvanced,
                    options=list(searching=F, pageLength = 25, scrollX = T,
                                scrollY="600px", scrollCollapse=T, deferRender=T,
                                columnDefs=list(list(targets=1:ncol(designAdvanced),
                                                     className="dt-center")))
                )
            }else{ #Split
                designSplit <<- TRUE
                temp <- filter(resultQuery, file_format==fileType)
                temp <- filter(temp, dataset_type == dataType)
                acc <- unique(temp$accession)
            
                designAdvanced <<- createDesign(resultQuery, df,split=TRUE, format="long",
                                        fileFormat=fileType, dataset_type=dataType, 
                                        output_type="data.table", ID=IDs)
                lenExp <- length(designAdvanced)
                #Fetching file accession  in each data frame of the list
                acc_list <- vector("list", lenExp)
                acc_list <- lapply(designAdvanced, function(i){
                    unique(gsub(pattern = "/files/(.*)/.*/.*", replacement = "\\1",
                            x = i$File))
                })
                #Creating a list of empty dataTable
                output$designSplitQuery <- renderUI({
                    table_output_list <- vector("list",lenExp)
                    table_output_list <- lapply(1:lenExp, function(i){
                        tablename <- paste(acc[i])
                        DT::dataTableOutput(tablename)
                    })
                    tagList(table_output_list)
                
                })
            #Filing the dataTable with the table within the design
            for(i in 1:lenExp) {
                local({
                    my_i <- i
                    tablename <- paste(acc[my_i])
                    output[[tablename]] <- DT::renderDataTable(designAdvanced[[my_i]],
                        options=list(searching=FALSE,
                                            pageLength = 25, scrollX = TRUE))
                })
            }
            
        }
        }
        
    })
    
    #Download request from queryEncode
    observeEvent(input$downloadFromQuery,{
        if(!viewDesign){
            selected_rows <- paste(input$advancedResult_rows_selected)
            selected_files <- resultQuery$file_accession[as.numeric(selected_rows)]
        
            downloadLog <- capture.output(downloadEncode(dt=encode_df, file_acc=
                           selected_files))
        }else{
            if(!designSplit){
                selected_rows <- paste(input$designAdvanced_rows_selected)
                selected_files <- designAdvanced$File[as.numeric(selected_rows)]
                selected_files <- sapply(selected_files, function(i){
                    temp <- file_path_sans_ext(basename(i))
                    paste(unlist(strsplit(temp,"[.]"))[1])
                })
                
                if(length(selected_files) > 0){
                    downloadLog <- capture.output(downloadEncode(dt=encode_df,
                                              file_acc=unlist(selected_files)))
                }
            }
        }
        downloadLog <- gsub(x=downloadLog, pattern = "\\[\\d\\] ", replacement = "")
        downloadLog <- gsub(x=downloadLog, pattern='\"', replacement="")
        output$consoleQuery <- renderPrint(downloadLog)
    })
    
    #Refresh the content of fileSizeQuery
    refresh_query_display <- eventReactive(c(input$advancedResult_rows_selected,input$designAdvanced_rows_selected,
                                            input$designFromQuery, input$searchAdvanced, input$designSplitQuery),{
          if(!(viewDesign)){ #for fuzzySearch object
              if(length(input$advancedResult_rows_selected) > 0){
                  paste("Number of selected files :", length(input$advancedResult_rows_selected),
                  " Total size of the selected files :",file_size_query(input$advancedResult_rows_selected))
              }
          }else{ #for design object
              if(!input$splitFromQuery & length(input$designAdvanced_rows_selected) > 0 ){
                 paste("Number of selected files :",length(input$designAdvanced_rows_selected),
                 " Total size of the selected files :",file_size_query(input$designAdvanced_rows_selected,T))
              }
          }
     })
    
    #Function that return the sum of selected files size
    file_size_query <- function(rows=NULL,is_design=F){
      #Getting selected row and thier size
      if(!is_design){
        selected_rows <- paste(rows)
      }else{ #Coming from a design
        selected_rows <- paste(rows)
        selected_files <- designAdvanced$File[as.numeric(selected_rows)]
        selected_files <- sapply(selected_files, function(i){
          temp <- file_path_sans_ext(basename(i))
          paste(unlist(strsplit(temp,"[.]"))[1])
        })
        as.character(selected_files)
      }
      selected_size <- resultQuery$file_size[as.numeric(selected_rows)]
      #Converting all size in byte to apply sum
      selected_size <- sapply(selected_size,function(i){
        val <- unlist(str_split(i," "))
        convert_to_byte(val)
      })
      sum_file <- sum(selected_size)
      #Converting sum_file 
      convert_to_all(sum_file)
    }
    
    output$fileSizeQuery <- renderText({
      refresh_query_display()
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