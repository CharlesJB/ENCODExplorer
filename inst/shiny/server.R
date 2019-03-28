#' @import shiny
#' @import shinythemes
#' @import data.table
#' @import tools
#' @import stringr
#' @importFrom tidyr spread
#' @importFrom dplyr filter
#' @importFrom dplyr setdiff
#' 
library(shiny)
library(shinythemes)

server <- function(input, output) {
    
    data(encode_df)
    allFilter <- c("accession", "dataset_type","lab", "title", "file_type",
                   "platform","project", "type", "control", "biosample_type",
                   "replicate", "organism", "file_accession","target","assay",
                   "biosample_name","file_format")
    df <- encode_df
    viewDesign <- FALSE #Global variable that indicate if a design is currently display
    resultFuzzy <- NULL #Global variable for the result of a fuzzySearch
    resultQuery <- NULL #Global variable for the result of a querySearch
    designFuzzy <- NULL #Global variable for the design from fuzzySearch
    designQuery <- NULL #Global variable for design from queryEncode
    designSplit <- NULL
    #////----------------------------------fuzzySearch---------------------////
    observeEvent(input$searchAction, {
        output$designVis <- reactive({FALSE})
        viewDesign <<- FALSE
        outputOptions(output, "designVis", suspendWhenHidden=FALSE)
        output$consoleSearch <- renderPrint(cat("Select row before using Download button"))
        multiple_Term <- FALSE
        
        if(input$typeInput == "2"){
            multiple_Term <- TRUE
        }
        
        if(length(input$filter) == 0){
            resultFuzzy <<- fuzzySearch(input$searchTerm, 
                                    multipleTerm=multiple_Term, database=df)
            #Removing empty row and a few specific row
            resultFuzzy <<- resultFuzzy[ , !c("antibody_characterization",
                                                "uuid","notes"), with=FALSE]
            resultFuzzy <<- resultFuzzy[,sapply(resultFuzzy, function(i){
                !all(sapply(i,function(val){is.na(val)|identical(val,"")}))}), with=FALSE]
            
            if(nrow(resultFuzzy)>0){
                output$searchFuzzy <- DT::renderDataTable(resultFuzzy,
                    options=list(searching=FALSE, fixedHeader=TRUE, pageLength=25,
                        scrollY="600px", scrollCollapse=TRUE,scrollX=TRUE, deferRender=TRUE,
                        columnDefs=list(list(targets=1:ncol(resultFuzzy),
                                             className="dt-center")))
                )
            }else{
                output$consoleSearch <- renderPrint(cat("Error : No result found"))
                output$searchFuzzy <- DT::renderDataTable(data.table())
            }
        }else{
            resultFuzzy <<- fuzzySearch(input$searchTerm, 
                                        multipleTerm=multiple_Term, 
                                        database=encode_df, filterVector=
                                        allFilter[as.numeric(input$filter)])
            #Removing empty row and a few specific row
            resultFuzzy <<- resultFuzzy[ , !c("antibody_characterization",
                                                "uuid","notes"), with=FALSE]
            resultFuzzy <<- resultFuzzy[,sapply(resultFuzzy, function(i){
                !all(sapply(i,function(val){is.na(val)|identical(val,"")}))}), with=FALSE]
            
            if(nrow(resultFuzzy) > 0){
                output$searchFuzzy <- DT::renderDataTable(resultFuzzy,
                    options=list(searching=FALSE, fixedHeader=TRUE,  pageLength=25,
                            scrollY="600px", scrollCollapse=TRUE,
                            scrollX=TRUE, deferRender=TRUE, columnDefs=list(list(
                            targets=1:ncol(resultFuzzy), className="dt-center")))
                )
            }else{
              output$consoleSearch <- renderPrint(cat("Error : No result found"))
              output$searchFuzzy <- DT::renderDataTable(data.table())
            }
        
    }})
    
    
    #Design request from fuzzySearch
    observeEvent(input$designFromSearch,{
        output$designVis <- reactive({TRUE})
        viewDesign <<- TRUE
        output$consoleSearch <- renderPrint(cat("Select row before using Download button"))
        
        IDs <- c(as.numeric(input$repFromSearch),as.numeric(input$ctrlFromSearch))
        
        formatType <- c("long","wide")
        formatType <- formatType[as.numeric(input$formatFromSearch)]
        
        fileType <- c("bam", "fastq", "sam", "bed", "bigBed", "bigWig")
        fileType <- fileType[as.numeric(input$fileFromSearch)]
        
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeFromSearch)]
        
        if(nrow(resultFuzzy) == 0){
            output$designFuzzy <- DT::renderDataTable(data.table(
                File=character(),Experiment=character(),Value=numeric()))
        }else if (nrow(dplyr::filter(resultFuzzy, file_format == fileType)) == 0){
            output$consoleSearch <- renderPrint(cat("Error : could not find files for this format"))
            output$designFuzzy <- DT::renderDataTable(data.table(
                File=character(),Experiment=character(),Value=numeric()))
        }else{
        
            if(!input$splitFromSearch){
                designSplit <<- FALSE
                # Design
                designFuzzy <<- createDesign(resultFuzzy,
                                               encode_df,split=FALSE, fileFormat=fileType,
                                               dataset_type=dataType, format=formatType,
                                               output_type="data.table", ID=IDs)
                #Fetching the list of accession from the File column
                acc_list <- gsub(pattern = "/files/(.*)/.*/.*", replacement = "\\1",
                             x = designFuzzy$File)
            
                output$designFuzzy <- DT::renderDataTable(designFuzzy, 
                   options=list(searching=FALSE,pageLength = 25,searching=FALSE,
                                fixedHeader=TRUE, scrollX=TRUE, scrollY="600px", 
                                scrollCollapse=TRUE, deferRender=TRUE)
                )
            
            }else{
                designSplit <<- TRUE
                #Getting the list of experiment
                temp <- dplyr::filter(resultFuzzy, file_format == fileType)
                temp <- dplyr::filter(temp, dataset_type == dataType)
                acc <- unique(temp$accession)
                #Creating the design
                designFuzzy <<- createDesign(resultFuzzy, df, split=TRUE,
                                        fileFormat=fileType,
                                        dataset_type=dataType,format="long", 
                                        output_type="data.table", ID=IDs)
                lenExp <- length(designFuzzy)
                #Fetching file accession  in each data frame of the list
                acc_list <- vector("list", lenExp)
                acc_list <- lapply(designFuzzy, function(i){
                    unique(gsub(pattern = "/files/(.*)/.*/.*", replacement = "\\1",
                                      x = i$File))
                })
                #Creating the empty list of dataTable that will be display 
                output$designSplitFuzzy <- renderUI({
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
                        output[[tablename]] <- DT::renderDataTable(designFuzzy[[my_i]],
                                options=list(searching=FALSE, paging=FALSE))
                    })
                }
            }
        }
    })
    
    #Download request from fuzzySearch
    observeEvent(input$downloadFromSearch,{
        
        if(!viewDesign){
            selected_rows <- paste(input$searchFuzzy_rows_selected)
            selected_files <- resultFuzzy$file_accession[as.numeric(selected_rows)]
            downloadLog <- capture.output(downloadEncode(df=encode_df,
                                    file_acc=selected_files))
        }else{
            if(!designSplit){
                selected_rows <- paste(input$designFuzzy_rows_selected)
                selected_files <- designFuzzy$File[as.numeric(selected_rows)]
                
            }else{
                unsplitDesign <-rbindlist(designFuzzy)
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
                temp <- tools::file_path_sans_ext(basename(i))
                paste(unlist(strsplit(temp,"[.]"))[1])
            })
            
            if(length(selected_files) > 0){
                downloadLog <- capture.output(downloadEncode(df=encode_df,
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
    file_size_fuzzy <- function(rows=NULL,is_design=FALSE){
        #Getting selected row and thier size
        selected_rows <- paste(rows)
        if(!viewDesign){
            selected_size <- resultFuzzy$file_size[as.numeric(selected_rows)]
        }else{ #Coming from a design
            selected_files <- designFuzzy$File[as.numeric(selected_rows)]
            selected_files <- sapply(selected_files, function (i){
                                      unlist(strsplit(i,"[.]"))[1]})
            selected_size <- dplyr::filter(encode_df, file_accession %in% selected_files)
            selected_size <- selected_size$file_size
        }

        #Converting all size in byte to apply sum
        selected_size <- sapply(selected_size,function(i){
            val <- unlist(strsplit(i," "))
            convert_to_byte(val)
        })
        sum_file <- sum(selected_size)
        #Converting sum_file 
        convert_to_all(sum_file)
    }
    
    
    #Refresh the content of fileSizeFuzzy
    refresh_fuzzy_display <- eventReactive(c(input$searchFuzzy_rows_selected,
                                input$designFuzzy_rows_selected,
                                input$designFromSearch, input$searchAction,
                                input$splitFromSearch),{
        if(!(viewDesign)){ #for fuzzySearch object
            if(length(input$searchFuzzy_rows_selected) > 0){
                paste("Number of selected files :", length(input$searchFuzzy_rows_selected),
                      " Total size of the selected files :",file_size_fuzzy(input$searchFuzzy_rows_selected))
            }
        }else{ #for design object
            if(!input$splitFromSearch & length(input$designFuzzy_rows_selected) > 0 ){
                paste("Number of selected files :",length(input$designFuzzy_rows_selected),
                      " Total size of the selected files :",file_size_fuzzy(input$designFuzzy_rows_selected,TRUE))
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
        output$consoleQuery <- renderPrint("Select row before using Download button")
        
        
        fileStat <- c("released","revoked", "all")
        fileStat <- fileStat[as.numeric(input$fileStatus)]
        
        #Parsing the input values of the filters
        if(input$setAccession == ""){ac <- NULL}
        else{ac <- input$setAccession}
        
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
                                assay=as, biosample_name=bn,
                                biosample_type=bt, project=pr,file_accession=fa,
                                file_format=ff,lab=lb, organism=og,target=tg,
                                treatment=tr,file_status=fileStat, quiet=FALSE,
                                status=es,fixed=as.logical(input$fixed))
        #Removing empty specific and empty row
        resultQuery <<- resultQuery[ , !c("antibody_characterization","uuid",
                                          "notes"), with=FALSE]
        resultQuery <<- resultQuery[,sapply(resultQuery, function(i){
            !all(sapply(i,function(val){is.na(val)|identical(val,"")}))}), with=FALSE]
        if(nrow(resultQuery) == 0){
          output$consoleQuery <- renderPrint(cat("Error : No result found"))
          output$resultQuery <- DT::renderDataTable(data.table())
        }else{
            output$resultQuery <- DT::renderDataTable(resultQuery, escape=FALSE,
                options=list(searching=FALSE, fixedHeader=TRUE, pageLength=25,
                    scrollY="600px", scrollCollapse=TRUE, scrollX=TRUE, deferRender=TRUE,
                    columnDefs=list(list(targets=1:ncol(resultQuery),
                                            className="dt-center")))
            )
        }
    })
    
    #Design request from advancedSearch
    observeEvent(input$designFromQuery,{
        output$consoleQuery <- renderPrint(cat("Select row before using Download button"))
        output$designVis <- reactive({TRUE})
        viewDesign <<- TRUE
        IDs<-c(as.numeric(input$repFromQuery), as.numeric(input$ctrlFromQuery))
        
        formatType <- c("long","wide")
        formatType <- formatType[as.numeric(input$formatFromQuery)]
        
        fileType <- c("bam", "fastq", "sam", "bed", "bigBed", "bigWig")
        fileType <- fileType[as.numeric(input$fileFromQuery)]
        
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeFromQuery)]
        if(nrow(resultQuery) == 0){
          DT::renderDataTable(data.table())
        }else if (nrow(dplyr::filter(resultQuery, file_format == fileType)) == 0){
          output$consoleQuery <- renderPrint(cat("Error : could not find files for this format"))
          output$designQuery <- DT::renderDataTable(data.table(
            File=character(),Experiment=character(),Value=numeric()))
        }else{
            designQuery <<- createDesign(resultQuery,
                                       df,split=FALSE, fileFormat=fileType,
                                       dataset_type=dataType, format=formatType,
                                       output_type="data.table", ID=IDs)
            if(!input$splitFromQuery){ #Not split
                designSplit <<- FALSE
                acc_list <- gsub(pattern = "/files/(.*)/.*/.*", replacement = "\\1",
                             x = designQuery$File)
                output$designQuery <- DT::renderDataTable(designQuery,
                    options=list(searching=FALSE, pageLength = 25, scrollX = TRUE,
                                scrollY="600px", scrollCollapse=TRUE, deferRender=TRUE,
                                columnDefs=list(list(targets=1:ncol(designQuery),
                                                     className="dt-center")))
                )
            }else{ #Split
                designSplit <<- TRUE
                temp <- dplyr::filter(resultQuery, file_format==fileType)
                temp <- dplyr::filter(temp, dataset_type == dataType)
                acc <- unique(temp$accession)
            
                designQuery <<- createDesign(resultQuery, df,split=TRUE, format="long",
                                        fileFormat=fileType, dataset_type=dataType, 
                                        output_type="data.table", ID=IDs)
                lenExp <- length(designQuery)
                #Fetching file accession  in each data frame of the list
                acc_list <- vector("list", lenExp)
                acc_list <- lapply(designQuery, function(i){
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
                    output[[tablename]] <- DT::renderDataTable(designQuery[[my_i]],
                        options=list(searching=FALSE,pageLength = 25,
                                     scrollX = TRUE))})
            }
        }
        }
    })
    
    #Download request from queryEncode
    observeEvent(input$downloadFromQuery,{
        if(!viewDesign){
            selected_rows <- paste(input$resultQuery_rows_selected)
            selected_files <- resultQuery$file_accession[as.numeric(selected_rows)]
            if(length(unlist(selected_files)) > 0){
                downloadLog <- capture.output(downloadEncode(df=encode_df, file_acc=
                           unlist(selected_files)))}
        }else{
            if(!designSplit){
                selected_rows <- paste(input$designQuery_rows_selected)
                selected_files <- designQuery$File[as.numeric(selected_rows)]
            }else{
              unsplitDesign <- rbindlist(designQuery)
              acc <- unique(unsplitDesign$Experiment)
              row_to_get <- vector("list", length(acc))
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
              temp <- tools::file_path_sans_ext(basename(i))
              paste(unlist(strsplit(temp,"[.]"))[1])
            })
            
            if(length(selected_files) > 0){
                    downloadLog <- capture.output(downloadEncode(df=encode_df,
                                              file_acc=unlist(selected_files)))
            }
            
        }

        downloadLog <- gsub(x=downloadLog, pattern = "\\[\\d\\] ", replacement = "")
        downloadLog <- gsub(x=downloadLog, pattern='\"', replacement="")
        output$consoleQuery <- renderPrint(downloadLog)
    })
    
    #Refresh the content of fileSizeQuery
    refresh_query_display <- eventReactive(c(input$resultQuery_rows_selected,input$designQuery_rows_selected,
                                            input$designQuery, input$searchAdvanced, input$designSplitQuery),{
          if(!(viewDesign)){ #for fuzzySearch object
              if(length(input$resultQuery_rows_selected) > 0){
                  paste("Number of selected files :", length(input$resultQuery_rows_selected),
                  " Total size of the selected files :",file_size_query(input$resultQuery_rows_selected))
              }
          }else{ #for design object
              if(!input$splitFromQuery & length(input$designQuery_rows_selected) > 0 ){
                 paste("Number of selected files :",length(input$designQuery_rows_selected),
                 " Total size of the selected files :",file_size_query(input$designQuery_rows_selected,TRUE))
              }
          }
     })
    
    #Function that return the sum of selected files size
    file_size_query <- function(rows=NULL,is_design=FALSE){
      selected_rows <- paste(rows)
      #Getting selected row and thier size
      if(!is_design){
        selected_size <- resultQuery$file_size[as.numeric(selected_rows)]
      }else{ #Coming from a design
        selected_files <- designQuery$File[as.numeric(selected_rows)]
        selected_files <- sapply(selected_files, function (i){
          unlist(strsplit(i,"[.]"))[1]})
        selected_size <- dplyr::filter(encode_df, file_accession %in% selected_files)
        selected_size <- selected_size$file_size
      }
      
      #Converting all size in byte to apply sum
      selected_size <- sapply(selected_size,function(i){
        val <- unlist(strsplit(i," "))
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
    observeEvent(input$designFromDesign,{
        
        IDs <- c(as.numeric(input$repFromDesign), as.numeric(input$ctrlFromDesign))
        fileRDS <- readRDS(input$inputTable$datapath)
        typeFile <- c("bam", "fastq", "sam", "bed", "bigbed", "bigWig")
        typeFile <- typeFile[as.numeric(input$fileFromDesign)]
        formatDesignVec <- c("long", "wide")
        formatDesignVec <- formatDesignVec[as.numeric(input$formatFromDesign)]
        dataType <- c("experiments", "ucsc-browser-composites", "annotations",
                      "matched-sets", "projects", "reference-epigenomes",
                      "references")
        dataType <- dataType[as.numeric(input$datatypeFromDesign)]
        print(typeFile)
        print(dataType)
        print(formatDesignVec)
        #output$design <- DT::renderDataTable(as.data.table(fileRDS))
        output$design <- DT::renderDataTable(createDesign(input=fileRDS,
                                      df=df,split=input$split, fileFormat = typeFile,
                                      dataset_type=dataType, format=formatDesignVec, ID=IDs),
                                      options=list(searching=FALSE))
    })
    
}
