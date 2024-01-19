library("readxl")
library(stringr)
library(dtComb)
library(ggplot2)
library(dplyr)
library(shinyalert)
library(combinat) 
library(pROC)
library(OptimalCutpoints)
library(epiR)
library(DT)

options(shiny.maxRequestSize=30*1024^2)

source("helpers/helpers.R")
# updateSelectStatus <- function(input,selectName,eventName)
server <- function(input, output, session) {
  output$SensSpecPlot2 <- renderPlot({
    req(input$goButton)
    
    createSenSpePlot1(input = input,output = output,session)
    
  }, height = 400, width = 500)
  
  observeEvent(input$tabGroupDataUpload, {
    tabName <- input$tabGroupDataUpload
    
    # if(!is.null(session$userData$tabName) && session$userData$tabName != tabName){
    #   updateSelectInput(session,"event1",
    #                     "Select category for cases",
    #                     choices = c(""), selected = "")
    #   updateSelectInput(session,"event1Roc",
    #                     "Select category for cases",
    #                     choices = c(""), selected = "")
    #   updateSelectInput(session,"event2",
    #                     "Select category for cases",
    #                     choices = c(""), selected = "")
    #   updateSelectInput(session,"event2Roc",
    #                     "Select category for cases",
    #                     choices = c(""), selected = "")
    # }
    
    if(tabName == "ROC Analysis for Single Marker(s)")
    {
      updateSelectInput(session,"event1",
                        "Select category for cases",
                        selected = "")
      shiny::hideTab(inputId = "generalTabSetPanel", target = "Analysis")
      shiny::hideTab(inputId = "generalTabSetPanel", target = "Predict")
      shiny::showTab(inputId = "generalTabSetPanel", target = "ROC")
    }else{
      updateSelectInput(session,"event1Roc",
                        "Select category for cases",
                        selected = "")
      shiny::hideTab(inputId = "generalTabSetPanel", target = "ROC")
      shiny::showTab(inputId = "generalTabSetPanel", target = "Analysis")
      shiny::showTab(inputId = "generalTabSetPanel", target = "Predict")
    }
    
    session$userData$tabName <- tabName
    
  })
  observeEvent(input$selectMarker3Roc, {
    markerNames <- input$selectMarker3Roc
    getData(input,session)
    
    allReset <- FALSE
    if(!is.null(markerNames) && length(markerNames)>0 && !is.null(session$userData$oldMarkerNames) && length(session$userData$oldMarkerNames)>0){
      allReset <- FALSE
      
      if(length(markerNames) != length(session$userData$oldMarkerNames)){
        allReset <- TRUE
      }else{
        for (i in 1:length(markerNames)) {
          if(markerNames[i] != session$userData$oldMarkerNames[i])
            allReset<-TRUE
        }
      }
    }
    if(allReset){
      lapply(session$userData$oldMarkerNames,\(x){
        removeTab("tabGroupCoordinatesRoc",x)
        removeTab("tabGroupDiagStatRoc",x)
        removeTab("tabGroupCutPointsRoc",x)
      })
      
      session$userData$oldMarkerNames <- markerNames
    }
  })
  observeEvent(input$selectMarker1Roc, {
    markerNames <- input$selectMarker1Roc
    getData(input,session)
    allReset <- FALSE
    if(!is.null(markerNames) && length(markerNames)>0 && !is.null(session$userData$oldMarkerNames) && length(session$userData$oldMarkerNames)>0){
      
      if(length(markerNames) != length(session$userData$oldMarkerNames)){
        allReset <- TRUE
      }else{
        for (i in 1:length(markerNames)) {
          if(markerNames[i] != session$userData$oldMarkerNames[i])
            allReset<-TRUE
        }
      }
    }else if(is.null(markerNames) && !is.null(session$userData$oldMarkerNames)){
      allReset<-TRUE
    }
    if(allReset){
      lapply(session$userData$oldMarkerNames,\(x){
        removeTab("tabGroupCoordinatesRoc",x)
        removeTab("tabGroupDiagStatRoc",x)
        removeTab("tabGroupCutPointsRoc",x)
      })
      
      session$userData$oldMarkerNames <- markerNames
    }
  })
  observeEvent(input$functions, {
    
    selectedMethods <- as.matrix(methods[which(methods[,1] == input$functions),2])
    updateSelectInput(session,"methods",
                      "Combination Method",
                      choices = selectedMethods)
    createElements(input, output,session)
  })
  observeEvent(input$methods, {
    test <- input$methods
    if(is.null(input$methods))
      return()
    if(input$methods == "")
      return()
    if(!is.null(session$userData$subMethodElementsAppenedList) && session$userData$subMethodElementsAppendIsOk == TRUE)
      return()
    
    session$userData$subMethodElementsAppendIsOk <- TRUE
    session$userData$subFunctionElementsAppendIsOk <- TRUE
    subFunctionElements <- session$userData$subFunctionElementsAppenedList
    
    if(length(subFunctionElements)>0){
      for (i in c(1:nrow(subFunctionElements))) {
        creatrFunctionSubParams(
          functionName = subFunctionElements$functionName[i],
          name = subFunctionElements$name[i],
          valueName = subFunctionElements$value[i],
          formatSubElemName = subFunctionElements$subElementName[i],
          appenedConditionName = subFunctionElements$appenedConditionName[i]
        )
        
      }
    }
    
    subElementsAppenedList <- session$userData$subMethodElementsAppenedList
    if(length(subElementsAppenedList)>0){
      
      for (i in c(1:nrow(subElementsAppenedList))) {
        functionName <- subElementsAppenedList$functionName[i]
        methodName<- subElementsAppenedList$methodName[i]
        createMethodSubParams(subElementsAppenedList$functionName[i],
                              subElementsAppenedList$methodName[i],
                              subElementsAppenedList$name[i],
                              subElementsAppenedList$value[i],
                              subElementsAppenedList$subElementName[i],
                              subElementsAppenedList$appenedConditionName[i])
      }
    }
  })
  observeEvent(input$dataInput,{
    
    if(input$dataInput == "1"){
      dataInput = input$dataInput
      
      
    }else{
      dataInput = input$dataInput
      
      data <- session$userData$data
      
      
    }
    
  })
  observeEvent(input$selectStatus1, {
    
    if(is.null(input$selectStatus1)){
      updateSelectInput(session,"event1",
                        "Select category for cases",
                        selected = "")
      return()
    }
    
    data <-  session$userData$data
    status <- data[[input$selectStatus1]]
    statusLevels<-as.factor(status)
    updateSelectInput(session,"event1",
                      "Select category for cases",
                      choices = c("",    levels(statusLevels))
                      
    )
  })
  observeEvent(input$selectStatus1Roc, {
    req(input$selectStatus1Roc)
    if(is.null(input$selectStatus1Roc)){
      updateSelectInput(session,"event1Roc",
                        "Select category for cases",
                        choices = c("", ))
      return()
    }
    
    data <-  session$userData$data
    status <- data[[input$selectStatus1Roc]]
    statusLevels<-as.factor(status)
    updateSelectInput(session,"event1Roc",
                      "Select category for cases",
                      choices = c("",    levels(statusLevels))
                      
    )
  })
  
  observeEvent(input$selectStatus2, {
    req(input$selectStatus2)
    sampleData <- input$sampleData
    
    if(is.null(input$selectStatus2))
      return()
    data <-  session$userData$data
    status <- data[[input$selectStatus2]]
    statusLevels<-as.factor(status)
    updateSelectInput(session,"event2",
                      "Select category for cases",
                      choices = levels(statusLevels)
    ) })
  observeEvent(input$selectStatus2Roc, {
    req(input$selectStatus2Roc)
    
    if(is.null(input$selectStatus2Roc))
      return()
    data <-  session$userData$data
    status <- data[[input$selectStatus2Roc]]
    statusLevels<-as.factor(status)
    updateSelectInput(session,"event2Roc",
                      "Select category for cases",
                      choices = levels(statusLevels)
    ) })
  
  output$RawData <- DT::renderDataTable({
    
    
    data <- getData(input,session)
    dataHeader <- colnames(data) 
    tabName <- input$tabGroupDataUpload
    
    if(input$dataInput == "1"){
      if(tabName == "ROC Analysis for Single Marker(s)"){
        updateSelectInput(session,"selectMarker1Roc",
                          "Select markers (*)",
                          choices = c("",dataHeader))
        updateSelectInput(session,"selectStatus1Roc",
                          "Status",
                          choices = c("",dataHeader)
        )
      }else{
        if(length(dataHeader) < 3){
          shinyalert(
            title = "Attention",
            text = "Combination methods are not used with a single marker. You are redirected to the ROC Analysis for Single Marker(s) tab. You can perform ROC Analysis of this marker.",
            size = "s", 
            closeOnEsc = TRUE,
            closeOnClickOutside = FALSE,
            html = FALSE,
            type = "error",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = TRUE
          )
          updateTabsetPanel(session, "tabGroupDataUpload",
                            selected = "ROC Analysis for Single Marker(s)")
        }
        else{
          updateSelectInput(session,"selectMarker1",
                            "Marker 1",
                            choices = c("",dataHeader))
          updateSelectInput(session,"selectMarker2",
                            "Marker 2",
                            choices = c("",dataHeader))
          updateSelectInput(session,"selectStatus1",
                            "Status",
                            choices = c("",dataHeader)
          )
        }
        
      }
      
    }else{
      if(tabName == "ROC Analysis for Single Marker(s)"){
        sampleDataRoc <- input$sampleDataRoc
        
        if(!is.null(session$userData$sampleDataRoc) && session$userData$sampleDataRoc == sampleDataRoc){
          
        }
        else if (sampleDataRoc == 1){
          updateSelectInput(session,"selectStatus2Roc",
                            "Status",
                            choices = dataHeader
                            ,selected = "group")
          
          updateSelectInput(session,"selectMarker3Roc",
                            "Select markers (*)",
                            choices = dataHeader)
          
        }
        else if (sampleDataRoc == 2){
          updateSelectInput(session,"selectStatus2Roc",
                            "Status",
                            choices = dataHeader,
                            selected = "Group")                           
          updateSelectInput(session,"selectMarker3Roc",
                            "Select markers (*)",
                            choices =  dataHeader)
        }
        else if (sampleDataRoc == 3){
          updateSelectInput(session,"selectStatus2Roc",
                            "Status",
                            choices = dataHeader,
                            selected = "status")                           
          updateSelectInput(session,"selectMarker3Roc",
                            "Select markers (*)",
                            choices =  dataHeader)
        }
        
        session$userData$sampleDataRoc <- sampleDataRoc
      }else{
        sampleData <- input$sampleData
        
        if(!is.null(session$userData$sampleData) && session$userData$sampleData == sampleData){
          
        }
        else if (sampleData == 1){
          updateSelectInput(session,"selectStatus2",
                            "Status ",
                            choices = dataHeader
                            ,selected = "group"
          )                           
          updateSelectInput(session,"selectMarker3",
                            "Marker 1",
                            choices = dataHeader
                            ,selected = "ddimer")
          updateSelectInput(session,"selectMarker4",
                            "Marker 2",
                            choices = dataHeader,
                            selected = "log_leukocyte"
          )}
        else if (sampleData == 2){
          updateSelectInput(session,"selectStatus2",
                            "Status",
                            choices = dataHeader,
                            selected = "Group")                           
          updateSelectInput(session,"selectMarker3",
                            "Marker 1",
                            choices =  dataHeader,
                            selected = "m1")
          updateSelectInput(session,"selectMarker4",
                            "Marker 2",
                            choices = dataHeader,
                            selected = "m2")
        }
        else if (sampleData == 3){
          updateSelectInput(session,"selectStatus2",
                            "Status",
                            choices = dataHeader,
                            selected = "status")                           
          updateSelectInput(session,"selectMarker3",
                            "Marker 1",
                            choices =  dataHeader,
                            selected = "marker1")
          updateSelectInput(session,"selectMarker4",
                            "Marker 2",
                            choices = dataHeader,
                            selected = "marker2")
        }
        
        session$userData$sampleData <- sampleData
      }
      
      
    }
    return(data)
    
  },
  extensions = 'Buttons',
  options = options
  )
  
  output$SensSpecPlot2 <- renderPlot({
    req(input$goButton)
    
    
    createSenSpePlot1(input = input,output = output,session)
    
  }, height = 400, width = 500)
  
  output$SensSpecPlot3 <- renderPlot({
    req(input$goButton)
    
    
    createSenSpePlot2(input = input,output = output,session)
    
  }, height = 400, width = 500)
  
  output$SensSpecPlotC <- renderPlot({
    req(input$goButton)
    
    
    createSenSpePlotC(input = input,output = output,session)
    
  }, height = 400, width = 500)
  
  output$Distplot2 <- renderPlot({
    req(input$goButton)
    
    
    createDistPlot1(input = input,output = output,session)
    
  }, height = 400, width = 500)
  
  output$Distplot3 <- renderPlot({
    req(input$goButton)
    
    
    createDistPlot2(input = input,output = output,session)
    
  }, height = 400, width = 500)
  
  output$DistplotC <- renderPlot({
    req(input$goButton)
    
    
    createDistPlotC(input = input,output = output,session)
    
  }, height = 400, width = 500)
  
  output$Sctplot2 <- renderPlot({
    req(input$goButton)
    
    
    createSctPlot1(input = input,output = output,session)
    
  }, height = 400, width = 500)
  
  output$Sctplot3 <- renderPlot({
    req(input$goButton)
    
    
    createSctPlot2(input = input,output = output,session)
    
  }, height = 400, width = 500)
  
  output$SctplotC <- renderPlot({
    req(input$goButton)
    createSctPlotC(input = input,output = output,session)
    
  }, height = 400, width = 500)
  
  observeEvent(input$goButton, {
    updateTabsetPanel(session, "tabGroupPlot",
                      selected = "Plots"
    )
    
    output$ROCplot <- renderPlot({
      hush(createROCPlot(input = input,output = output,session))
    }, height = 400, width = 500)
    
    
  }) 
  
  observeEvent(input$goButtonAnalysis, {
    if(input$dataInput == 1 && (is.null(input$selectStatus1) || is.null(input$selectMarker1)||
                                is.null(input$selectMarker2) || is.null(input$event1) ||
                                (input$selectStatus1 == "") || (input$selectMarker2 == "") ||
                                (input$selectMarker1 == "") || (input$event1 == ""))){
      shinyalert(
        title = "Attention",
        text = "Parameters check",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      return()
    }
    updateTabsetPanel(session, "generalTabSetPanel",
                      selected = "Analysis"
    )
  }) 
  observeEvent(input$goButtonRoc, {
    if(input$dataInput == 1 && (is.null(input$selectStatus1Roc) || is.null(input$selectMarker1Roc) || is.null(input$event1Roc) 
                                || (input$selectStatus1Roc == "") || (input$event1Roc == ""))){
      shinyalert(
        title = "Attention",
        text = "Parameters check",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      return()
    }
    if(input$dataInput == 2 && (is.null(input$selectStatus2Roc) || is.null(input$selectMarker3Roc) || is.null(input$event2Roc) 
                                || (input$selectStatus2Roc == "") || (input$event2Roc == ""))){
      shinyalert(
        title = "Attention",
        text = "Parameters check",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      return()
    }
    isActive <- controlMarkerCount(input)
    if(isActive){
      lapply(session$userData$oldMarkerNames,\(x){
        removeTab("tabGroupCoordinatesRoc",x)
        removeTab("tabGroupDiagStatRoc",x)
        removeTab("tabGroupCutPointsRoc",x)
      })
      updateTabsetPanel(session, "generalTabSetPanel",
                        selected = "ROC"
      )
      updateTabsetPanel(session, "tabGroupPlotRoc",
                        selected = "Plots"
      )
      output$ROCplotRoc <- renderPlot({
        hush(createROCPlotRoc(input = input,output = output,session))
      }, height = 400, width = 500)
    }else {
      shinyalert(
        title = "Attention",
        text = "A maximum of 8 markers can be selected.",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  }) 
  observeEvent(input$calculate,{
    output$warningMessage <- renderText({})
    
    if(input$uploadModel == 1){
      
      
      if(is.null(input$modelUpload)) {
        
        return()
      } else {
        model <- readRDS(input$modelUpload$datapath)
      }
    }else{
      model <- session$userData$model
      if(is.null(model)){
        
        return()
      }
    }
    if(is.null(input$newUpload)) {
      
      return()
    } else {
      delimiter <- " "
      if(input$fileSepDF2 == 1){
        delimiter <- ","
      }else if(input$fileSepDF2 == 2){
        delimiter <- "\t"
      }else if(input$fileSepDF2 == 3){
        delimiter <- ";"
      }else if(input$fileSepDF2 == 4){
        delimiter <- " "
      }
      data <- read.table(input$newUpload$datapath,
                         header = TRUE,
                         sep = delimiter)
      data[,1] <- as.numeric(data[, 1])
      data[,2] <- as.numeric(data[, 2])
    }
    
    updateTabsetPanel(session, "tabGroupres",
                      selected = "Result"
    )
    output$outputPredict <- DT::renderDataTable({
      
      predict(model,data)
      
    },extensions = 'Buttons',options = options)
    
  })
  output$TestData <- DT::renderDataTable({
    req(input$newUpload)
    delimiter <- " "
    fileSepDF2 <- input$fileSepDF2
    if(input$fileSepDF2 == "1"){
      delimiter <- ","
    }else if(input$fileSepDF2 == "2"){
      delimiter <- "\t"
    }else if(input$fileSepDF2 == "3"){
      delimiter <- ";"
    }else if(input$fileSepDF2 == "4"){
      delimiter <- " "
    }
    data<-read.table(input$newUpload$datapath,
                     header = TRUE,
                     sep =  delimiter) 
  } ,extensions = 'Buttons',options = options)
}


