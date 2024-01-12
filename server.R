library("readxl")
library(stringr)
library(dtComb)
library(ggplot2)
library(dplyr)
  

options(shiny.maxRequestSize=30*1024^2)


load("parameters.Rdata")
functions <- parameters$functions

methods <- parameters$methods
functionParameters <- parameters$functionParameters
functionSubParameters <- parameters$functionSubParameters
methodParameters <- parameters$methodParameters
generalParameters <- parameters$generalParameters
realNames <- parameters$realNames


options = 
  list(
    dom = 'Blfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    lengthMenu = list(c(10, 30, 50, -1),
                      c('10', '30', '50', 'All')),
    paging = T,columnDefs = list(list(className = 'dt-center',targets="_all")))

createElements <- function(input,output,session){
  
  if(!is.null(session$userData$readExcels) && session$userData$readExcels == TRUE)
    return()
  
  session$userData$readExcels <- TRUE
  
  subMethodElementsAppenedList <- data.frame()
  subFunctionElementsAppenedList <- data.frame()
  for (variable in functions[, 1]) {
    functionResponse <- createFunctionParams(variable,input,output,session)
    subFunctionElementsAppenedList <- rbind(subFunctionElementsAppenedList,functionResponse)
    selectedMethods <- as.matrix(methods[which(methods[, 1] == variable), ])
    for (methodName in selectedMethods[, 2]) 
    {
      response <- createMethodParams(variable, methodName, input, output, session)
      subMethodElementsAppenedList <- rbind(subMethodElementsAppenedList, response)
    }
    
  }
  session$userData$subMethodElementsAppenedList <- subMethodElementsAppenedList
  session$userData$subFunctionElementsAppenedList <- subFunctionElementsAppenedList
  session$userData$subFunctionElementsAppendIsOk <- FALSE
  
  session$userData$subMethodElementsAppendIsOk <- FALSE
  
}
createMethodParams <- function(functionName, methodName, input, output, session){
  funcParams <- methodParameters[which(methodParameters[, 1] == methodName),]
  inputElements <- list()
  subElementsAppenedList <- data.frame()
  
  if(nrow(as.matrix(funcParams)) == 0)
    return()
  for (i in c(1:nrow(funcParams))) {
    name <- as.character(funcParams[i, 2])
    type <- as.character(funcParams[i, 3])
    subElementName <-paste0(formatName(name = functionName), formatName(name = methodName), formatName(name = name))
    if(type == "select"){
      values <- as.matrix(unlist(strsplit(as.character(funcParams[i, 4]), ',', fixed = T)))
      if(length(values) > 0){
        inputElements <- append(inputElements, list(selectInput(subElementName, name,
                                                                choices = values,
                                                                selected = FALSE,
                                                                multiple = FALSE)
        ))
        for (value in values) {
          selectSubElems <- methodParameters[which(methodParameters[, 1] == value),]
          if(nrow(selectSubElems) > 0){
            subElementsAppenedList <- rbind(subElementsAppenedList, data.frame(
              functionName = functionName,
              methodName = methodName,
              name = name,
              value = value,
              subElementName = subElementName,
              appenedConditionName = formatName(paste0("hdn",functionName,methodName))
            ))
          }
        }
      }
    }
    else if(type == "number"){
      minValue <-  as.numeric(funcParams[i, 6])
      maxValue <-  as.numeric(funcParams[i, 7])
      defaultValue <-  as.numeric(funcParams[i, 5])
      if(is.null(defaultValue)){
        defaultValue <- 0
      }
      step <- 1
      if(name == "Mixing parameter"){
        step <- 0.1
      }
      inputElements <- append(inputElements,list(numericInput(subElementName, name,
                                                              defaultValue,
                                                              min = minValue,
                                                              max = maxValue, 
                                                              step = step)))
    }
    else if(type == "radio"){
      inputElements <- append(inputElements,list(
        radioButtons(subElementName, name,
                     list("True" = TRUE, "False" = FALSE),
                     selected = TRUE)
      ))
    }
  }
  conditionName = paste0("input.methods == '",methodName,"'")
  
  inputElements <- append(inputElements, list(uiOutput(formatName(paste0("hdn", functionName, methodName)))))
  x <- sample(1:1000, 1)
  insertUI("#hiddenElementMethodSub", where = "beforeBegin", ui = tags$div(id = paste0("hiddenElementFunction", x) ,conditionalPanel(condition = conditionName, inputElements)))
  
  return(subElementsAppenedList)
}
createMethodSubParams <- function(functionName,methodName,name,valueName,formatSubElemName,appenedConditionName){
  selectSubElems <- methodParameters[which(methodParameters[,1] == valueName),]
  if(nrow(as.matrix(selectSubElems))>0){
    
    subInputElements <- list()
    for (j in c(1:nrow(selectSubElems))) {
      selectSubElemName <- selectSubElems[j,2]
      selectSubElemType <- selectSubElems[j,3]
      subFunctionElementName <-paste0(formatName(name = functionName),formatName(methodName),formatName(name = name),formatName(valueName),formatName(selectSubElemName))
      if(selectSubElemType == "select"){
        values <- as.matrix(unlist(strsplit(as.character(funcParams[j,4]), ',', fixed=T)))
        if(length(values)>0){
          subInputElements <- append(subInputElements,list(selectInput(subFunctionElementName, selectSubElemName,
                                                                       choices = values,
                                                                       selected = FALSE,
                                                                       multiple = FALSE)
          ))
        }
      }
      else if(selectSubElemType == "number"){
        minValue <-  as.numeric(selectSubElems[j, 6])
        maxValue <-  as.numeric(selectSubElems[j, 7])
        defaultValue <-  as.numeric(selectSubElems[j, 5])
        if(is.null(defaultValue)){
          defaultValue <- 0
        }
        
        subInputElements <- append(subInputElements,list(numericInput(subFunctionElementName, selectSubElemName,
                                                                      defaultValue,
                                                                      min = minValue,
                                                                      max = maxValue)))
        
      }
      else if(selectSubElemType == "radio"){
        subInputElements <- append(subInputElements,list(
          radioButtons(subFunctionElementName, selectSubElemName,
                       list("True" = TRUE, "False" = FALSE),
                       selected = TRUE)
        ))
      }
    }
    subConditionElementName <-paste0(formatName(name = functionName),formatName(methodName),formatName(name = name))
    
    
    subConditionName = paste0("input.",subConditionElementName," == '",valueName,"'")
    insertUI(paste0("#",appenedConditionName),where = "afterBegin",ui = tags$div(id="hiddenElementMethodsSub",conditionalPanel(condition = subConditionName,subInputElements)))
  }
  
}
createFunctionParams <- function(functionName,input,output,session){
  subElementsAppenedList <- data.frame()
  
  funcParams <- functionParameters[which(functionParameters[,1] == functionName),]
  inputElements <- list()
  for (i in c(1:nrow(as.matrix(funcParams)))) {
    name <- as.character(funcParams[i,2])
    type <- as.character(funcParams[i,3])
    
    subElementName <-paste0(formatName(name = functionName),formatName(name = name))
    if(type == "select"){
      values <- as.matrix(unlist(strsplit(as.character(funcParams[i,4]), ',', fixed=T)))
      if(length(values)>0){
        inputElements <- append(inputElements,list(selectInput(subElementName, name,
                                                               choices = values,
                                                               selected = FALSE,
                                                               multiple = FALSE)
        ))
        for (value in values) {
          selectSubElems <- functionSubParameters[which(functionSubParameters[,1] == value),]
          if(nrow(selectSubElems)>0){
            subElementsAppenedList <- rbind(subElementsAppenedList, data.frame(
              functionName = functionName,
              name = name,
              value = value,
              subElementName = subElementName,
              appenedConditionName = formatName(paste0("hdn",functionName))
            ))
            # creatrFunctionSubParams(functionName,name,value,subElementName)
          }
        }
        
      }
    }
    else if(type == "number"){
      
      minValue <-  as.numeric(funcParams[i, 6])
      maxValue <-  as.numeric(funcParams[i, 7])
      defaultValue <-  as.numeric(funcParams[i, 5])
      if(is.null(defaultValue)){
        defaultValue <- 0
      }
      inputElements <- append(inputElements,list(numericInput(subElementName, name,
                                                              defaultValue,
                                                              min = minValue,
                                                              max = maxValue)))
      
      
    }
    else if(type == "radio"){
      inputElements <- append(inputElements,list(
        radioButtons(subElementName, name,
                     list("True" = TRUE, "False" = FALSE),
                     selected = TRUE)
      ))
    }
  }
  
  conditionName = paste0("input.functions == '",functionName,"'")
  inputElements <- append(inputElements, list(uiOutput(formatName(paste0("hdn", functionName)))))
  x <- sample(1:1000, 1)
  insertUI("#hiddenElement",where = "beforeBegin",ui = tags$div(id=paste0("hiddenElementFunction", x),conditionalPanel(condition = conditionName,inputElements)))
  return(subElementsAppenedList)
}

creatrFunctionSubParams <- function(functionName,name,valueName,formatSubElemName,appenedConditionName){
  selectSubElems <- functionSubParameters[which(functionSubParameters[,1] == valueName),]
  if(nrow(as.matrix(selectSubElems))>0){
    
    subInputElements <- list()
    for (j in c(1:nrow(selectSubElems))) {
      selectSubElemName <- selectSubElems[j,2]
      selectSubElemType <- selectSubElems[j,3]
      subFunctionElementName <-paste0(formatName(name = functionName),formatName(name = name),formatName(selectSubElemName),formatName(valueName))
      if(selectSubElemType == "select"){
        values <- as.matrix(unlist(strsplit(as.character(funcParams[j,4]), ',', fixed=T)))
        if(length(values)>0){
          subInputElements <- append(subInputElements,list(selectInput(subFunctionElementName, selectSubElemName,
                                                                       choices = values,
                                                                       selected = FALSE,
                                                                       multiple = FALSE)
          ))
        }
      }
      else if(selectSubElemType == "number"){
        minValue <-  as.numeric(selectSubElems[j, 6])
        maxValue <-  as.numeric(selectSubElems[j, 7])
        defaultValue <-  as.numeric(selectSubElems[j, 5])
        if(is.null(defaultValue)){
          defaultValue <- 0
        }
        
        subInputElements <- append(subInputElements,list(numericInput(subFunctionElementName, selectSubElemName,
                                                                      defaultValue,
                                                                      min = minValue,
                                                                      max = maxValue)))
        
      }
      else if(selectSubElemType == "radio"){
        subInputElements <- append(subInputElements,list(
          radioButtons(subFunctionElementName, selectSubElemName,
                       list("True" = TRUE, "False" = FALSE),
                       selected = TRUE)
        ))
      }
    }
    subConditionElementName <-paste0(formatName(name = functionName),formatName(name = name))
    x <- sample(1:1000, 1)
    subConditionName = paste0("input.",subConditionElementName," == '",valueName,"'")
    insertUI(paste0("#",appenedConditionName),where = "afterBegin",
             ui = tags$div(id=paste0("hiddenElementFunction", x),conditionalPanel(condition = subConditionName,subInputElements)))
  }
  
}

formatName <- function(name){
  name <- gsub("-", "",name)
  name <- gsub(" ", "",name)
  name <- gsub("&", "",name)
  name <- gsub("_", "",name)
  name <- gsub(",", "",name)
  return(name)
}
getRealName <- function(name){
  realName <-  realNames[which(realNames[,1] == name),2]
}

getData<- function(input,session){
  dataInput <- input$dataInput
  sampleData <- input$sampleData
  
  
  if(dataInput == "1"){
    req(input$upload)
    
    delimiter <- " "
    if(input$fileSepDF == 1){
      delimiter <- ","
    }else if(input$fileSepDF == 2){
      delimiter <- "\t"
    }else if(input$fileSepDF == 3){
      delimiter <- ";"
    }else if(input$fileSepDF == 4){
      delimiter <- " "
    }
    data<-read.table(input$upload$datapath,
                     header = TRUE,
                     sep = delimiter) 
    dataHeader <- colnames(data)
    session$userData$dataHeader <- colnames(data)
  }
  else{
    if (sampleData == 1){
      data <- exampleData1
      dataHeader <- colnames(data)
      session$userData$dataHeader <- colnames(data)
    }
    else if (sampleData == 2){
      data <- exampleData2
      dataHeader <- colnames(data)
      session$userData$dataHeader <- colnames(data)
    } else if(sampleData == 3){
      data <- exampleData3
      dataHeader <- colnames(data)
      session$userData$dataHeader <- colnames(data)
      
    }
    
  }
  
  session$userData$data <- data
  
  updateSelectInput(session, "slctColNames",
                    "Select Plot variable",
                    choices = c("",dataHeader)
  )
  return(data)
}

createROCPlot <- function(input, output, session){
  library(dtComb)
  
  rawData <- session$userData$data
  dataInput <- input$dataInput
  
  dataHeader <- colnames(rawData)
  
  
  if(dataInput == "1"){
    marker1 <- as.numeric(rawData[[input$selectMarker1]])
    marker2 <-as.numeric(rawData[[input$selectMarker2]])
    
    event <- input$event1
    status <- rawData[[input$selectStatus1]]
    data <- data.frame(status,marker1, marker2)
    colnames(data) = c( input$selectStatus1,input$selectMarker1, input$selectMarker2)
    
  }
  else{
    marker1 <- rawData[[input$selectMarker3]]
    marker2 <- rawData[[input$selectMarker4]]
    
    event <- input$event2
    status <- rawData[[input$selectStatus2]]
    data <- data.frame(status,marker1, marker2)
    colnames(data) = c( input$selectStatus2,input$selectMarker3, input$selectMarker4)
    
  }  
  
  session$userData$data <- data
  
  markers <- as.data.frame(data[, -1])
  status <- as.factor(data[, 1])
  cutoff.method <- input$cutoffmethod
  direction <- input$direction
  conf.level <- input[["conflevel"]]
  method <- input$methods
  functionName <- input$functions
  resampleName <- formatName(paste0(functionName,method,"Resampling method"))
  resample <- input[[resampleName]]
  if(is.null(resample))
    resample <- "none"
  standardizeName <- formatName(paste0(functionName,"Standardization method"))
  standardize <- input[[standardizeName]]
  if(is.null(standardize))
    standardize<-"none"
  else
    standardize <- getRealName(standardize)
  nfoldsName <- formatName(paste0(resampleName,resample,"Number of folds"))
  nfolds <- input[[nfoldsName]]
  nrepatsName <- formatName(paste0(resampleName,resample,"Number of repeats"))
  nrepeats <- input[[nrepatsName]]
  nitersName <- formatName(paste0(resampleName,resample,"Number of resampling iterations"))
  niters <- input[[nitersName]]
  
  if(functionName == "Linear Combination Methods"){
    
    ndigitsName <- formatName(paste0(functionName,method,"Number of digit"))
    ndigits <- input[[ndigitsName]]
    if(is.null(ndigits)){
      ndigits <- 0
    }
    strMethod <- getRealName(method)
    strResample <- getRealName(resample)
    modelFit<- linComb(markers = markers,
                       status = status,
                       event = event,
                       method = getRealName(method),
                       resample = getRealName(resample),
                       show.plot = TRUE,
                       standardize = standardize,
                       direction = direction,
                       cutoff.method = cutoff.method,
                       nfolds = nfolds,
                       nrepeats =nrepeats,
                       niters = niters,
                       ndigits = ndigits,
                       conf.level = conf.level
    )
    
    
  }
  else if(functionName == "Nonlinear Combination Methods"){
    degree1Name <- formatName(paste0(functionName,method,"Number of degrees marker 1"))
    degree2Name <- formatName(paste0(functionName,method,"Number of degrees marker 2"))
    degree1 <-  input[[degree1Name]]
    degree2 <-  input[[degree2Name]]
    df1Name <- formatName(paste0(functionName,method,"Number of degrees of freedom for marker 1"))
    df2Name <- formatName(paste0(functionName,method,"Number of degrees of freedom for marker 2"))
    df1 <-  input[[df1Name]]
    df2 <-  input[[df2Name]]
    interactName <- formatName(paste0(functionName,method,'Include of interaction'))
    include.interact <- input[[interactName]] 
    alphaName <- formatName(paste0(functionName,method,"Mixing parameter"))
    alpha <- input[[alphaName]] 
    modelFit <- nonlinComb(markers = markers, status = status, event = event,
                           method = getRealName(method), degree1 = degree1, degree2 = degree2, 
                           include.interact = include.interact, cutoff.method = cutoff.method, 
                           resample = getRealName(resample), direction = direction, conf.level = conf.level,
                           standardize = standardize, df1 = df1, df2 = df2, alpha = alpha,
                           nfolds = nfolds,
                           nrepeats = nrepeats,
                           niters = niters)
  }
  
  else if(functionName == "Machine-Learning Algorithms"){
    preProcessName <- formatName(paste0(functionName,"Data Pre-processing"))
    preProcess <- input[[preProcessName]]
    if(preProcess == "None"){
      preProcess = NULL
    }
    method <- getRealName(method)
    modelFit <- mlComb(markers = markers, status = status, event = event,
                       method = method, resample = resample, nfolds = nfolds,
                       nrepeats = nrepeats, niters = niters, preProcess = preProcess, 
                       direction = direction, cutoff.method = cutoff.method)
    
    
  }
  else if(functionName == "Mathematical Operators"){
    
    distanceName <- formatName(paste0(functionName,method,"Distance measures"))
    distance <- input[[distanceName]]
    distance <- getRealName(distance)
    transformName <- formatName(paste0(functionName,"Transformation"))
    transform <-  input[[transformName]]
    transform <- getRealName(transform)
    modelFit <- mathComb(markers = markers,
                         status = status,
                         event = event,
                         method = getRealName(method),
                         distance = distance,
                         standardize = standardize,
                         transform = transform,
                         show.plot = TRUE,
                         direction = direction,
                         conf.level = conf.level,
                         cutoff.method = cutoff.method
    )
    
  }
  session$userData$model <- modelFit
  createAnalysis(input = input,output = output,session)
  output$downloadModel <- downloadHandler(
    filename = function() {
      paste("modelFit", "Rdata", sep = ".") 
    },
    content = function(file) {
      saveRDS(modelFit,file)
    }
  )
  
}
createDistPlot1 <- function(input, output, session){
  
  dataInput <- input$dataInput
  
  data <-  session$userData$data
  model <-  session$userData$model
  
  data[, 1] <- factor(data[, 1], levels = unique(data[, 1]))
  
  marker1Data = cbind(as.data.frame(data[, 2]), as.data.frame(data[, 1]))
  
  names(marker1Data) <- c("Marker1", "Labels")
  
  ggplot2::ggplot(marker1Data, ggplot2::aes(x=Marker1, 
                                            colour=Labels)) + 
    ggplot2::xlab("Marker 1") +
    ggplot2::ylab("Density") +
    ggplot2::geom_density(size = 2) +
    ggplot2::ggtitle("Distribution Plot") +
    ggplot2::geom_vline(xintercept = model$ThresholdMarker1, linetype = "dotted") +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::theme(text = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20)) +
    ggplot2::theme(legend.position = "bottom")
  
}

createDistPlot2 <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  
  data[, 1] <- factor(data[, 1], levels = unique(data[, 1]))
  
  marker2Data = cbind(as.data.frame(data[, 3]), as.data.frame(data[, 1]))
  
  names(marker2Data) <- c("Marker2", "Labels")
  
  ggplot2::ggplot(marker2Data, ggplot2::aes(x=Marker2, 
                                            colour=Labels)) + 
    ggplot2::xlab("Marker 2") +
    ggplot2::ylab("Density") +
    ggplot2::geom_density(size = 2) +
    ggplot2::ggtitle("Distribution Plot") +
    ggplot2::geom_vline(xintercept = model$ThresholdMarker2, linetype = "dotted") +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::theme(text = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20)) +
    ggplot2::theme(legend.position = "bottom")
  
}

createDistPlotC <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  
  event <-session$userData$event
  
  status <- factor(data[, 1], levels = unique(data[, 1]))
  
  if(is.null(session$userData$plotC)){
    
    session$userData$plotC <- plotComb(model, status)
    
  }
  session$userData$plotC$plotDensity
  
}

createSctPlot1 <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  
  data[, 1] <- factor(data[, 1], levels = unique(data[, 1]))
  
  marker1Data = cbind(as.data.frame(data[, 2]), as.data.frame(data[, 1]))
  
  names(marker1Data) <- c("Marker1", "Labels")
  
  
  ggplot2::ggplot(marker1Data, ggplot2::aes(x=Labels, y=Marker1,  
                                            color=Labels)) + ggplot2::geom_point(size = 3) +
    ggplot2::ylab("Marker 1") +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_jitter(width = 0.40) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Scatter Plot") +
    ggplot2::geom_hline(yintercept = model$ThresholdMarker1, linetype = "dotted") +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::theme(text = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20)) +
    ggplot2::theme(legend.position = "bottom")
  
  
}

createSctPlot2 <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  
  data[, 1] <- factor(data[, 1], levels = unique(data[, 1]))
  
  marker2Data = cbind(as.data.frame(data[, 3]), as.data.frame(data[, 1]))
  
  names(marker2Data) <- c("Marker2", "Labels")
  
  
  ggplot2::ggplot(marker2Data, ggplot2::aes(x=Labels, y=Marker2,  
                                            color=Labels)) + ggplot2::geom_point(size = 3) +
    ggplot2::ylab("Marker 2") +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_jitter(width = 0.40) +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Scatter Plot") +
    ggplot2::geom_hline(yintercept = model$ThresholdMarker2, linetype = "dotted") +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20)) +
    ggplot2::theme(text = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20)) +
    ggplot2::theme(legend.position = "bottom")
  
}

createSctPlotC <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  event <-session$userData$event
  
  status <- factor(data[, 1], levels = unique(data[, 1]))
  
  if(is.null(session$userData$plotC)){
    
    session$userData$plotC <- plotComb(model, status)
    
  }
  session$userData$plotC$plotScatter
  
}

createSenSpePlot1 <- function(input, output, session){
  
  model <-  session$userData$model
  
  results <- model$ROC_coordinates
  markers <- session$userData$data
  
  coord = results[results[ ,"Marker"] == colnames(markers)[2], ]
  
  colors <- c("Sensitivity" = "#f8766d", "Specificity" = "#00bfc4")
  
  ggplot2::ggplot(coord, ggplot2::aes(x=Threshold)) + 
    ggplot2::geom_line(ggplot2::aes(y = Sensitivity, color = "Sensitivity"), show.legend = TRUE, size = 2) +
    ggplot2::geom_line(ggplot2::aes(y = Specificity, color = "Specificity"), show.legend = TRUE, size = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::theme(text = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20)) +
    ggplot2::ggtitle("Sensitivity&Specificity Plot") +
    ggplot2::labs(y = "Value", x = "Marker 1", color = "Labels") +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::geom_vline(xintercept=model$ThresholdMarker1, linetype='dotted') +
    ggplot2::theme(legend.position = "bottom")
  
}

createSenSpePlot2 <- function(input, output, session){
  
  model <-  session$userData$model
  
  results <- model$ROC_coordinates
  markers <- session$userData$data
  coord = results[results[ ,"Marker"] == colnames(markers)[3], ]
  
  colors <- c("Sensitivity" = "#f8766d", "Specificity" = "#00bfc4")
  
  ggplot2::ggplot(coord, ggplot2::aes(x=Threshold)) + 
    ggplot2::geom_line(ggplot2::aes(y = Sensitivity, color = "Sensitivity"), show.legend = TRUE, size = 2) +
    ggplot2::geom_line(ggplot2::aes(y = Specificity, color = "Specificity"), show.legend = TRUE, size = 2) +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::theme(text = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 20)) +
    ggplot2::ggtitle("Sensitivity&Specificity Plot") +
    ggplot2::labs(y = "Value", x = "Marker 2", color = "Labels") +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::geom_vline(xintercept=model$ThresholdMarker2, linetype='dotted') +
    ggplot2::theme(legend.position = "bottom")
  
}

createSenSpePlotC <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  event <-session$userData$event
  
  
  data[,3] = as.numeric(data[,3])
  
  status <- factor(data[, 1], levels = unique(data[, 1]))
  
  if(is.null(session$userData$plotC)){
    
    session$userData$plotC <- plotComb(model, status)
    
  }
  session$userData$plotC$plotSensSpec
  
  # ggplot(data, aes(x=status, y=data[,3], colour=status)) + geom_point() +
  #   ggtitle("Scatter Graph of Combined Score, not yet")
  # 
}


createAnalysis <- function(input,output, session){
  
  functionName <- input$functions
  AUC_table <-session$userData$model$AUC_table
  aucRowNames <- as.matrix(rownames(AUC_table))
  aucRowNames <- aucRowNames[which(aucRowNames[,1] != "Combined"),]
  
  rOC_coordinates <- session$userData$model$ROC_coordinates
  
  output$rOCCoordinatesCombination <- DT::renderDataTable({
    tryCatch(
      {
        coordinates <-data.frame(Markers = rOC_coordinates$Marker[which(rOC_coordinates$Marker == "Combined")],
                                 Threshold = round(rOC_coordinates$Threshold[which(rOC_coordinates$Marker == "Combined")], 3),
                                 Specificity = round(rOC_coordinates$Specificity[which(rOC_coordinates$Marker == "Combined")], 3),
                                 Sensitivity = round(rOC_coordinates$Sensitivity[which(rOC_coordinates$Marker == "Combined")], 3)) 
        data <-coordinates
        # rOC_coordinates[which(rOC_coordinates[,1] == "Combined")]
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(data)
  },extensions = 'Buttons',options =options)
  output$rOCCoordinatesMarker1 <- DT::renderDataTable({
    tryCatch(
      {
        coordinates <-data.frame(Markers = rOC_coordinates$Marker[which(rOC_coordinates$Marker == aucRowNames[1])],
                                 Threshold = round(rOC_coordinates$Threshold[which(rOC_coordinates$Marker == aucRowNames[1])], 3),
                                 Specificity = round(rOC_coordinates$Specificity[which(rOC_coordinates$Marker == aucRowNames[1])], 3),
                                 Sensitivity = round(rOC_coordinates$Sensitivity[which(rOC_coordinates$Marker == aucRowNames[1])], 3)) 
        data <-coordinates
        
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(data)
  },extensions = 'Buttons',options =options)
  output$rOCCoordinatesMarker2 <- DT::renderDataTable({
    tryCatch(
      {
        coordinates <-data.frame(Markers = rOC_coordinates$Marker[which(rOC_coordinates$Marker == aucRowNames[2])],
                                 Threshold = round(rOC_coordinates$Threshold[which(rOC_coordinates$Marker == aucRowNames[2])], 3),
                                 Specificity = round(rOC_coordinates$Specificity[which(rOC_coordinates$Marker == aucRowNames[2])], 3),
                                 Sensitivity = round(rOC_coordinates$Sensitivity[which(rOC_coordinates$Marker == aucRowNames[2])], 3)) 
        data <-coordinates
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(data)
  },extensions = 'Buttons',options =options)
  
  aucTableOptions <- options
  aucTableOptions$columnDefs <- list(list(className = 'dt-center',targets=1:(ncol(session$userData$model$AUC_table)-1)))
  output$AUCTable <- DT::renderDataTable({
    tryCatch(
      {
        roundedCols <- round(session$userData$model$AUC_table[,-6],3)
        
        calData <- data.frame(roundedCols, p.value = session$userData$model$AUC_table[,6])
        
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(calData)
  },extensions = 'Buttons',options =aucTableOptions)
  
  ##------------------------------
  Statistic <- c(
    "Apparent prevalence",
    "True prevalence",
    "Sensitivity",
    "Specificity",
    "Correctly classified proportion",
    "sil1",
    "sil2",
    "Youden",
    "Positive predictive value",
    "Negative predictive value",
    "Positive likelihood ratio",
    "Negative likelihood ratio",
    "sil3",
    "sil4",
    "False T+ proportion for true D-",
    "False T- proportion for true D+",
    "False T+ proportion for T+",
    "False T- proportion for T-")
  diagStatCombinedTab <- session$userData$model$DiagStatCombined$tab
  diagStatCombinedDetail <- session$userData$model$DiagStatCombined$detail
  if(class(diagStatCombinedDetail)=="data.frame"){
    
    diagStatOptions1 <- options  
    diagStatOptions1$columnDefs <- list(list(className = 'dt-center',targets=1:3))
    
    diagStatOptions2 = options
    diagStatOptions2$lengthMenu = list(c(13, 30, 50, -1),
                                       c('13', '30', '50', 'All'))
    diagStatOptions2$columnDefs = list(list(className = 'dt-center',targets = 2:4))
    
    output$DiagStatCombination <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatCombinedTab
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions1)
    diagStatCombinedDetail <- diagStatCombinedDetail[,-c(1)]
    diagStatCombinedDetail <- data.frame(Statistic,diagStatCombinedDetail)
    diagStatCombinedDetail <- diagStatCombinedDetail[-c(6, 7, 8, 13,14),]
    diagStatCombinedDetail[,2] <- round(diagStatCombinedDetail[,2],3)
    diagStatCombinedDetail[,3] <- round(diagStatCombinedDetail[,3],3)
    diagStatCombinedDetail[,4] <- round(diagStatCombinedDetail[,4],3)
    rownames(diagStatCombinedDetail) <- c()
    
    
    output$DiagStatCombinationDetail <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatCombinedDetail
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions2)
  }else{
    diagStatDetailSub <- round(diagStatCombinedDetail$ap, 3)
    rownames(diagStatDetailSub) <- c("Apparent prevalence")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$tp, 3)
    rownames(diagStatDetailSub) <- c("True prevalence")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$se, 3)
    rownames(diagStatDetailSub) <- c("Sensitivity")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$sp, 3)
    rownames(diagStatDetailSub) <- c("Specificity")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$pv.pos, 3)
    rownames(diagStatDetailSub) <- c("Positive predictive value")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$pv.neg, 3)
    rownames(diagStatDetailSub) <- c("Negative predictive value")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$lr.pos, 3)
    rownames(diagStatDetailSub) <- c("Positive likelihood ratio")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$lr.neg, 3)
    rownames(diagStatDetailSub) <- c("Negative likelihood ratio")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$p.tpdn, 3)
    rownames(diagStatDetailSub) <- c("False T+ proportion for true D-")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$p.tndp, 3)
    rownames(diagStatDetailSub) <- c("False T- proportion for true D+")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$p.dntp, 3)
    rownames(diagStatDetailSub) <- c("False T+ proportion for T+")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$p.dptn, 3)
    rownames(diagStatDetailSub) <- c("False T- proportion for T-")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatCombinedDetail$diag.ac, 3)
    rownames(diagStatDetailSub) <- c("Correctly classified proportion")
    diagStatCombinedTab[nrow(diagStatCombinedTab) + 1, ] <- diagStatDetailSub
    
    ##------------------------------
    output$DiagStatCombination <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatCombinedTab
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions1)
  }
  
  
  ##------------------------------
  
  diagStatMarker1Tab <- session$userData$model$DiagStatMarker1$tab
  
  diagStatMarker1Detail <- session$userData$model$DiagStatMarker1$detail
  if(class(diagStatMarker1Detail) == "data.frame"){
    output$DiagStatMarker1 <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatMarker1Tab
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions1)
    
    diagStatMarker1Detail <- diagStatMarker1Detail[,-c(1)]
    diagStatMarker1Detail <- data.frame(Statistic,diagStatMarker1Detail)
    diagStatMarker1Detail <- diagStatMarker1Detail[-c(6, 7, 8, 13,14),]
    
    diagStatMarker1Detail[,2] <- round(diagStatMarker1Detail[,2],3)
    diagStatMarker1Detail[,3] <- round(diagStatMarker1Detail[,3],3)
    diagStatMarker1Detail[,4] <- round(diagStatMarker1Detail[,4],3)
    rownames(diagStatMarker1Detail) <- NULL
    output$DiagStatMarker1Detail <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatMarker1Detail
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions2)
  }else{
    diagStatDetailSub <- round(diagStatMarker1Detail$ap, 3)
    rownames(diagStatDetailSub) <- c("Apparent prevalence")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$tp, 3)
    rownames(diagStatDetailSub) <- c("True prevalence")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$se, 3)
    rownames(diagStatDetailSub) <- c("Sensitivity")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$sp, 3)
    rownames(diagStatDetailSub) <- c("Specificity")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$pv.pos, 3)
    rownames(diagStatDetailSub) <- c("Positive predictive value")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$pv.neg, 3)
    rownames(diagStatDetailSub) <- c("Negative predictive value")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$lr.pos, 3)
    rownames(diagStatDetailSub) <- c("Positive likelihood ratio")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$lr.neg, 3)
    rownames(diagStatDetailSub) <- c("Negative likelihood ratio")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$p.tpdn, 3)
    rownames(diagStatDetailSub) <- c("False T+ proportion for true D-")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$p.tndp, 3)
    rownames(diagStatDetailSub) <- c("False T- proportion for true D+")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$p.dntp, 3)
    rownames(diagStatDetailSub) <- c("False T+ proportion for T+")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$p.dptn, 3)
    rownames(diagStatDetailSub) <- c("False T- proportion for T-")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker1Detail$diag.ac, 3)
    rownames(diagStatDetailSub) <- c("Correctly classified proportion")
    diagStatMarker1Tab[nrow(diagStatMarker1Tab) + 1, ] <- diagStatDetailSub
    ##------------------------------
    
    output$DiagStatMarker1 <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatMarker1Tab
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions1)
    output$DiagStatMarker2Detail <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatDetailSub
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions2)
  }
  
  
  ##------------------------------
  diagStatMarker2Tab <- session$userData$model$DiagStatMarker2$tab
  diagStatMarker2Detail <- session$userData$model$DiagStatMarker2$detail
  if(class(diagStatMarker2Detail) == "data.frame"){
    output$DiagStatMarker2 <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatMarker2Tab
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions1)
    diagStatMarker2Detail <- diagStatMarker2Detail[,-c(1)]
    diagStatMarker2Detail <- data.frame(Statistic,diagStatMarker2Detail)
    diagStatMarker2Detail <- diagStatMarker2Detail[-c(6, 7, 8, 13,14),]
    diagStatMarker2Detail[,2] <- round(diagStatMarker2Detail[,2],3)
    diagStatMarker2Detail[,3] <- round(diagStatMarker2Detail[,3],3)
    diagStatMarker2Detail[,4] <- round(diagStatMarker2Detail[,4],3)
    rownames(diagStatMarker2Detail) <- NULL
    
    output$DiagStatMarker2Detail <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatMarker2Detail
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions2)
  }
  else{
    
    
    diagStatDetailSub <- round(diagStatMarker2Detail$ap, 3)
    rownames(diagStatDetailSub) <- c("Apparent prevalence")
    # diagStatMarker2Tab[nrow(diagStatMarker2Tab) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$tp, 3)
    rownames(diagStatDetailSub) <- c("True prevalence")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$se, 3)
    rownames(diagStatDetailSub) <- c("Sensitivity")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$sp, 3)
    rownames(diagStatDetailSub) <- c("Specificity")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$pv.pos, 3)
    rownames(diagStatDetailSub) <- c("Positive predictive value")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$pv.neg, 3)
    rownames(diagStatDetailSub) <- c("Negative predictive value")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$lr.pos, 3)
    rownames(diagStatDetailSub) <- c("Positive likelihood ratio")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$lr.neg, 3)
    rownames(diagStatDetailSub) <- c("Negative likelihood ratio")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$p.tpdn, 3)
    rownames(diagStatDetailSub) <- c("False T+ proportion for true D-")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$p.tndp, 3)
    rownames(diagStatDetailSub) <- c("False T- proportion for true D+")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$p.dntp, 3)
    rownames(diagStatDetailSub) <- c("False T+ proportion for T+")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$p.dptn, 3)
    rownames(diagStatDetailSub) <- c("False T- proportion for T-")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    
    diagStatDetailSub <- round(diagStatMarker2Detail$diag.ac, 3)
    rownames(diagStatDetailSub) <- c("Correctly classified proportion")
    diagStatDetailSub[nrow(diagStatDetailSub) + 1, ] <- diagStatDetailSub
    ##------------------------------
    output$DiagStatMarker2 <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatMarker2Tab
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions1)
    output$DiagStatMarker2Detail <- DT::renderDataTable({
      tryCatch(
        {
          data <-  diagStatDetailSub
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =diagStatOptions2)
  }
  
  
  
  multCombTableOptions <- options
  multCombTableOptions$columnDefs <- list(list(className = 'dt-center',targets=2:7))
  output$MultCombTable <- DT::renderDataTable({
    tryCatch(
      {
        nameData <- session$userData$model$MultComp_table[, 1:2]
        p.value <- session$userData$model$MultComp_table[, 8]
        calData <- round(session$userData$model$MultComp_table[, -c(1, 2, 8)], 3)
        data <- data.frame(nameData, calData, p.value)
        colnames(data) <- c("Marker1 (A)", "Marker2 (B)", "AUC (A)", "AUC (B)",
                            "|A-B|", "SE(|A-B|)", "z", "p-value")
        
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(data)
  },extensions = 'Buttons',options =multCombTableOptions)
  
  cutoff_name <- session$userData$model$Cuttoff.method
  if(!is.null(session$userData$model$Criterion.m1)){
    m1 <- round(session$userData$model$Criterion.m1, 2)
  } else {
    m1 <- ""
  }
  if(!is.null(session$userData$model$Criterion.m2)){
    m2 <- round(session$userData$model$Criterion.m2, 2)
  } else {
    m2 <- ""
  }
  if(!is.null(session$userData$model$Criterion.c)){
    c <- round(session$userData$model$Criterion.c, 2)
  } else {
    c <- ""
  }

  tM1 <- round(session$userData$model$ThresholdMarker1, 2)
  tM2 <- round(session$userData$model$ThresholdMarker2, 2)
  tC <- round(session$userData$model$ThresholdCombined, 2)
  
  
  cutpointsTableOptions <- options
  cutpointsTableOptions$columnDefs <- list(list(className = 'dt-center',targets=1:1))
  output$CutPointsCombination <- DT::renderDataTable({
    tryCatch(
      {
        
        dataValues <-as.matrix( c(cutoff_name, c,  tC))
        rownames(dataValues) <-c("Optimal cut-off method","Optimal criterion", "Optimal cut-off point")
        data <- dataValues
        colnames(data) <- c("Statistic")
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(data)
  },extensions = 'Buttons',options =cutpointsTableOptions)
  
  output$CutPointsMarker1 <- DT::renderDataTable({
    tryCatch(
      {
        
        dataValues <-as.matrix( c(cutoff_name, m1, tM1))
        rownames(dataValues) <-c("Optimal cut-off method","Optimal criterion", "Optimal cut-off point")
        data <- dataValues
        colnames(data) <- c("Statistic")
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(data)
  },extensions = 'Buttons',options =cutpointsTableOptions)
  
  
  output$CutPointsMarker2 <- DT::renderDataTable({
    tryCatch(
      {
        
        dataValues <-as.matrix( c(cutoff_name,  m2,  tM2))
        rownames(dataValues) <-c("Optimal cut-off method","Optimal criterion", "Optimal cut-off point")
        data <- dataValues
        colnames(data) <- c("Statistic")
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(data)
  },extensions = 'Buttons',options =cutpointsTableOptions)
  
  
  
}
hush=function(code){
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}

server <- function(input, output, session) {
  
  observeEvent(input$tabGroupDataUpload, {
    tabName <- input$tabGroupDataUpload;
    if(tabName == "rocTab")
    {
      shiny::hideTab(inputId = "generalTabSetPanel", target = "Analysis")
      shiny::showTab(inputId = "generalTabSetPanel", target = "Roc")
    }else{
      shiny::hideTab(inputId = "generalTabSetPanel", target = "Roc")
      shiny::showTab(inputId = "generalTabSetPanel", target = "Analysis")
      
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
    req(input$selectStatus1)
    if(is.null(input$selectStatus1))
      return()
    a <- input$selectStatus1
    
    data <-  session$userData$data
    status <- data[[input$selectStatus1]]
    statusLevels<-as.factor(status)
    updateSelectInput(session,"event1",
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
  observeEvent(input$sampleData,{
    
  })
  output$RawData <- DT::renderDataTable({
    
    sampleData <- input$sampleData
    
    data <- getData(input,session)
    dataHeader <- colnames(data) 
    if(input$dataInput == "1"){
      updateSelectInput(session,"selectMarker1",
                        "Marker 1 variable",
                        choices = c("",dataHeader))
      updateSelectInput(session,"selectMarker2",
                        "Marker 2 variable",
                        choices = c("",dataHeader))
      updateSelectInput(session,"selectStatus1",
                        "Status variable",
                        choices = c("",dataHeader)
      )
    }else{
      if(!is.null(session$userData$sampleData) && session$userData$sampleData == sampleData){
        
      }
      else if (sampleData == 1){
        updateSelectInput(session,"selectStatus2",
                          "Status variable",
                          choices = dataHeader
                          ,selected = "group"
        )                           
        updateSelectInput(session,"selectMarker3",
                          "Marker 1 variable",
                          choices = dataHeader
                          ,selected = "ddimer")
        updateSelectInput(session,"selectMarker4",
                          "Marker 2 variable",
                          choices = dataHeader,
                          selected = "log_leukocyte"
        )}
      else if (sampleData == 2){
        updateSelectInput(session,"selectStatus2",
                          "Status variable",
                          choices = dataHeader,
                          selected = "Group")                           
        updateSelectInput(session,"selectMarker3",
                          "Marker 1 variable",
                          choices =  dataHeader,
                          selected = "m1")
        updateSelectInput(session,"selectMarker4",
                          "Marker 2 variable",
                          choices = dataHeader,
                          selected = "m2")
      }
      else if (sampleData == 3){
        updateSelectInput(session,"selectStatus2",
                          "Status variable",
                          choices = dataHeader,
                          selected = "status")                           
        updateSelectInput(session,"selectMarker3",
                          "Marker 1 variable",
                          choices =  dataHeader,
                          selected = "marker1")
        updateSelectInput(session,"selectMarker4",
                          "Marker 2 variable",
                          choices = dataHeader,
                          selected = "marker2")
      }
      session$userData$sampleData <- sampleData
      
      
      
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


