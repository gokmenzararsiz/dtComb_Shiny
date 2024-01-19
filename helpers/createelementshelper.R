
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
