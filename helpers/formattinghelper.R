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
controlMarkerCount <- function(input){
  isActive <- FALSE
  if(input$dataInput == 1 && !is.null(input$selectMarker1Roc) && length(input$selectMarker1Roc) <= 8){
    isActive <- TRUE
  } 
  if(input$dataInput == 2 && !is.null(input$selectMarker3Roc) && length(input$selectMarker3Roc) <= 8){
    isActive <- TRUE
  } 
  
  return(isActive)
} 
controlMultcombMarkerCount <- function(input){
  isActive <- FALSE
  if(input$dataInput == 1 && !is.null(input$selectMarker1Roc) && length(input$selectMarker1Roc) > 1){
    isActive <- TRUE
  } 
  if(input$dataInput == 2 && !is.null(input$selectMarker3Roc) && length(input$selectMarker3Roc) > 1){
    isActive <- TRUE
  } 
  
  return(isActive)
} 

hush <- function(code) {
  tmp <- NULL
  tryCatch({
    sink(tempfile())  # Geçici dosyaya yönlendir
    tmp = code
  }, finally = {
    if (sink.number() > 0) sink(NULL)  # Sadece açıksa kapat
  })
  return(tmp)
}
color.gradient <- function(x, colors=c("blue","white","red"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
getData<- function(input,session){
  dataInput <- input$dataInput
  
  tabName <- input$tabGroupDataUpload
  
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
    if(tabName == "ROC Analysis for Single Marker(s)"){
      sampleDataRoc <- input$sampleDataRoc
      
      if (sampleDataRoc == 1){
        data <- laparotomy
        dataHeader <- colnames(data)
        session$userData$dataHeader <- colnames(data)
      }
      else if (sampleDataRoc == 2){
        data <- exampleData2
        dataHeader <- colnames(data)
        session$userData$dataHeader <- colnames(data)
      } else if(sampleDataRoc == 3){
        data <- exampleData3
        dataHeader <- colnames(data)
        session$userData$dataHeader <- colnames(data)
      }
    }else{
      sampleData <- input$sampleData
      
      if (sampleData == 1){
        data <- laparotomy
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
  }
  
  session$userData$data <- data
  
  updateSelectInput(session, "slctColNames",
                    "Select Plot variable",
                    choices = c("",dataHeader)
  )
  return(data)
}

