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

 collapseTrigger <- reactiveVal(0)
  
  observeEvent(input$panelClicked, {
    panelName <- input$panelClicked$panelName
    print(paste("Panel tıklandı:", panelName))
    
    # Panel adına göre işlem yap
    if(panelName == "ROC Coordinates") {
      print("ROC Coordinates paneli açıldı")
      collapseTrigger(collapseTrigger() + 1)      
      
    } else if(panelName == "AUC Table") {
      print("AUC Table paneli açıldı")
      collapseTrigger(collapseTrigger() + 1)      
      
    } else if(panelName == "Multiple Comparison Table") {
      print("Multiple Comparison Table paneli açıldı")
      collapseTrigger(collapseTrigger() + 1)      
    } else if(panelName == "Cut Points") {
      print("Cut Points paneli açıldı")
      collapseTrigger(collapseTrigger() + 1)      
      
    } else if(panelName == "Performance Measures") {
      print("Performance Measures paneli açıldı")
      collapseTrigger(collapseTrigger() + 1)      
      
    }
  })
  
  # 1. Reaktif Veri Hazırlama Bloğu
  diagStatCombinationData <- reactive({
    # Tetikleyiciler
    input$goButton
    collapseTrigger()
    
    # Veri Güvenliği: Modelin bu parçaları yoksa dur
    req(session$userData$model$DiagStatCombined$tab)
    req(session$userData$model$DiagStatCombined$detail)
    
    # Ham verileri çek
    diagTab <- session$userData$model$DiagStatCombined$tab
    diagDetail <- session$userData$model$DiagStatCombined$detail
    
    # Statistic Etiketleri
    StatisticLabels <- c(
      "Apparent prevalence", "True prevalence", "Sensitivity", "Specificity",
      "Correctly classified proportion", "delete1", "delete2", "Youden",
      "Positive predictive value", "Negative predictive value",
      "Positive likelihood ratio", "Negative likelihood ratio",
      "delete3", "delete4", "False T+ proportion for true D-",
      "False T- proportion for true D+", "False T+ proportion for T+",
      "False T- proportion for T-"
    )
    
    # MANTIK: Eğer detail bir data.frame ise (Detaylı tablo modu)
    if(is.data.frame(diagDetail)) {
      
      # Detay tablosunu düzenle
      df_detail <- diagDetail[,-1, drop = FALSE] # İlk sütunu sil
      df_detail <- data.frame(Statistic = StatisticLabels, df_detail)
      df_detail <- df_detail[-c(6, 7, 8, 13, 14), ] # Gereksiz satırları sil
      
      # Sayısal sütunları yuvarla (2, 3 ve 4. sütunlar)
      df_detail[, 2:4] <- lapply(df_detail[, 2:4], function(x) round(as.numeric(x), 3))
      rownames(df_detail) <- NULL
      
      return(list(main = diagTab, detail = df_detail, mode = "complex"))
      
    } else {
      # MANTIK: Eğer detail liste formatındaysa (Basit tabloya ekleme modu)
      tempTab <- diagTab
      
      # Eklenecek değerler listesi
      mappings <- list(
        "Apparent prevalence" = diagDetail$ap,
        "True prevalence" = diagDetail$tp,
        "Sensitivity" = diagDetail$se,
        "Specificity" = diagDetail$sp,
        "Positive predictive value" = diagDetail$pv.pos,
        "Negative predictive value" = diagDetail$pv.neg,
        "Positive likelihood ratio" = diagDetail$lr.pos,
        "Negative likelihood ratio" = diagDetail$lr.neg,
        "False T+ proportion for true D-" = diagDetail$p.tpdn,
        "False T- proportion for true D+" = diagDetail$p.tndp,
        "False T+ proportion for T+" = diagDetail$p.dntp,
        "False T- proportion for T-" = diagDetail$p.dptn,
        "Correctly classified proportion" = diagDetail$diag.ac
      )
      
      # Döngü ile tabloyu büyüt
      for(name in names(mappings)) {
        row_val <- round(mappings[[name]], 3)
        tempTab[name, ] <- row_val
      }
      
      return(list(main = tempTab, detail = NULL, mode = "simple"))
    }
  })
  
  # 2. Üst Tablo Render (DiagStatCombination)
  output$DiagStatCombination <- renderDataTable({
    res <- diagStatCombinationData()
    req(res$main)
    
    datatable(res$main, 
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ))
  })
  
  # 3. Alt Tablo Render (DiagStatCombinationDetail)
  output$DiagStatCombinationDetail <- renderDataTable({
    res <- diagStatCombinationData()
    # Sadece kompleks modda (data.frame olduğunda) çiz
    req(res$mode == "complex", res$detail)
    
    datatable(res$detail, 
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                pageLength = 13,
                lengthMenu = list(c(13, 30, 50, -1), c('13', '30', '50', 'All')),
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ))
  })
  
  # 4. Askıdan Çıkar (Resize problemini ve boş gelme sorununu çözer)
  outputOptions(output, "DiagStatCombination", suspendWhenHidden = FALSE)
  outputOptions(output, "DiagStatCombinationDetail", suspendWhenHidden = FALSE)
  
  multCombTable(input = input,output = output,session = session,collapseTrigger)
  rOCCoordinatesCombinationData <- reactive({
    # Bu reaktif, model hesaplandığında veya panel tıklandığında veriyi hazırlar
    input$goButton # Hesapla butonu
    collapseTrigger()      # Panel tıklaması
    
    # Veri yoksa sessizce çık
    req(session$userData$model$ROC_coordinates)
    
    rOC_coordinates <- session$userData$model$ROC_coordinates
    coordinates <-data.frame(Markers = rOC_coordinates$Marker[which(rOC_coordinates$Marker == "Combination")],
                             Threshold = round(rOC_coordinates$Threshold[which(rOC_coordinates$Marker == "Combination")], 3),
                             Specificity = round(rOC_coordinates$Specificity[which(rOC_coordinates$Marker == "Combination")], 3),
                             Sensitivity = round(rOC_coordinates$Sensitivity[which(rOC_coordinates$Marker == "Combination")], 3)) 
    return(coordinates)
  })
  output$rOCCoordinatesCombination <- renderDataTable({
    data<- rOCCoordinatesCombinationData()
    return(data)
  },extensions = 'Buttons',options =options)
  outputOptions(output, "rOCCoordinatesCombination", suspendWhenHidden = FALSE)
  
  # 1. Veri Hazırlama (Reactive)
  cutPointsData <- reactive({
    # Tetikleyiciler
    input$goButton 
    collapseTrigger() 
    
    # Veri Güvenliği
    req(session$userData$model$Cuttoff.method)
    
    # Modelden verileri çek (Hata payını azaltmak için kontrollü çekim)
    cutoff_name <- if(!is.null(session$userData$model$Cuttoff.method)) session$userData$model$Cuttoff.method else "N/A"
    
    # Criterion.c kontrolü
    crit_c <- if(!is.null(session$userData$model$Criterion.c)) round(session$userData$model$Criterion.c, 3) else "-"
    
    # ThresholdCombined kontrolü
    thresh_c <- if(!is.null(session$userData$model$ThresholdCombined)) round(session$userData$model$ThresholdCombined, 3) else "-"
    
    # Matris yerine Data Frame oluşturuyoruz (DT için en güvenli yol)
    df <- data.frame(
      "Metric Name" = c("Optimal cut-off method", "Optimal criterion", "Optimal cut-off point"),
      "Value" = c(as.character(cutoff_name), as.character(crit_c), as.character(thresh_c)),
      stringsAsFactors = FALSE
    )
    
    return(df)
  })
  
  # 2. Tablo Render
  output$CutPointsCombination <- renderDataTable({
    df <- cutPointsData()
    datatable(df, 
              options = list(
                autoWidth = FALSE,  # Shiny'nin kafasına göre genişlik vermesini engelle
                scrollX = TRUE, 
                autoWidth = TRUE,
                columnDefs = list(list(width = '100px', targets = "_all"))
              ))
  })
  
  # 3. Gizli Paneli Uyandır
  outputOptions(output, "CutPointsCombination", suspendWhenHidden = FALSE)
  
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
   subMethodNames <- c("Random Forest")
    if(input$methods %in% subMethodNames){
      # 1. Mevcut fonksiyon grubuna göre temel listeyi matristen çek
      currentResaples <- session$userData$machineLearningResamples
      
      # 2. Üzerine OBB ekle (unique ile mükerrer kaydı engelle)
      newChoices <- unique(c(currentResaples, "OOB"))
      
      # 3. Input'u güncelle
      updateSelectInput(session, "MachineLearningAlgorithmsResamplingmethod",
                        choices = newChoices) # Mevcut seçimi korumak için
      
    }else if(input$functions == "Machine-Learning Algorithms"){
      currentResaples <- session$userData$machineLearningResamples
      updateSelectInput(session, "MachineLearningAlgorithmsResamplingmethod",
                        choices = currentResaples) # Mevcut seçimi korumak için
    }
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
    
    
    hush(createSenSpePlotC(input = input,output = output,session))
    
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
    
    
    hush(createDistPlotC(input = input,output = output,session))
    
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
    hush(createSctPlotC(input = input,output = output,session))
    
  }, height = 400, width = 500)
  
  observeEvent(input$goButton, {
    updateTabsetPanel(session, "tabGroupPlot",
                      selected = "Plots"
    )
    
  }) 
  
  # Reaktif bir değişken oluştur
  plotData <- eventReactive(input$goButton, {
    # Grafik için gerekli verileri burada hesaplayın
    createROCPlot(input = input, output = output, session)
  })
  
  # Grafiği render etmek için reaktif değişkeni kullanın
  output$ROCplot <- renderPlot({
    req(plotData())  # Sadece plotData tetiklendiğinde çalışır
    hush(plotData())
  }, height = 400, width = 500)
  
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


