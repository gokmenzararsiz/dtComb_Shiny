createROCPlotRoc <- function(input, output, session){
  
  rawData <- session$userData$data
  dataInput <- input$dataInput
  
  dataHeader <- colnames(rawData)
  
  isActive <- controlMarkerCount(input)
  if(!isActive)
    return()
  markerNames <- c()
  if(dataInput == "1"){
    markers <- data.frame(matrix(nrow = nrow(rawData)))
    
    markerNames <- input$selectMarker1Roc
    for (variable in markerNames) {
      markerVariable <- rawData[[variable]]
      markers[,variable] = markerVariable    
    }
    markers <- markers[,-1]
    event <- input$event1Roc
    status <- rawData[[input$selectStatus1Roc]]
    data <- data.frame(status,markers)
    colnames(data) = c(input$selectStatus1Roc,input$selectMarker1Roc)
    
  }else{
    markers <- data.frame(matrix(nrow = nrow(rawData)))
    markerNames <- input$selectMarker3Roc
    
    for (variable in input$selectMarker3Roc) {
      markerVariable <- rawData[[variable]]
      markers[,variable] = markerVariable    
    }
    markers <- markers[,-1]
    
    event <- input$event2Roc
    status <- rawData[[input$selectStatus2Roc]]
    
    data <- data.frame(status,markers)
    colnames(data) = c(input$selectStatus2Roc,input$selectMarker3Roc)
    
  }  
  
  session$userData$data <- data
  
  markers <- as.data.frame(data[, -1])
  colnames(markers) <- markerNames
  status <- as.factor(data[, 1])
  cutoff.method <- input$cutoffmethodRoc
  direction <- input$directionRoc
  conf.level <- input[["conflevelRoc"]]
  
  rocList <- c()
  rocListNames <- c()
  coords <- c()
  AUC_table <- c()
  std.err <- c()
  coord.names <- c()
  markerColNames <- colnames(markers)
  colorCodes <- c("#c10c20", "#3098cb", "#75dd85", "#258205", "#3f0a71", "#9bf4d5", "#9baddd", "#e5129b")
  # , 
  for (i in 1:ncol(markers)) {
    isAdd <- F
    if(i > 1)
      isAdd <- T
    colorCode <-  colorCodes[i]
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    graphics::par(pty = "s")
    roc.m1 <- suppressMessages(roc(status ~ markers[, i],
                                         plot = TRUE, print.auc = F,
                                         direction = direction, col =colorCode, lwd = 6, legacy.axes = TRUE, grid = TRUE,
                                         percent = F, main = "ROC Curves for Combination Diagnostic Test", add = isAdd
    ))
    colorCodes <- c(colorCodes,colorCode)
    rocList[[paste0("roc.m",i)]] = roc.m1
    markerColName <- markerColNames[i]
    coord <- coords(roc.m1)
    coords <- rbind(coords,coord) 
    coord.names <- c(coord.names,rep(markerColName, nrow(coord)))
    
    auc <- ci.auc(roc.m1, method = "delong")
    AUC_table <- rbind(AUC_table,auc)
    
    var <- var(roc.m1, method = "delong")
    std.err  = c(std.err,sqrt(var))
  }
  # if gelecek
  legend("bottomright", legend =
           markerNames
         , col = colorCodes, lwd = 4)
  
  ROC_coordinates <- data.frame(coord.names, coords)
  colnames(ROC_coordinates) <- c("Marker", "Threshold", "Specificity", "Sensitivity")
  
  z.stat <- (AUC_table[, 2] - .5) / std.err
  p.val <- 2 * pt(-abs(z.stat), df = Inf)
  
  AUC_table <- cbind(
    AUC_table[, 2], std.err, AUC_table[, 1], AUC_table[, 3],
    z.stat, p.val
  )
  
  rownames(AUC_table) <- c(markerColNames)
  colnames(AUC_table) <- c("AUC", "SE.AUC", "LowerLimit", "UpperLimit", "z", "p-value")
  
  
  
  
  MultComp_table<- NULL
  isActiveMultComb <- controlMultcombMarkerCount(input)
  if(isActiveMultComb){
    combinations <-  as.matrix(combn(ncol(markers), 2)) 
    MultComp_table <- matrix(0, ncol(combinations), 6)
    
    firstColnames <- c()
    secondColnames <- c()
    for (i in 1:ncol(combinations)) {
      firstColnames <- c(firstColnames,colnames(markers)[[combinations[1,i]]])
      secondColnames <- c(secondColnames,colnames(markers)[[combinations[2,i]]])
      roccm1 <- roc.test(rocList[[combinations[1,i]]], rocList[[combinations[2,i]]], method = "delong")
      
      MultComp_table[i, ] <- cbind(
        roccm1$estimate[1],
        roccm1$estimate[2], abs(roccm1$estimate[1] - roccm1$estimate[2]),
        abs(roccm1$estimate[1] - roccm1$estimate[2]) / roccm1$statistic,
        roccm1$statistic, roccm1$p.value
      )
    }
    
    comp.names <- cbind(
      firstColnames,
      secondColnames
    )
    
    MultComp_table <- data.frame(comp.names, MultComp_table)
    colnames(MultComp_table) <- c(
      "Marker1 (A)", "Marker2 (B)", "AUC (A)", "AUC (B)",
      "|A-B|", "SE(|A-B|)", "z", "p-value"
    )
    
  }
  
  statusFactored <- factor(ifelse(status == event, 1, 0), ordered = TRUE)
  
  data <- data.frame(markers, statusFactored)
  
  allres <- list(
    ROC_coordinates = ROC_coordinates,
    AUC_table = AUC_table,
    MultComp_table = MultComp_table,
    Cuttoff.method = cutoff.method
  )
  
  cutoff.all <- c()
  for (i in 1:(ncol(data)-1)) {
    
    cutoff.m1 <- optimal.cutpoints(colnames(data[i]), colnames(data[ncol(data)]),
                                                     tag.healthy = min(statusFactored),
                                                     methods = cutoff.method, data = data)
    
    cutoff.all[[paste0("cutoff.m",i)]] = cutoff.m1
    
    threshold.m1 <- cutoff.m1[[cutoff.method]]$Global$optimal.cutoff
    TP.m1 <- cutoff.m1[[cutoff.method]]$Global$measures.acc$n$d - threshold.m1$FN
    TN.m1 <- cutoff.m1[[cutoff.method]]$Global$measures.acc$n$h - threshold.m1$FP
    
    best.m1.tbl <- as.table(matrix(c(TP.m1[1], threshold.m1$FP[1], threshold.m1$FN[1], TN.m1[1]),
                                   nrow = 2, byrow = TRUE
    ))
    
    DiagStatMarker1 <- epi.tests(best.m1.tbl, conf.level = conf.level)
    
    allres[[paste0("DiagStatMarker",markerNames[i])]] <- DiagStatMarker1
    
    allres[[paste0("ThresholdMarker",markerNames[i])]] <- threshold.m1$cutoff
    
    allres[[paste0("Criterion.",markerNames[i])]] <- cutoff.m1[[cutoff.method]]$Global$optimal.criterion
    
  }
  
  
  
  
  session$userData$model <- allres
  createAnalysisRoc(input = input,output = output,session)
  
}

createROCPlot <- function(input, output, session){

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
    resampleName <- formatName(paste0(functionName,"Resampling method"))
    resample <- input[[resampleName]]
    if(is.null(resample))
      resample <- "none"
    
    nfoldsName <- formatName(paste0(resampleName,"Number of folds",resample))
    nfolds <- input[[nfoldsName]]
    nrepatsName <- formatName(paste0(resampleName,"Number of repeats",resample))
    nrepeats <- input[[nrepatsName]]
    nitersName <- formatName(paste0(resampleName,"Number of resampling iterations",resample))
    niters <- input[[nitersName]]
    
    preProcessName <- formatName(paste0(functionName,"Data Pre-processing"))
    preProcess <- input[[preProcessName]]
    if(preProcess == "None"){
      preProcess = NULL
    }
    resample <- getRealName(resample)
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
  
  ggplot(marker1Data, aes(x=Marker1, 
                                            colour=Labels)) + 
    xlab("Marker 1") +
    ylab("Density") +
    geom_density(size = 2) +
    ggtitle("Kernel density plot") +
    geom_vline(xintercept = model$ThresholdMarker1, linetype = "dotted") +
    theme_classic() +
    theme(plot.title = element_text(size = 22, face = "bold")) +
    theme(text = element_text(size = 20)) +
    theme(axis.text.x = element_text(size = 20)) +
    theme(axis.text.y = element_text(size = 20)) +
    theme(legend.position = "bottom")
  
}

createDistPlot2 <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  
  data[, 1] <- factor(data[, 1], levels = unique(data[, 1]))
  
  marker2Data = cbind(as.data.frame(data[, 3]), as.data.frame(data[, 1]))
  
  names(marker2Data) <- c("Marker2", "Labels")
  
  ggplot(marker2Data, aes(x=Marker2, 
                                            colour=Labels)) + 
    xlab("Marker 2") +
    ylab("Density") +
    geom_density(size = 2) +
    ggtitle("Kernel density plot") +
    geom_vline(xintercept = model$ThresholdMarker2, linetype = "dotted") +
    theme_classic() +
    theme(plot.title = element_text(size = 22, face = "bold")) +
    theme(text = element_text(size = 20)) +
    theme(axis.text.x = element_text(size = 20)) +
    theme(axis.text.y = element_text(size = 20)) +
    theme(legend.position = "bottom")
  
}

createDistPlotC <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  
  event <-session$userData$event
  
  status <- factor(data[, 1], levels = unique(data[, 1]))
  
  session$userData$plotC <- plotComb(model, status)
    
  session$userData$plotC$plotDensity
  
}

createSctPlot1 <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  
  data[, 1] <- factor(data[, 1], levels = unique(data[, 1]))
  
  marker1Data = cbind(as.data.frame(data[, 2]), as.data.frame(data[, 1]))
  
  names(marker1Data) <- c("Marker1", "Labels")
  
  
  ggplot(marker1Data, aes(x=Labels, y=Marker1,  
                                            color=Labels)) + geom_point(size = 2) +
    ylab("Marker 1") +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_jitter(width = 0.40, size = 2) +
    geom_point() +
    ggtitle("Individual-value plot") +
    geom_hline(yintercept = model$ThresholdMarker1, linetype = "dotted") +
    theme_classic() +
    theme(plot.title = element_text(size = 22, face = "bold")) +
    theme(text = element_text(size = 20)) +
    theme(axis.text.x = element_text(size = 20)) +
    theme(axis.text.y = element_text(size = 20)) +
    theme(legend.position = "bottom")
  
  
}

createSctPlot2 <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  
  data[, 1] <- factor(data[, 1], levels = unique(data[, 1]))
  
  marker2Data = cbind(as.data.frame(data[, 3]), as.data.frame(data[, 1]))
  
  names(marker2Data) <- c("Marker2", "Labels")
  
  
  ggplot(marker2Data, aes(x=Labels, y=Marker2,  
                                            color=Labels)) + geom_point(size = 2) +
    ylab("Marker 2") +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_jitter(width = 0.40, size = 2) +
    geom_point() +
    ggtitle("Individual-value plot") +
    geom_hline(yintercept = model$ThresholdMarker2, linetype = "dotted") +
    theme_classic() +
    theme(plot.title = element_text(size = 22, face = "bold")) +
    theme(plot.title = element_text(size = 20)) +
    theme(text = element_text(size = 20)) +
    theme(axis.text.x = element_text(size = 20)) +
    theme(axis.text.y = element_text(size = 20)) +
    theme(legend.position = "bottom")
  
}

createSctPlotC <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  event <-session$userData$event
  
  status <- factor(data[, 1], levels = unique(data[, 1]))
  
  session$userData$plotC <- plotComb(model, status)
    
  session$userData$plotC$plotScatter
  
}

createSenSpePlot1 <- function(input, output, session){
  
  model <-  session$userData$model
  
  results <- model$ROC_coordinates
  markers <- session$userData$data
  
  coord = results[results[ ,"Marker"] == colnames(markers)[2], ]
  
  colors <- c("Sensitivity" = "#f8766d", "Specificity" = "#00bfc4")
  
  ggplot(coord, aes(x=Threshold)) + 
    geom_line(aes(y = Sensitivity, color = "Sensitivity"), show.legend = TRUE, size = 2) +
    geom_line(aes(y = Specificity, color = "Specificity"), show.legend = TRUE, size = 2) +
    theme_classic() +
    theme(plot.title = element_text(size = 22, face = "bold")) +
    theme(text = element_text(size = 20)) +
    theme(axis.text.x = element_text(size = 20)) +
    theme(axis.text.y = element_text(size = 20)) +
    ggtitle("Sensitivity&Specificity Plot") +
    labs(y = "Value", x = "Marker 1", color = "Labels") +
    scale_color_manual(values = colors) +
    geom_vline(xintercept=model$ThresholdMarker1, linetype='dotted') +
    theme(legend.position = "bottom")
  
}

createSenSpePlot2 <- function(input, output, session){
  
  model <-  session$userData$model
  
  results <- model$ROC_coordinates
  markers <- session$userData$data
  coord = results[results[ ,"Marker"] == colnames(markers)[3], ]
  
  colors <- c("Sensitivity" = "#f8766d", "Specificity" = "#00bfc4")
  
  ggplot(coord, aes(x=Threshold)) + 
    geom_line(aes(y = Sensitivity, color = "Sensitivity"), show.legend = TRUE, size = 2) +
    geom_line(aes(y = Specificity, color = "Specificity"), show.legend = TRUE, size = 2) +
    theme_classic() +
    theme(plot.title = element_text(size = 22, face = "bold")) +
    theme(text = element_text(size = 20)) +
    theme(axis.text.x = element_text(size = 20)) +
    theme(axis.text.y = element_text(size = 20)) +
    ggtitle("Sensitivity&Specificity Plot") +
    labs(y = "Value", x = "Marker 2", color = "Labels") +
    scale_color_manual(values = colors) +
    geom_vline(xintercept=model$ThresholdMarker2, linetype='dotted') +
    theme(legend.position = "bottom")
  
}

createSenSpePlotC <- function(input, output, session){
  
  dataInput <- input$dataInput
  data <-  session$userData$data
  model <-  session$userData$model
  event <-session$userData$event
  
  
  data[,3] = as.numeric(data[,3])
  
  status <- factor(data[, 1], levels = unique(data[, 1]))
  
  session$userData$plotC <- plotComb(model, status)
    
  session$userData$plotC$plotSensSpec
  
  # ggplot(data, aes(x=status, y=data[,3], colour=status)) + geom_point() +
  #   ggtitle("Scatter Graph of Combined Score, not yet")
  # 
}