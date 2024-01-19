
createAnalysisRoc <- function(input,output, session){
  
  functionName <- input$functions
  AUC_table <-session$userData$model$AUC_table
  aucRowNames <- as.matrix(rownames(AUC_table))

  rOC_coordinates <- session$userData$model$ROC_coordinates
  Statistic <- c(
    "Apparent prevalence",
    "True prevalence",
    "Sensitivity",
    "Specificity",
    "Correctly classified proportion",
    "delete1",
    "delete2",
    "Youden",
    "Positive predictive value",
    "Negative predictive value",
    "Positive likelihood ratio",
    "Negative likelihood ratio",
    "delete3",
    "delete4",
    "False T+ proportion for true D-",
    "False T- proportion for true D+",
    "False T+ proportion for T+",
    "False T- proportion for T-")
  markerNames <- rownames(AUC_table)
  
  session$userData$oldMarkerNames <- markerNames
  lapply(markerNames, \(x) {
    
    appendTab("tabGroupCoordinatesRoc", tabPanel(id = paste0("coordinatCombinationRocTab",x),title = x, 
                                                 renderDataTable({
                                                   tryCatch(
                                                     {
                                                       coordinates <-data.frame(Markers = rOC_coordinates$Marker[which(rOC_coordinates$Marker == x)],
                                                                                Threshold = round(rOC_coordinates$Threshold[which(rOC_coordinates$Marker == x)], 3),
                                                                                Specificity = round(rOC_coordinates$Specificity[which(rOC_coordinates$Marker == x)], 3),
                                                                                Sensitivity = round(rOC_coordinates$Sensitivity[which(rOC_coordinates$Marker == x)], 3)) 
                                                       data <-coordinates
                                                     },
                                                     error = function(e) {
                                                       stop(safeError(e))
                                                     }
                                                   )
                                                   return(data)
                                                 },extensions = 'Buttons',options =options)
    ), select = ifelse(x == markerNames[1],TRUE,FALSE))
    
    ##------------------------------
    diagStatMarker <- session$userData$model[[paste0("DiagStatMarker",x)]]
    diagStatMarker1Tab <- diagStatMarker$tab
    diagStatMarker1Detail <- diagStatMarker$detail
    
    diagStatOptions1 <- options  
    diagStatOptions1$columnDefs <- list(list(className = 'dt-center',targets=1:3))
    
    diagStatOptions2 = options
    diagStatOptions2$lengthMenu = list(c(13, 30, 50, -1),
                                       c('13', '30', '50', 'All'))
    diagStatOptions2$columnDefs = list(list(className = 'dt-center',targets = 2:4))
    if(class(diagStatMarker1Detail) == "data.frame"){
      
      
      diagStatMarker1Detail <- diagStatMarker1Detail[,-c(1)]
      diagStatMarker1Detail <- data.frame(Statistic,diagStatMarker1Detail)
      diagStatMarker1Detail <- diagStatMarker1Detail[-c(6, 7, 8, 13,14),]
      
      diagStatMarker1Detail[,2] <- round(diagStatMarker1Detail[,2],3)
      diagStatMarker1Detail[,3] <- round(diagStatMarker1Detail[,3],3)
      diagStatMarker1Detail[,4] <- round(diagStatMarker1Detail[,4],3)
      rownames(diagStatMarker1Detail) <- NULL
      
      appendTab("tabGroupDiagStatRoc", tabPanel(id = paste0("DiagStatMarker1Roc",x),title = x, 
                                                renderDataTable({
                                                  tryCatch(
                                                    {
                                                      data <-  diagStatMarker1Tab
                                                    },
                                                    error = function(e) {
                                                      stop(safeError(e))
                                                    }
                                                  )
                                                  return(data)
                                                },extensions = 'Buttons',options =diagStatOptions1),
                                                renderDataTable({
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
      ), select = ifelse(x == markerNames[1],TRUE,FALSE))
      
      
    }
    else{
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
      
      appendTab("tabGroupDiagStatRoc", tabPanel(id = paste0("DiagStatMarker1Roc",x),title = x, 
                                                renderDataTable({
                                                  tryCatch(
                                                    {
                                                      data <-  diagStatMarker1Tab
                                                    },
                                                    error = function(e) {
                                                      stop(safeError(e))
                                                    }
                                                  )
                                                  return(data)
                                                },extensions = 'Buttons',options =diagStatOptions1),
                                                renderDataTable({
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
      ), select = ifelse(x == markerNames[1],TRUE,FALSE))
      
    }
    
    
    
    cutoff_name <- session$userData$model$Cuttoff.method
    if(!is.null(session$userData$model[[paste0("Criterion.",x)]])){
      m1 <- round(session$userData$model[[paste0("Criterion.",x)]], 2)
    } else {
      m1 <- ""
    }
    tM1 <- round(session$userData$model[[paste0("ThresholdMarker",x)]], 2)
    
    cutpointsTableOptions <- options
    cutpointsTableOptions$columnDefs <- list(list(className = 'dt-center',targets=1:1))
    
    appendTab("tabGroupCutPointsRoc", tabPanel(id = paste0("coordinatCombinationRocTab",x),title = x, 
                                               renderDataTable({
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
    ), select = ifelse(x == markerNames[1],TRUE,FALSE))
    
    
    ##------------------------------
  })
  
  isActiveMultComb <- controlMultcombMarkerCount(input)
  
  aucTableOptions <- options
  aucTableOptions$columnDefs <- list(list(className = 'dt-center',targets=1:(ncol(session$userData$model$AUC_table)-1)))
  
  roundedCols <- round(session$userData$model$AUC_table[,-6],3)
  
  if(!isActiveMultComb){
    roundedCols <- t(roundedCols)
    rownames(roundedCols) <- markerNames
  }
  output$AUCTableRoc <- renderDataTable({
    tryCatch(
      {
        calData <- data.frame(roundedCols, p.value = session$userData$model$AUC_table[,6])
        
        colnames(calData) <- c(colnames(roundedCols),"p.value")
      }, 
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(calData)
  },extensions = 'Buttons',options =aucTableOptions)
  
  ##------------------------------
  multCombTableOptions <- options
  multCombTableOptions$columnDefs <- list(list(className = 'dt-center',targets=2:7))
  if(length(markerNames)>1){
   
    output$MultCombTableRoc <- renderDataTable({
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
  }else{
    output$MultCombTableRoc <- renderDataTable({
      tryCatch(
        {
          data <- NULL
          
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
      return(data)
    },extensions = 'Buttons',options =multCombTableOptions)
  }
  
  
}


createAnalysis <- function(input,output, session){
  
  functionName <- input$functions
  AUC_table <-session$userData$model$AUC_table
  aucRowNames <- as.matrix(rownames(AUC_table))
  aucRowNames <- aucRowNames[which(aucRowNames[,1] != "Combination"),]
  
  rOC_coordinates <- session$userData$model$ROC_coordinates
  
  output$rOCCoordinatesCombination <- renderDataTable({
    tryCatch(
      {
        coordinates <-data.frame(Markers = rOC_coordinates$Marker[which(rOC_coordinates$Marker == "Combination")],
                                 Threshold = round(rOC_coordinates$Threshold[which(rOC_coordinates$Marker == "Combination")], 3),
                                 Specificity = round(rOC_coordinates$Specificity[which(rOC_coordinates$Marker == "Combination")], 3),
                                 Sensitivity = round(rOC_coordinates$Sensitivity[which(rOC_coordinates$Marker == "Combination")], 3)) 
        data <-coordinates
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(data)
  },extensions = 'Buttons',options =options)
  output$rOCCoordinatesMarker1 <- renderDataTable({
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
  output$rOCCoordinatesMarker2 <- renderDataTable({
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
  output$AUCTable <- renderDataTable({
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
    "delete1",
    "delete2",
    "Youden",
    "Positive predictive value",
    "Negative predictive value",
    "Positive likelihood ratio",
    "Negative likelihood ratio",
    "delete3",
    "delete4",
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
    
    output$DiagStatCombination <- renderDataTable({
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
    
    
    output$DiagStatCombinationDetail <- renderDataTable({
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
  }
  else{
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
    output$DiagStatCombination <- renderDataTable({
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
    output$DiagStatMarker1 <- renderDataTable({
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
    output$DiagStatMarker1Detail <- renderDataTable({
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
    
    output$DiagStatMarker1 <- renderDataTable({
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
    output$DiagStatMarker1Detail <- renderDataTable({
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
    output$DiagStatMarker2 <- renderDataTable({
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
    
    output$DiagStatMarker2Detail <- renderDataTable({
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
    output$DiagStatMarker2 <- renderDataTable({
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
    output$DiagStatMarker2Detail <- renderDataTable({
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
  output$MultCombTable <- renderDataTable({
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
  output$CutPointsCombination <- renderDataTable({
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
  
  output$CutPointsMarker1 <- renderDataTable({
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
  
  
  output$CutPointsMarker2 <- renderDataTable({
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
