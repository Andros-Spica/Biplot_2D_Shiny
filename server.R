
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(biplots2d3d)

shinyServer(function(input, output, session) {
  
  # reactive values
  v <- reactiveValues( dataSet = NULL,
                       variableNames = NULL,
                       inputVariables = NULL,
                       groupVariable = NULL,
                       maxMainPlot = TRUE,
                       varCodes = NULL,
                       arrowLabelAdjOverrides = NULL,
                       vipMouse = NULL, 
                       vipAreCharacters = TRUE,
                       # warning flags
                       dataSetLoaded = NULL,
                       noCSV = FALSE,
                       noValidCSV = FALSE,
                       noVarSelected = FALSE,
                       notEnoughVarSelectedForPCA = FALSE,
                       noGroupVarSelected = FALSE,
                       groupVarNotExist = FALSE,
                       groupVarIsInput = FALSE,
                       arrowLabelOverrideNotValid = FALSE
                       )
  
  inputWarnings <- reactive({
    
    dtWarn <- ""
    
    if (!input$useIris) {
      
      if (v$noValidCSV)
        dtWarn <- c(dtWarn, "File format is not CSV or the file contains a single column. ")
      if (v$noVarSelected)
        dtWarn <- c(dtWarn, "Select input variables. ")
      
      if (v$notEnoughVarSelectedForPCA)
        dtWarn <- c(dtWarn, "Select more than 3 input variables to perform PCA. ")
      
      if (v$groupVarNotExist)
        dtWarn <- c(dtWarn, "Group variable is not in this data set. ")
      
      if (v$groupVarIsInput)
        dtWarn <- c(dtWarn, "Group variable cannot be selected as input variable. ")
    }
    
    if (v$arrowLabelOverrideNotValid)
      dtWarn <- c(dtWarn, "arrow label adj override given is not valid. ")
    
    return(paste(dtWarn, collapse = ""))
  })
  
  output$warningPrint <- renderText(inputWarnings())
  
  inputGoodMessages <- reactive({
    
    dtMess <- ""
    
    if (!is.null(v$dataSetLoaded)) {
      if (v$dataSetLoaded)
        dtMess <- c(dtMess, "Data set loaded. ")
    }
    
    return(paste(dtMess, collapse = ""))
  })
  
  output$goodMessagePrint <- renderText(inputGoodMessages())
  
  inputMessages <- reactive({
    
    dtMess <- ""
    
    if (v$noCSV)
      dtMess <- c(dtMess, "Select a CSV file. ")
    
    if (v$noGroupVarSelected)
      dtMess <- c(dtMess, "Select a group variable. ")
    
    return(paste(dtMess, collapse = ""))
  })
  
  output$messagePrint <- renderText(inputMessages())
  
  observe({
    if (input$useIris) {
      v$noCSV <- FALSE
      v$noVarSelected <- FALSE
      v$dataSetLoaded <- NULL
      v$dataSet <- NULL
      v$variableNames <- NULL
      v$inputVariables <- NULL
      v$noGroupVarSelected <- FALSE
    } else {
      if (is.null(input$userDataSet)) {
        v$noCSV <- TRUE
      }
    }
  })
  
  observeEvent(
    input$loadDataSet,
    {
      readDataFile()
    }
  )
  
  readDataFile <- function() {
    
    # input$userDataSet will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$userDataSet
    
    if (is.null(inFile)) {
      
      v$dataSetLoaded <- FALSE
      v$noCSV <- TRUE
      v$dataSet <- NULL
      v$noVarSelected <- FALSE
      v$variableNames <- NULL
      v$noGroupVarSelected <- FALSE
      
    } else {
      
      v$noCSV <- FALSE
      v$dataSetLoaded <- TRUE
      
      v$dataSet <- read.csv(inFile$datapath, header = input$header, row.names = 1)
      v$variableNames <- names(v$dataSet)
      
      if (ncol(v$dataSet) == 0) {
        
        v$dataSet <- read.csv2(inFile$datapath, header = input$header, row.names = 1)
        v$variableNames <- names(v$dataSet)
        
        if (ncol(v$dataSet) == 0) {
          v$noValidCSV <- TRUE
          v$dataSet <- iris
          v$variableNames <- NULL
        } else {
          v$noValidCSV <- FALSE
        }
      } else {
        v$noValidCSV <- FALSE
      }
      v$noVarSelected <- TRUE
      
      v$groupVariable <- NULL
      v$noGroupVarSelected <- TRUE
      
      updateSelectInput(session, "inputVariables",
                        choices = v$variableNames
      )
      updateSelectInput(session, "groupVariables",
                        choices = v$variableNames
      )
    }
  }
  
  output$dataSetLoaded <- renderText(as.character(v$dataSetLoaded))
  
  outputOptions(output, "dataSetLoaded", suspendWhenHidden = FALSE) 
  
  observeEvent(
    input$selectVariables,
    {
      selectVariables()
    }
  )
  
  selectVariables <- function() {
    
    v$inputVariables <- input$inputVariables
    
    if (is.null(v$inputVariables)) {
      
      v$noVarSelected <- TRUE
      
    } else if (length(v$inputVariables) < 4) {
      
      v$noVarSelected <- FALSE
      v$notEnoughVarSelectedForPCA <- TRUE
      v$inputVariables <- NULL
      
    } else {
      
      v$noVarSelected <- FALSE
      v$notEnoughVarSelected <- FALSE
      v$notEnoughVarSelectedForPCA <- FALSE
      
    }
    
    v$groupVariable <- NULL
    
    if (is.null(input$groupVariable) | input$groupVariable == "") {
      v$noGroupVarSelected <- TRUE
    } else {
      v$noGroupVarSelected <- FALSE
      
      if (input$groupVariable %in% names(v$dataSet)) {
        v$groupVarNotExist <- FALSE
        v$groupVariable <- v$dataSet[, input$groupVariable]
      } else {
        v$groupVarNotExist <- TRUE
      }
      
      if (!is.null(v$inputVariables)) {
        if (input$groupVariable %in% v$inputVariables) {
          v$groupVarIsInput <- TRUE
          v$groupVariable <- NULL
        } else {
          v$groupVarIsInput <- FALSE
        }
      }
    }
    
  }
  
  ordination_object <- reactive({
    
    dt <- v$dataSet
    inputVariables <- v$inputVariables
    
    if (is.null(dt) | is.null(inputVariables)) {
      dt <- iris
      inputVariables <- names(iris)[1:4]
    }
    
    if (input$ordination_method == "PCA") {
      
      for (v in inputVariables) {
        dt[, v] <- as.numeric(as.character(dt[, v]))
      }
      
      ordination_object_ <- princomp(dt[, inputVariables])
    } else {
      # ...
    }
    
    return(ordination_object_)
  })
  
  
  output$width <- reactive({input$width})
  output$height <- reactive({input$height})
  
  main_fig <- reactive({
    mainFIG <- as.numeric(as.character(unlist(strsplit(input$main_fig,","))))
    if (any(is.na(mainFIG))) mainFIG <- c(0,1,0,1)
    if (!all( mainFIG == c(0,1,0,1))) v$maxMainPlot <- FALSE
    return(mainFIG)
  })
  
  output$maxMainPlot <- renderText(as.character(v$maxMainPlot))
  
  scaledScores <- reactive({
    if (input$ordination_method == "PCA") {
      lambda <- get_lambda(ordination_object()$sdev,
                           n.obs = nrow(ordination_object()$scores),
                           dimensions = 2,
                           scale = input$rows_over_columns,
                           pc.biplot = F)
      dt <- data.frame(t(t(ordination_object()$scores[, 1:2])/lambda))
      names(dt) <- c("Comp1","Comp2")
      row.names(dt) <- row.names(ordination_object()$scores)
      if (is.null(row.names(ordination_object()$scores))) row.names(dt) <- 1:nrow(dt)
      
      updateNumericInput(session, "xlim_min", 
                         value = round(min(dt[,1]), 2) - 0.1,
                         min = round(min(dt[,1]), 2) - 2,
                         max = round(min(dt[,1]), 2) - 0.01)
      updateNumericInput(session, "xlim_max", 
                         value = round(max(dt[,1]), 2) + 0.1,
                         min = round(max(dt[,1]), 2) + 0.01,
                         max = round(max(dt[,1]), 2) + 2)
      updateNumericInput(session, "ylim_min", 
                         value = round(min(dt[,2]), 2) - 0.1,
                         min = round(min(dt[,2]), 2) - 2,
                         max = round(min(dt[,2]), 2) - 0.01)
      updateNumericInput(session, "ylim_max", 
                         value = round(max(dt[,2]), 2) + 0.1,
                         min = round(max(dt[,2]), 2) + 0.01,
                         max = round(max(dt[,2]), 2) + 2)
      
    } else {
      # ...
    }
    return(dt)
  })
  
  invert_coordinates <- reactive({
    invert_coord <- as.numeric(input$invert_coordinates)
    if (length(invert_coord) == 0) {invert_coord <- c(0, 0)}
    return(invert_coord == 1:2)
  })
   
  x_title_fig <- reactive({as.numeric(unlist(strsplit(input$x_title_fig,",")))})
  y_title_fig <- reactive({as.numeric(unlist(strsplit(input$y_title_fig,",")))})
  
  subtitle <- reactive({
    sub <- input$subtitle
    if (sub == "") sub <- NULL
    return(sub)
  })
  
  point_label <- reactive({
    point_lab <- as.character(unlist(strsplit(input$point_label,",")))
    if (input$useRowNamesAsLabels | length(point_lab) == 0) 
      point_lab <- NULL
    return(point_lab)
  })
  
  arrow_fig <- reactive({
    arrowFIG <- as.numeric(as.character(unlist(strsplit(input$arrow_fig,","))))
    if (any(is.na(arrowFIG))) arrowFIG <- c(0.69,0.99,0.01,0.31)
    return(arrowFIG)
  })
  
  arrow_color <- reactive({
    arrow_col <- as.character(unlist(strsplit(input$arrow_color,",")))
    if (length(arrow_col) == 0) 
      arrow_col <- "black"
    return(arrow_col)
  })
  
  arrow_label_color <- reactive({
    arrow_label_col <- as.character(unlist(strsplit(input$arrow_label_color,",")))
    if (length(arrow_label_col) == 0) 
      arrow_label_col <- "black"
    return(arrow_label_col)
  })
  
  group_color <- reactive({
    group_col <- as.character(unlist(strsplit(input$group_color,",")))
    
    if (length(group_col) == 0 |
        (!input$useIris &
         !(is.null(v$dataSetLoaded) | v$noCSV | v$noValidCSV | v$noVarSelected) &
         (is.null(input$groupVariable) | input$groupVariable == ""))) 
      group_col <- "black"
    
    return(group_col)
  })
  
  group_legend_fig <- reactive({
    groupLegendFIG <- as.numeric(as.character(unlist(strsplit(input$group_legend_fig,","))))
    if (any(is.na(groupLegendFIG))) groupLegendFIG <- c(0.8, 0.99, 0.8, 0.95)
    return(groupLegendFIG)
  })
  
  group_legend_box_color <- reactive({
    group_legend_box_col <- as.character(unlist(strsplit(input$group_legend_box_color,",")))
  if (length(group_legend_box_col) == 0) 
    group_legend_box_col <- "white"
  return(group_legend_box_col)
  })
  
  group_legend_text_color <- reactive({
    group_legend_text_col <- as.character(unlist(strsplit(input$group_legend_text_color,",")))
  if (length(group_legend_text_col) == 0) 
    group_legend_text_col <- "black"
  return(group_legend_text_col)
  })
  
  vipAreCharacters <- reactive({
    return(as.character(any(grepl("[^0-9]", vip_pch()))))
  })
  
  vips <- reactive({
    return(
      list(
        mouse = (row.names(scaledScores()) %in% v$vipMouse),
        keyboard = (row.names(scaledScores()) %in% vipKeyboard())
      )
    )
  })
  
  vipKeyboard <- reactive({
    vipKey <- as.character(unlist(strsplit(input$vipKeyboard,",")))
    return(vipKey)
  })
  
  vip_pch <- reactive({
    vipPCH <- as.character(unlist(strsplit(input$vip_pch,",")))
    if (length(vipPCH) == 0) vipPCH <- 0
    v$vipAreCharacters <- any(grepl("[^0-9]", vipPCH))
    if (!v$vipAreCharacters) vipPCH <- as.numeric((vipPCH))
    return(vipPCH)
  })
  
  vip_color <- reactive({
    vip_col <- as.character(unlist(strsplit(input$vip_color,",")))
    if (length(vip_col) == 0) vip_col <- "purple"
    return(vip_col)
  })
  
  vip_cex <- reactive({
    vip_cex_ <- as.numeric(as.character(unlist(strsplit(input$vip_cex,","))))
    if (length(vip_cex_) == 0) vip_cex_ <- c(5, 5, 5, 5, 5, 3, 3)
    return(vip_cex_)
  })
  
  vip_legend_fig <- reactive({
    vipLegendFIG <- as.numeric(as.character(unlist(strsplit(input$vip_legend_fig,","))))
    if (any(is.na(vipLegendFIG))) vipLegendFIG <- c(0.78, 0.99, 0.6, 0.75)
    return(vipLegendFIG)
  })
  
  output$vipAreCharacters <- renderText(as.character(v$vipAreCharacters))
  
  vip_legend_box_color <- reactive({
    vip_legend_box_col <- as.character(unlist(strsplit(input$vip_legend_box_color,",")))
    if (length(vip_legend_box_col) == 0) 
      vip_legend_box_col <- "white"
    return(vip_legend_box_col)
  })
  
  fitAnalysis_fig <- reactive({
    fitAnalysisFIG <- as.numeric(as.character(unlist(strsplit(input$fitAnalysis_fig,","))))
    if (any(is.na(fitAnalysisFIG))) fitAnalysisFIG <- c(0.02, 0.35, 0.06, 0.25)
    return(fitAnalysisFIG)
  })
  
  fitAnalysis_screePlot_color <- reactive({
    fitAnalysis_screePlot_col <- as.character(unlist(strsplit(input$fitAnalysis_screePlot_color,",")))
    if (length(fitAnalysis_screePlot_col) == 0) 
      fitAnalysis_screePlot_col <- "white"
    return(fitAnalysis_screePlot_col)
  })
  
  fitAnalysis_stress_p_color <- reactive({
    fitAnalysis_stress_p_col <- as.character(unlist(strsplit(input$fitAnalysis_stress_p_color,",")))
    if (length(fitAnalysis_stress_p_col) == 0) 
      fitAnalysis_stress_p_col <- "darkgrey"
    return(fitAnalysis_stress_p_col)
  })
  
  fitAnalysis_stress_l_color <- reactive({
    fitAnalysis_stress_l_col <- as.character(unlist(strsplit(input$fitAnalysis_stress_l_color,",")))
    if (length(fitAnalysis_stress_l_col) == 0) 
      fitAnalysis_stress_l_col <- "black"
    return(fitAnalysis_stress_l_col)
  })
  
  # Initialization of axes limits----
  observe({
    if (input$ordination_method == "PCA") {
      
      lambda <- get_lambda(ordination_object()$sdev,
                           n.obs = nrow(ordination_object()$scores),
                           dimensions = 2,
                           scale = 0.5,
                           pc.biplot = F)
      scores <- t(t(ordination_object()$scores[, 1:2])/lambda)
    } else {
      # ... 
    }
    
    updateNumericInput(session, "xlim_min", 
                       value = round(min(scores[,1]), 2) - 0.1,
                       min = round(min(scores[,1]), 2) - 2,
                       max = round(min(scores[,1]), 2) - 0.01)
    updateNumericInput(session, "xlim_max", 
                       value = round(max(scores[,1]), 2) + 0.1,
                       min = round(max(scores[,1]), 2) + 0.01,
                       max = round(max(scores[,1]), 2) + 2)
    updateNumericInput(session, "ylim_min", 
                       value = round(min(scores[,2]), 2) - 0.1,
                       min = round(min(scores[,2]), 2) - 2,
                       max = round(min(scores[,2]), 2) - 0.01)
    updateNumericInput(session, "ylim_max", 
                       value = round(max(scores[,2]), 2) + 0.1,
                       min = round(max(scores[,2]), 2) + 0.01,
                       max = round(max(scores[,2]), 2) + 2)
  })
  #----
  
  output$show1 <- renderPrint(v$arrowLabelAdjOverrides)
  
  # VIP selection----
  observeEvent(
    input$plot_click,
    {
      dt <- scaledScores()
      rangeX <- (input$xlim_max - input$xlim_min)
      rangeY <- (input$ylim_max - input$ylim_min)
      if (rangeX > rangeY) {
        dt$Comp1_ <- 
          (dt[, 1] - input$xlim_min) / (input$xlim_max - input$xlim_min)
        dt$Comp2_ <- 
          ((rangeY / rangeX) * dt[, 2] - input$ylim_min) / (input$ylim_max - input$ylim_min)
      } else {
        dt$Comp1_ <- 
          ((rangeX / rangeY) * dt[, 1] - input$xlim_min) / (input$xlim_max - input$xlim_min)
        dt$Comp2_ <- 
          (dt[, 2] - input$ylim_min) / (input$ylim_max - input$ylim_min)
      }
      
      pointsNear <- nearPoints(dt, 
                               xvar = "Comp1_", 
                               yvar = "Comp2_", 
                               input$plot_click)
      
      if (!is.na(row.names(pointsNear)[1])){
        if (row.names(pointsNear)[1] %in% v$vip){
          v$vipMouse <- v$vipMouse[v$vipMouse != row.names(pointsNear)[1]]
          if (length(v$vipMouse) == 0) v$vipMouse <- NULL
        } else {
          v$vipMouse <- c(v$vipMouse, row.names(pointsNear)[1])
        }
      }
    }
  )
  output$selectedVIP <- renderText(paste("Selected VIPs: Mouse = ",
                                         paste(v$vipMouse, collapse=", "),
                                         " , Keyboard = ",
                                         paste(vipKeyboard(), collapse=", ")))
  
  #----
  
  drawBiplot2d <- function() {
    
    # Update axes limits----
    observeEvent(
      input$resetXLimits,
      {
        updateNumericInput(session, "xlim_min", 
                           value = round(min(scaledScores()[,1]), 2) - 0.1)
        updateNumericInput(session, "xlim_max", 
                           value = round(max(scaledScores()[,1]), 2) + 0.1)
      })
    observeEvent(
      input$resetYLimits,
      {
        updateNumericInput(session, "ylim_min", 
                           value = round(min(scaledScores()[,2]), 2) - 0.1)
        updateNumericInput(session, "ylim_max", 
                           value = round(max(scaledScores()[,2]), 2) + 0.1)
      })
    
    updateNumericInput(session, "xlim_min", 
                       min = round(min(scaledScores()[,1]), 2) - 2,
                       max = round(min(scaledScores()[,1]), 2) - 0.01)
    updateNumericInput(session, "xlim_max", 
                       min = round(max(scaledScores()[,1]), 2) + 0.01,
                       max = round(max(scaledScores()[,1]), 2) + 2)
    updateNumericInput(session, "ylim_min", 
                       min = round(min(scaledScores()[,2]), 2) - 2,
                       max = round(min(scaledScores()[,2]), 2) - 0.01)
    updateNumericInput(session, "ylim_max", 
                       min = round(max(scaledScores()[,2]), 2) + 0.01,
                       max = round(max(scaledScores()[,2]), 2) + 2)
    #----
    
    # Match legend keys with points----
    observeEvent(
      input$matchKeysWithPoints,
      {
        updateTextInput(session, "group_legend_key_pch",
                        value = input$point_pch)
      }
    )
    #----
    
    # arrow settings----
    observeEvent(
      input$resetArrowFig,
      {
        updateTextInput(session, "arrow_fig",
                        value = "0.69,0.99,0.01,0.31")
      }
    )
    observeEvent(
      input$resetArrowLabelAdjOverride,
      {
        v$arrowLabelAdjOverrides <- NULL
        v$arrowLabelOverrideNotValid <- FALSE
      }
    )
    observeEvent(
      input$enterArrowLabelAdjOverride,
      {
        newOverride <- as.character(unlist(strsplit(input$arrow_label_adj_override,",")))
        if (length(newOverride) == 3 &
            !any(is.na(as.numeric(newOverride[2:3])))) {
          
          v$arrowLabelOverrideNotValid <- FALSE
          
          if (is.null(v$arrowLabelAdjOverrides)){
            v$arrowLabelAdjOverrides <- 
              data.frame(x=c(0),y=c(0),row.names = c("NULL"))
          } 
          
          if (newOverride[1] %in% row.names(v$arrowLabelAdjOverrides)) {
            v$arrowLabelAdjOverrides[newOverride[1],] <-
              as.numeric(newOverride[2:3])
          } else {
            v$arrowLabelAdjOverrides <- rbind(v$arrowLabelAdjOverrides,
                                              as.numeric(newOverride[2:3]))
            
            row.names(v$arrowLabelAdjOverrides)[nrow(v$arrowLabelAdjOverrides)] <- 
              newOverride[1]
          }
          v$arrowLabelAdjOverrides <- as.matrix(v$arrowLabelAdjOverrides)
          
          
        } else {
          v$arrowLabelOverrideNotValid <- TRUE
        }
      }
    )
    
    # observeEvent(
    #   input$resetArrowLabelCode,
    #   {
    #     print(str(v$varCodes))
    #     
    #     varsNames <- v$inputVariables
    #     if (is.null(varsNames))
    #       varsNames <- names(iris)[1:4]
    #     
    #     v$varCodes <- varsNames
    #     
    #     print(str(v$varCodes))
    #   }
    # )
    # observeEvent(
    #   input$enterArrowLabelCode,
    #   {
    #     varsNames <- v$inputVariables
    #     if (is.null(varsNames))
    #       varsNames <- names(iris)[1:4]
    #     
    #     newLabel <- as.character(unlist(strsplit(input$arrow_label_code,"=")))
    #     
    #     if (length(newLabel) == 2) {
    #       print(v$varCodes)
    #       if (newLabel[1] %in% varsNames){
    #         v$varCodes[varsNames == newLabel[1]] <- 
    #           newLabel[2]
    #       }
    #       else {
    #         warning("arrow label mapping is not valid. Variable ",
    #                 newLabel[1], " not found.")
    #       }
    #       print(v$varCodes)
    #       
    #     } else {
    #       warning("arrow label mapping format is not valid. Please enter 'VariableName=Label'")
    #     }
    #   }
    # )
    #----
    
    # reset VIP settings----
    observeEvent(
      input$resetVipPCH,
      {
        updateTextInput(session, "vip_pch",
                        value = "0,1,5,2,6,4,3")
      }
    )
    observeEvent(
      input$resetVipCEX,
      {
        updateTextInput(session, "vip_cex",
                        value = "2,2,2,2,2,1,1")
      }
    )
    #----
    ordination_object_ <- ordination_object()
    
    v$varCodes <- v$inputVariables
    
    groupVariable <- v$groupVariable
    
    if (input$useIris |
        (is.null(v$dataSetLoaded) | v$noCSV | v$noValidCSV | v$noVarSelected)) {
      
      groupVariable <- iris$Species
      
    }
    # if (is.null(v$varCodes))
    #   v$varCodes <- v$inputVariables
    # if (is.null(v$varCodes))
    #   v$varCodes <- names(iris)[1:4]
    
    # if (!is.null(v$varCodes))
    #   row.names(ordination_object_$loadings) <- v$varCodes
    
    
    biplot_2d(ordination_object_, 
              ordination_method = input$ordination_method,
              width = input$width,
              height = input$height,
              groups = groupVariable,
              vips = vips(),
              detach_arrows = input$detach_arrows,
              rows_over_columns = input$rows_over_columns,
              main_fig = main_fig(),
              fit_into_main = input$fit_into_main,
              invert_coordinates = invert_coordinates(),
              show_grid = input$grid,
              show_axes = input$axes,
              show_arrows = input$arrows,
              show_group_legend = input$group_legend,
              show_vip_legend = input$vip_legend,
              show_fitAnalysis = input$fitAnalysis,
              main_lwd = input$main_lwd,
              grid_cex = input$grid_cex,
              grid_font = input$grid_font,
              grid_adj = input$grid_adj,
              xlim = c(input$xlim_min, input$xlim_max),
              ylim = c(input$ylim_min, input$ylim_max),
              x_title = input$x_title,
              x_title_cex = input$x_title_cex,
              x_title_font = as.numeric(input$x_title_font),
              x_title_fig = x_title_fig(),
              y_title = input$y_title,
              y_title_cex = input$y_title_cex,
              y_title_font = as.numeric(input$y_title_font),
              y_title_fig = y_title_fig(),
              subtitle = subtitle(),
              subtitle_cex = input$subtitle_cex,
              subtitle_position = input$subtitle_position,
              point_type = input$point_type,
              point_pch = input$point_pch,
              point_size = input$point_size,
              point_label = point_label(),
              point_label_cex = input$point_label_cex,
              point_label_font = as.numeric(input$point_label_font),
              point_label_adj = c(input$point_label_adj_x, 
                                  input$point_label_adj_y),
              arrow_fig = arrow_fig(),
              arrow_mim_dist = input$arrow_mim_dist,
              arrow_length = input$arrow_length,
              arrow_color = arrow_color(),
              arrow_cex = input$arrow_cex,
              arrow_lwd = input$arrow_lwd,
              arrow_label_color = arrow_label_color(),
              arrow_label_cex = input$arrow_label_cex,
              arrow_label_font = as.numeric(input$arrow_label_font),
              arrow_label_adj = c(input$arrow_label_adj_x, 
                                  input$arrow_label_adj_y),
              arrow_label_adj_override = v$arrowLabelAdjOverrides,
              group_color = group_color(),
              group_star_cex = input$group_star_cex,
              group_ellipse_cex = input$group_ellipse_cex,
              group_ellipse_axes = input$group_ellipse_axes,
              group_label_cex = input$group_label_cex,
              group_legend_title = input$group_legend_title,
              group_legend_title_pos = c(input$group_legend_title_pos_x, 
                                         input$group_legend_title_pos_y),
              group_legend_title_cex = input$group_legend_title_cex,
              group_legend_title_font = as.numeric(input$group_legend_title_font),
              group_legend_title_adj = input$group_legend_title_adj,
              group_legend_fig = group_legend_fig(),
              group_legend_box_color = group_legend_box_color(),
              group_legend_key_pch = input$group_legend_key_pch,
              group_legend_key_cex = input$group_legend_key_cex,
              group_legend_key_lwd = input$group_legend_key_lwd,
              group_legend_key_margin = input$group_legend_key_margin,
              group_legend_text_margin = input$group_legend_text_margin,
              group_legend_text_color = group_legend_text_color(),
              group_legend_text_cex = input$group_legend_text_cex,
              group_legend_text_font = as.numeric(input$group_legend_text_font),
              group_legend_text_adj = input$group_legend_text_adj,
              vip_color = vip_color(),
              vip_pch = vip_pch(),
              vip_cex = vip_cex(),
              vip_lwd = input$vip_lwd,
              vip_font = as.numeric(input$vip_font),
              vip_adj = c(input$vip_adj_x,input$vip_adj_y),
              vip_legend_title = input$vip_legend_title,
              vip_legend_title_pos = c(input$vip_legend_title_pos_x, 
                                       input$vip_legend_title_pos_y),
              vip_legend_title_cex = input$vip_legend_title_cex,
              vip_legend_title_font = as.numeric(input$vip_legend_title_font),
              vip_legend_title_adj = input$vip_legend_title_adj,
              vip_legend_fig = vip_legend_fig(),
              vip_legend_box_color = vip_legend_box_color(),
              vip_legend_key_cex = input$vip_legend_key_cex,
              vip_legend_key_margin = input$vip_legend_key_margin,
              vip_legend_text_margin = input$vip_legend_text_margin,
              vip_legend_text_cex = input$vip_legend_text_cex,
              vip_legend_text_font = as.numeric(input$vip_legend_text_font),
              vip_legend_text_adj = input$vip_legend_text_adj,
              fitAnalysis_fig = fitAnalysis_fig(),
              fitAnalysis_lwd = input$fitAnalysis_lwd,
              fitAnalysis_screePlot_color = fitAnalysis_screePlot_color(),
              fitAnalysis_stress_cex = input$fitAnalysis_cex,
              fitAnalysis_stress_lab_cex = input$fitAnalysis_lab_cex,
              fitAnalysis_stress_axis_cex = input$fitAnalysis_axis_cex,
              fitAnalysis_stress_l_color =  fitAnalysis_stress_l_color(),
              fitAnalysis_stress_p_color =  fitAnalysis_stress_p_color(),
              
              output_type = c("preview")
    )
    
    par(mar = c(0, 0, 0, 0),
        fig = c(0, 1, 0, 1),
        usr = c(0,1,0,1)) # this corrects the user coord system
  }
  
  output$plot2d <- renderPlot({
    
    drawBiplot2d()
    
  })
  
  output$plot.ui <- renderUI({
    plotOutput("plot2d",
               width = input$width,
               height = input$height,
               click = clickOpts(id="plot_click", clip = F))
  })
  
  output$Biplot2D_download <- downloadHandler(
    filename = function() {return(paste("Biplot 2D", input$output_type, sep = "."))},
    content = function(file) {
      if (input$output_type == "png")
        png(file)
      else if (input$output_type == "eps")
        postscript(file)
      else if (input$output_type == "tiff")
        tiff(file)
      else if (input$output_type == "jpeg")
        jpeg(file)
      drawBiplot2d()
      dev.off()
    },
    contentType = paste("image/", input$output_type, sep = "")
  )

})
