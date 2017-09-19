
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Biplot 2D"),
  
  fluidRow(
    column(5,
           "",
           tabsetPanel(
             type = "tabs",
             tabPanel(
               "Main",
               tabsetPanel(
                 type = "tabs",
                 
                 tabPanel(
                   "Data set",
                   #----
                   strong("For now, this biplot 2D Shiny application only performs Principal Component Analysis (PCA).\nPlease provide data that meet the requirements (e.g., numeric variables). For ploting results of other ordination methods, please use the biplot2d3d package in a regular R session. Install it running 'devtools::install_github('Andros-Spica/biplot2d3d')'."),
                   checkboxInput("useIris",
                                 "Use example (iris)",
                                 value = TRUE),
                   conditionalPanel(
                     condition = "!input.useIris",
                     wellPanel(
                       id = "userDataPanel",
                       style = "overflow-y:scroll; max-height: 200px",
                       fileInput("userDataSet", 
                                 "Select a data set:",
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                       ),
                       actionButton("loadDataSet",
                                    "Load data set"),
                       conditionalPanel(
                         condition = "output.dataSetLoaded == 'TRUE'",
                         tags$hr(),
                         checkboxInput("header", "Header", TRUE),
                         selectInput(
                           "inputVariables", "Input variables:",
                           "",
                           multiple = TRUE
                         ),
                         selectInput(
                           "groupVariable", "Group variable:",
                           ""
                         ),
                         actionButton("selectVariables",
                                      "Select variables")
                       )
                     )
                   )
                   #----
                 ),
                 
                 tabPanel(
                   "Biplot options",
                   #----
                   selectInput("ordination_method",
                               "Ordination method:",
                               c("Principal Components Analysis" = "PCA"#,
                                 # "Principal Coordinates Analysis" = "PCoA", 
                                 # "Non-metric Multidimensional Scaling" = "NMDS",
                                 # "Linear Discriminant Analysis" = "LDA"
                                 ),
                               selected = "PCA"),
                   sliderInput("rows_over_columns",
                               "Scaling focus (observations = 1, variables = 0):",
                               min = 0,
                               max = 1,
                               value = 0.5)
                   #----
                 ),
                 
                 tabPanel(
                   "Aesthetics",
                   #----
                   numericInput("main_lwd",
                                "Line width (general)",
                                value = 2, step = 0.1,
                                min = 0.1, max = 10),
                   
                   numericInput("width",
                                "plot width:",
                                value = 400, step = 1,
                                min = 200, max = 600),
                   numericInput("height",
                                "plot height:",
                                value = 400, step = 1,
                                min = 200, max = 600),
                   textInput("main_fig",
                             "main plot borders (fig):",
                             value = "0,1,0,1"),
                   conditionalPanel(
                     condition = "output.maxMainPlot == 'FALSE'",
                     checkboxInput("fit_into_main",
                                   "Fit all elements into the main plot",
                                   value = FALSE)
                   )
                   #----
                 ),
                 
                 tabPanel(
                   "Save",
                   selectInput(
                     "output_type",
                     "Output type:",
                     choices = c("png", "tiff", "jpeg", "eps" ),
                     selected = "png"
                   ),
                   downloadButton('Biplot2D_download', 'Download Plot')
                 )
               )
               #----
             ),
             
             tabPanel(
               "Axes/grid",
               tabsetPanel(
                 tabPanel(
                   "Main",
                   #----
                   checkboxInput("axes",
                                 "Show axes",
                                 value = T),
                   checkboxInput("grid",
                                 "Show grid",
                                 value = T),
                   checkboxGroupInput("invert_coordinates",
                                      "Invert coordinates:",
                                      c("x" = 1, "y" = 2),
                                      inline = T),
                   numericInput("grid_cex",
                                "Grid text size:",
                                value = 1, step = 0.1,
                                min = 0.1, max = 10),
                   selectInput("grid_font",
                               "Grid text font:",
                               c("normal" = 1, "bold" = 2, 
                                 "italic" = 3, "bold italic" = 4),
                               selected = 1),
                   sliderInput("grid_adj",
                               "Grid text justification (adj):",
                               min = 0, max = 1,
                               value = 0.5)
                   #----
                 ),
                 
                 tabPanel(
                   "Axes titles",
                   #----
                   wellPanel(
                     id = "axesTitlesPanel",
                     style = "overflow-y:scroll; max-height: 300px",
                     titlePanel(div(h4("x axis"))),
                     inputPanel(
                       textInput("x_title",
                                 "title:",
                                 value = ""),
                       numericInput("x_title_cex",
                                    "size",
                                    value = 1, step = 0.1,
                                    min = 0.1, max = 10),
                       selectInput("x_title_font",
                                   "Font type:",
                                   c("normal" = 1, "bold" = 2, 
                                     "italic" = 3, "bold italic" = 4),
                                   selected = 2),
                       textInput('x_title_fig', 'position (fig)', "0.25, 1, 0.85, 1")
                     ),
                     titlePanel(div(h4("y axis"))),
                     inputPanel(
                       textInput("y_title",
                                 "title:",
                                 value = ""),
                       numericInput("y_title_cex",
                                    "Size:",
                                    value = 1, step = 0.1,
                                    min = 0.1, max = 10),
                       selectInput("y_title_font",
                                   "Font type:",
                                   c("normal" = 1, "bold" = 2, 
                                     "italic" = 3, "bold italic" = 4),
                                   selected = 2),
                       textInput('y_title_fig', 'position (fig)', "0.91, 1, 0, 1")
                     )
                   )
                   #----
                 ),
                 
                 tabPanel(
                   "Axes limits",
                   #----
                   wellPanel(
                     id = "axesLimitsPanel",
                     style = "overflow-y:scroll; max-height: 300px",
                     
                     titlePanel(div(h4("x limits"))),
                     actionButton("resetXLimits",
                                  "Default"),
                     inputPanel(
                       numericInput("xlim_min",
                                    "min",
                                    value = 1, step = 0.01,
                                    min = 0.1, max = 10),
                       numericInput("xlim_max",
                                    "max",
                                    value = 1, step = 0.01,
                                    min = 0.1, max = 10)
                     ),
                     
                     titlePanel(div(h4("y limits"))),
                     actionButton("resetYLimits",
                                  "Default"),
                     inputPanel(
                       numericInput("ylim_min",
                                    "min",
                                    value = 1, step = 0.01,
                                    min = 0.1, max = 10),
                       numericInput("ylim_max",
                                    "max",
                                    value = 1, step = 0.01,
                                    min = 0.1, max = 10)
                     )
                   )
                   #----
                 )
               )
             ),
             
             tabPanel(
               "Subtitle",
               #----
               textInput("subtitle",
                         "Subtitle:",
                         value = NULL),
               numericInput("subtitle_cex",
                            "Size:",
                            value = 1, step = 0.1,
                            min = 0.1, max = 10),
               selectInput("subtitle_position",
                           "Position:",
                           c("bottom left" = "bottomleft", "bottom right" = "bottomright", 
                             "top left" = "topleft", "top right" = "topright"),
                           selected = "bottomleft")
               #----
             ),
             
             tabPanel(
               "Point",
               #----
               wellPanel(
                 id = "pointPanel",
                 style = "overflow-y:scroll; max-height: 300px",
                 
                 selectInput("point_type",
                             "Font type:",
                             c("point", "label", "point and label"),
                             selected = "point"
                 ),
                 conditionalPanel(
                   condition = "input.point_type != 'label'",
                   numericInput("point_pch",
                                "shape (pch):",
                                value = 1, step = 1,
                                min = 0, max = 25),
                   numericInput("point_size",
                                "Size:",
                                value = 1, step = 0.01,
                                min = 0.01, max = 10)
                 ),
                 conditionalPanel(
                   condition = "input.point_type != 'point'",
                   titlePanel(div(h5(strong("labels")))),
                   inputPanel(
                     textInput("point_label",
                               "Text vector (comma separated):",
                               value = ""),
                     checkboxInput("useRowNamesAsLabels",
                                   "Use row names",
                                   value = T),
                     numericInput("point_label_cex",
                                  "Size:",
                                  value = 1, step = 0.01,
                                  min = 0.01, max = 10),
                     selectInput("point_label_font",
                                 "Font type:",
                                 c("normal" = 1, "bold" = 2, 
                                   "italic" = 3, "bold italic" = 4),
                                 selected = 3),
                     titlePanel(div(h5("Justification (adj):"))),
                     sliderInput("point_label_adj_x",
                                 "x",
                                 min = 0, max = 1,
                                 value = 0.5),
                     sliderInput("point_label_adj_y",
                                 "y",
                                 min = 0, max = 1,
                                 value = 0.5)
                   )
                 )
               )
               #----
             ),
             
             tabPanel(
               "Arrows",
               tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "Main",
                   #----
                   checkboxInput("arrows",
                                 "Show variable arrows",
                                 value = T),
                   checkboxInput("detach_arrows",
                                 "Detach variable's origin",
                                 value = T),
                   textInput("arrow_fig",
                             "Borders (fig):",
                             value = "0.69,0.99,0.01,0.31"),
                   actionButton("resetArrowFig", "Default"),
                   sliderInput("arrow_mim_dist",
                               "Variable relevance filter:",
                               value = 0, step = 0.01,
                               min = 0, max = 1),
                   conditionalPanel(
                     condition = "!input.detach_arrows",
                     numericInput("arrow_length",
                                  "Variable scaling factor:",
                                  value = 0.2, step = 0.01,
                                  min = 0.01, max = 1)
                   )
                   #----
                 ),
                 tabPanel(
                   "Aesthetics",
                   #----
                   textInput("arrow_color",
                             "Color vector (comma separated):",
                             value = "darkorange"),
                   numericInput("arrow_cex",
                                "Head size:",
                                value = 0.1, step = 0.01,
                                min = 0.01, max = 10),
                   numericInput("arrow_lwd",
                                "Line width (lwd):",
                                value = 2, step = 0.01,
                                min = 0.01, max = 10)
                   #----
                 ),
                 tabPanel(
                   "Labels",
                   #----
                   wellPanel(
                     id = "groupLegendPanel",
                     style = "overflow-y:scroll; max-height: 300px",
                     
                     # textInput("arrow_label_code",
                     #           "Abreviate a variable label (e.g. 'VariableName=Label')",
                     #           value = ""),
                     # actionButton("enterArrowLabelCode",
                     #              "replace label"),
                     # actionButton("resetArrowLabelCode",
                     #              "Reset labels"),
                     
                     textInput("arrow_label_color",
                               "Color vector (comma separated):",
                               value = "black"),
                     numericInput("arrow_label_cex",
                                  "Size:",
                                  value = 1, step = 0.01,
                                  min = 0.01, max = 10),
                     selectInput("arrow_label_font",
                                 "Font type:",
                                 c("normal" = 1, "bold" = 2, 
                                   "italic" = 3, "bold italic" = 4),
                                 selected = 1),
                     titlePanel(div(h4("Relative position (adj):"))),
                     sliderInput("arrow_label_adj_x",
                                 "x",
                                 min = 0, max = 1,
                                 value = 0.5),
                     sliderInput("arrow_label_adj_y",
                                 "y",
                                 min = 0, max = 1,
                                 value = 0.5),
                     textInput("arrow_label_adj_override",
                               "Override position (e.g. 'VariableName,0.1,0.8')",
                               value = ""),
                     actionButton("enterArrowLabelAdjOverride",
                                  "Add override"),
                     actionButton("resetArrowLabelAdjOverride",
                                  "Reset overrides")
                   )
                   #----
                 )
               )
             ),
             
             tabPanel(
               "Group",
               tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "Main",
                   #----
                   textInput("group_color",
                             "Color vector (comma separated):",
                             value = "darkred,darkgreen,darkblue"),
                   numericInput("group_star_cex",
                                "Star size:",
                                value = 1, step = 0.01,
                                min = 0.01, max = 10),
                   numericInput("group_ellipse_cex",
                                "Ellipse size:",
                                value = 1, step = 0.01,
                                min = 0.01, max = 10),
                   checkboxInput("group_ellipse_axes",
                                 "Ellipse axes",
                                 value = F),
                   numericInput("group_label_cex",
                                "Label size:",
                                value = 1, step = 0.01,
                                min = 0.01, max = 10)
                   #----
                 ),
                 
                 tabPanel(
                   "Legend",
                   wellPanel(
                     id = "groupLegendPanel",
                     style = "overflow-y:scroll; max-height: 300px",
                     
                     checkboxInput("group_legend",
                                   "Show legend",
                                   value = F),
                     
                     tabsetPanel(
                       type = "tabs",
                       tabPanel(
                         "Title",
                         #----
                         textInput("group_legend_title",
                                   "title:",
                                   value = "groups"),
                         titlePanel(div(h5(strong("position:")))),
                         inputPanel(
                           sliderInput("group_legend_title_pos_x",
                                       "x",
                                       min = 0, max = 1,
                                       value = 0.5),
                           sliderInput("group_legend_title_pos_y",
                                       "y",
                                       min = 0, max = 1,
                                       value = 0.85)
                         ),
                         numericInput("group_legend_title_cex",
                                      "Size:",
                                      value = 1, step = 0.01,
                                      min = 0.01, max = 10),
                         selectInput("group_legend_title_font",
                                     "Font type:",
                                     c("normal" = 1, "bold" = 2, 
                                       "italic" = 3, "bold italic" = 4),
                                     selected = 4),
                         sliderInput("group_legend_title_adj",
                                     "Justification (adj):",
                                     min = 0, max = 1,
                                     value = 0.5)
                         #----
                       ),
                       
                       tabPanel(
                         "Box",
                         #----
                         textInput("group_legend_fig",
                                   "Borders (fig):",
                                   value = "0.78, 0.99, 0.8, 0.95"),
                         
                         textInput("group_legend_box_color",
                                   "Fill color:",
                                   value = "white")
                         #----
                       ),
                       
                       tabPanel(
                         "Keys",
                         #----
                         numericInput("group_legend_key_pch",
                                      "shape (pch):",
                                      value = 15, step = 1,
                                      min = 0, max = 25),
                         actionButton("matchKeysWithPoints",
                                      "Match key with point"),
                         numericInput("group_legend_key_cex",
                                      "Size:",
                                      value = 1, step = 0.01,
                                      min = 0.01, max = 10),
                         numericInput("group_legend_key_lwd",
                                      "line width:",
                                      value = 1, step = 0.01,
                                      min = 0.01, max = 10),
                         numericInput("group_legend_key_margin",
                                      "left margin:",
                                      value = .15, step = 0.01,
                                      min = 0.01, max = 10)
                         #----
                       ),
                       
                       tabPanel(
                         "Text",
                         #----
                         numericInput("group_legend_text_margin",
                                      "left margin:",
                                      value = 0.25, step = 0.01,
                                      min = 0.01, max = 10),
                         textInput("group_legend_text_color",
                                   "colors:",
                                   value = "black"),
                         numericInput("group_legend_text_cex",
                                      "Size:",
                                      value = 1, step = 0.01,
                                      min = 0.01, max = 10),
                         selectInput("group_legend_text_font",
                                     "Font type:",
                                     c("normal" = 1, "bold" = 2, 
                                       "italic" = 3, "bold italic" = 4),
                                     selected = 1),
                         sliderInput("group_legend_text_adj",
                                     "Justification (adj):",
                                     min = 0, max = 1,
                                     value = 0)
                         #----
                       )
                     )
                   )
                 )
               )
             ),
             
             tabPanel(
               "VIPs",
               tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "Main",
                   #----
                   textInput("vipKeyboard",
                             "keyboard selected VIPs:",
                             value = "42")
                   #----
                   ),
                 tabPanel(
                   "Point",
                   #----
                   textInput("vip_color",
                             "Color vector (comma separated):",
                             value = "purple"),
                   textInput("vip_pch",
                             "Shape vector (pch, comma separated):",
                             value = "0,1,5,2,6,4,3"),
                   actionButton("resetVipPCH",
                                "Default"),
                   textInput("vip_cex",
                             "Size vector (comma separated):",
                             value = "2,2,2,2,2,1,1"),
                   actionButton("resetVipCEX",
                                "Default"),
                   numericInput("vip_lwd",
                                "Line width (lwd):",
                                value = 2, step = 0.01,
                                min = 0.01, max = 10),
                   
                   conditionalPanel(
                     condition = "output.vipAreCharacters == 'TRUE'",
                     titlePanel(div(h4(strong("If shape is a character vector:")))),
                     selectInput("vip_font",
                                 "Font type:",
                                 c("normal" = 1, "bold" = 2,
                                   "italic" = 3, "bold italic" = 4),
                                 selected = 1),
                     titlePanel(div(h5(strong("Justification (adj):")))),
                     inputPanel(
                       sliderInput("vip_adj_x",
                                   "x",
                                   min = 0, max = 1,
                                   value = 0.5),
                       sliderInput("vip_adj_y",
                                   "y",
                                   min = 0, max = 1,
                                   value = 0.5)
                     )
                   )
                   #----
                 ),
                 tabPanel(
                   "Legend",
                   
                   wellPanel(
                     id = "groupLegendPanel",
                     style = "overflow-y:scroll; max-height: 300px",
                     
                     checkboxInput("vip_legend",
                                   "Show legend",
                                   value = T),
                     
                     tabsetPanel(
                       type = "tabs",
                       tabPanel(
                         "Title",
                         #----
                         textInput("vip_legend_title",
                                   "title:",
                                   value = "VIPs"),
                         titlePanel(div(h5(strong("position:")))),
                         inputPanel(
                           sliderInput("vip_legend_title_pos_x",
                                       "x",
                                       min = 0, max = 1,
                                       value = 0.5),
                           sliderInput("vip_legend_title_pos_y",
                                       "y",
                                       min = 0, max = 1,
                                       value = 0.85)
                         ),
                         numericInput("vip_legend_title_cex",
                                      "Size:",
                                      value = 1, step = 0.01,
                                      min = 0.01, max = 10),
                         selectInput("vip_legend_title_font",
                                     "Font type:",
                                     c("normal" = 1, "bold" = 2,
                                       "italic" = 3, "bold italic" = 4),
                                     selected = 4),
                         sliderInput("vip_legend_title_adj",
                                     "Justification (adj):",
                                     min = 0, max = 1,
                                     value = 0.5)
                         #----
                       ),
                       
                       tabPanel(
                         "Box",
                         #----
                         textInput("vip_legend_fig",
                                   "Borders (fig):",
                                   value = "0.78, 0.99, 0.6, 0.75"),
                         
                         textInput("vip_legend_box_color",
                                   "Box color:",
                                   value = "white")
                         #----
                       ),
                       
                       tabPanel(
                         "Keys",
                         #----
                         numericInput("vip_legend_key_cex",
                                      "Size:",
                                      value = 0.8, step = 0.01,
                                      min = 0.01, max = 10),
                         numericInput("vip_legend_key_margin",
                                      "left margin:",
                                      value = .15, step = 0.01,
                                      min = 0.01, max = 10)
                         #----
                       ),
                       
                       tabPanel(
                         "Text",
                         #----
                         numericInput("vip_legend_text_margin",
                                      "left margin:",
                                      value = 0.25, step = 0.01,
                                      min = 0.01, max = 10),
                         numericInput("vip_legend_text_cex",
                                      "Size:",
                                      value = 1, step = 0.01,
                                      min = 0.01, max = 10),
                         selectInput("vip_legend_text_font",
                                     "Font type:",
                                     c("normal" = 1, "bold" = 2,
                                       "italic" = 3, "bold italic" = 4),
                                     selected = 1),
                         sliderInput("vip_legend_text_adj",
                                     "Justification (adj):",
                                     min = 0, max = 1,
                                     value = 0)
                         #----
                       )
                     )
                   )
                 )
               )
             ),
             
             tabPanel(
               "Fit analysis",
               #----
               checkboxInput("fitAnalysis",
                             "Show fit analysis",
                             value = T),
               textInput("fitAnalysis_fig",
                         "Borders (fig):",
                         value = "0.02, 0.35, 0.06, 0.25"),
               numericInput("fitAnalysis_lwd",
                            "Line width (lwd):",
                            value = 3, step = 0.01,
                            min = 0.01, max = 10),
               
               conditionalPanel(
                 condition = "input.ordination_method != 'NMDS'",
                 titlePanel(div(h4(strong("Scree plot")))),
                 textInput("fitAnalysis_screePlot_color",
                           "Bar fill color (highlight,regular):",
                           value = "grey,white")
               ),
               conditionalPanel(
                 condition = "input.ordination_method == 'NMDS'",
                 titlePanel(div(h4(strong("Stress plot")))),
                 numericInput("fitAnalysis_cex",
                              "General size (cex):",
                              value = 1, step = 0.01,
                              min = 0.01, max = 10),
                 numericInput("fitAnalysis_lab_cex",
                              "Label size (cex.lab):",
                              value = 1, step = 0.01,
                              min = 0.01, max = 10),
                 numericInput("fitAnalysis_axis_cex",
                              "Axis size (cex.axis):",
                              value = 1, step = 0.01,
                              min = 0.01, max = 10),
                 textInput("fitAnalysis_stress_p_color",
                           "Point color:",
                           value = "darkgrey"),
                 textInput("fitAnalysis_stress_l_color",
                           "Line color:",
                           value = "black")
               )
               #----
             )
           )
    ),
    column(7,
           "",
           uiOutput("plot.ui"),
           
           textOutput("selectedVIP"),
           
           textOutput("goodMessagePrint"),
           tags$head(tags$style("#goodMessagePrint{color: green;
                                font-size: 16px;
                                font-style: italic;
                                }")),
           
           textOutput("messagePrint"),
           tags$head(tags$style("#messagePrint{color: orange;
                                font-size: 16px;
                                font-style: italic;
                                }")),
           
           textOutput("warningPrint"),
           tags$head(tags$style("#warningPrint{color: red;
                                 font-size: 20px;
                                font-style: italic;
                                }"))
           
    )
  )
))
