library(shinyBS)
library(dtComb)
load("parameters.Rdata")
functionsData <- parameters$functions
functionsData <- as.matrix(functionsData)

generalParameters <- parameters$generalParameters
direction <- generalParameters[which(generalParameters[, 1] == "direction"), ]
cutoffMethod <- generalParameters[which(generalParameters[, 1] == "cutoff.method"), ]


ui <- fluidPage(
  includeCSS("www/css/bootstrap.min.css"),
  includeCSS("www/css/style.css"),
  tags$script(src = "js/site.js"),
  tags$head(HTML("<title>dtComb: Combining Two Diagnostic Tests</title>")),
  tags$head(HTML("<link rel='shortcut icon' type='image/x-icon' href='images/favicon2.png'>")),
  navbarPage(id = "generalTabSetPanel", HTML("<span><img src='images/Untitled.png' style='height:40px;margin-right:8px' /img> dtComb: Combining Two Diagnostic Tests</span>") ,
             tabPanel("About",
                      sidebarPanel(class="aboutSidebarPanel",HTML('<left><img src="images/func.png" style="width:100%;height:auto" ></center>'), width = 2),
                      
                      mainPanel( 
                        
                        h4(tags$b('Interactive and comprehensive tool for combining two diagnostic tests.')),
                        
                        HTML('<div class="row"> <div class="col-md-6"> <p align="justify" style="margin-left:45px;margin-top:24px">Diagnostic tests are widely used for purposes of distinguishing diseases from each other and making the correct
                                 diagnosis for the patients. Therefore, the role of the diagnostic tests in making decisions is crucial. In addition to having an essential role in medical diagnosis, these tests also contribute to planning the proper treatment while reducing treatment costs.
                                 The diagnostic accuracy performance and reliability of these diagnostic tests are taken into account when making these tests widely available. For some conditions, more than one diagnostic test may be available. Some of these diagnostic tests may even replace
                                 existing methods because they have better performance. In many studies, new diagnostic test performance measures with superior performance were obtained by using multiple diagnostic tests instead of one. The dtComb package includes diverse combination methods, 
                                data standardization, and resampling methods available for combining two diagnostic tests.  </p></div>
                                      <div class="col-md-6"><img src="images/roc.png" type="application/pdf"  style="width:auto;height:400px;margin-left:110px" ></div>
                                      </div>
                                      '),
                        
                        HTML('<p align="justify"> Here the web-tool application is presented to implement the combination approaches available in the dtComb package. This application allows users to upload their own data (Data Upload tab), build models (Analysis tab), present the results (Graphs tab)
                           and make new predictions (Predict tab) from the model built. More detailed information about the combination methods and approaches through this web-tool and dtComb  package can be found in the paper of the package. All source codes are in GitHub.</p>'),
                        HTML('<div class="row">
                                     <div class="col-lg-12" style="margin-top:35px">
                                      <div class="col-lg-4"><center><img src="images/plot2.png" style="width:65%;height:auto" ></center></div>
                                       <div class="col-lg-4"><center><img src="images/plot3.png" style="width:65%;height:auto" ></center></div>
                                        <div class="col-lg-4"><center><img src="images/plot4.png" style="width:65%;height:auto" ></center></div>
                                        </div>
                                        </div>
                                     '),  
                        HTML('<p align="justify"> If you use this tool for your research please cite: …</p>')
                        
                        
                        
                        ,width = 9),
             ),
             tabPanel("Data Upload",
                      sidebarPanel(
                        "Input data", width = 3,
                        radioButtons("dataInput", "", list("Upload a file" = 1,"Load example data" = 2), selected = 2),
                        conditionalPanel(condition = "input.dataInput == '2'",
                                         selectInput("selectMarker3", "Marker 1 variable", choices = NULL),
                                         selectInput("selectMarker4", "Marker 2 variable", choices = NULL),
                                         selectInput("selectStatus2", "Status variable", choices = NULL),
                                         selectInput("event2","Select category for cases", choices = NULL),
                                         h5(tags$b("Datasets:")),
                                         radioButtons("sampleData", "Examples", 
                                                      list("Abdominal pain data (n = 225, p = 3)" = 1,
                                                           "DMD data (n = 120, p = 5)" = 2,
                                                           "Simalation data (n = 500, p = 3)" = 3), 
                                                      selected = 1),
                                         tags$p(tags$b('n:'), ' number of observations'),
                                         HTML('<p><b>p</b>: number of variables</p>')
                        ),
                        conditionalPanel(condition = "input.dataInput == '1'",
                                         # HTML('<br>'),
                                         # h5("Upload a delimited text file (max. 30MB): "),
                                         #HTML('<i class="fa fa-beer fa-lg"></i>'),
                                         fileInput("upload", "", multiple = FALSE),
                                         radioButtons("fileSepDF", "Delimiter:", 
                                                      list("Comma" = 1, "Tab" = 2, "Semicolon" = 3, "Space" = 4),
                                                      selected = 2),
                                         # conditionalPanel(condition = "input.fileSepDF != '1'",
                                         #                  checkboxInput(inputId = "decimal", label = "Use comma as decimal", value = FALSE)
                                         # ),
                                         selectInput("selectMarker1", "Marker 1 variable", choices = NULL),
                                         selectInput("selectMarker2", "Marker 2 variable", choices = NULL),
                                         selectInput("selectStatus1", "Status variable", choices = NULL),
                                         selectInput("event1", "Select category for cases", choices = NULL),
                                         
                                         
                                         
                                         
                                         HTML('<br>'),
                                         HTML('<p>You can upload your data separated by comma, tab, semicolon or space.</p>'),
                                         HTML('<p><b>Note</b>: First row must be the header including the variable names.</p>')
                        ),
                      ),
                      mainPanel(width = 9, DT::dataTableOutput("RawData"))
             ),
             tabPanel("Analysis", 
                      sidebarPanel(
                        "", width = 3,
                        selectInput("functions",
                                    "Combination approach",
                                    choices = functionsData[,1],
                                    selected = FALSE,
                                    multiple = FALSE),
                        selectInput("methods",
                                    "Combination method",
                                    choices = NULL,
                                    selected = FALSE,
                                    multiple = FALSE),
                        uiOutput("hiddenElementMethodSub"),                     
                        uiOutput("hiddenElement"),                    
                        uiOutput("hiddenElementFunction"), 
                        
                        
                        uiOutput("hiddenElementFunctionSub"), 
                        
                        checkboxInput("chxAdvanced","Advanced",value=0),
                        conditionalPanel(id ="conditionAdvanced", condition = "input.chxAdvanced == '1'",
                                         selectInput("cutoffmethod",
                                                     "Select a method for optimal cut-off",
                                                     choices = as.matrix(unlist(strsplit(as.character(cutoffMethod[, 3]), ',', fixed=T))),
                                                     selected = FALSE,
                                                     multiple = FALSE),
                                         selectInput("direction",
                                                     "Direction",
                                                     choices = as.matrix(unlist(strsplit(as.character(direction[, 3]), ',', fixed=T))),
                                                     selected = FALSE,
                                                     multiple = FALSE),
                                         numericInput("conflevel",
                                                      "Confidence Level",
                                                      value = 0.95,
                                                      step = 0.01,
                                                      max = 1,
                                                      min = 0)
                        )
                        
                        ,
                        actionButton("goButton", "Go!", style="color: #fff; background-color: #2db83d; border-color: #2e6da4"),
                        downloadButton('downloadModel', 'Download Model' ,style="display:none")     
                        
                      ),
                      mainPanel(id="outputModelFit",
                                width = 9,
                                tabsetPanel(id = "tabGroupPlot",
                                            tabPanel(id="result",title= "Results",
                                                     bsCollapse(id = "collapseExample", open = "AUC Table",
                                                                bsCollapsePanel("ROC Coordinates", "",
                                                                                tabsetPanel(id = "tabGroupCoordinates",
                                                                                            tabPanel(id="coordinatCombination",title = "Combination Score",
                                                                                                     DT::dataTableOutput("rOCCoordinatesCombination")
                                                                                            ),
                                                                                            tabPanel(id="coordinatMarker1",title = "Marker 1",
                                                                                                     DT::dataTableOutput("rOCCoordinatesMarker1")
                                                                                            ),
                                                                                            tabPanel(id="coordinatMarker2",title = "Marker 2",
                                                                                                     DT::dataTableOutput("rOCCoordinatesMarker2")
                                                                                            )
                                                                                ), style = "rocCurv"),
                                                                bsCollapsePanel("AUC Table", "",
                                                                                DT::dataTableOutput("AUCTable")
                                                                                , style = "aucTable"),
                                                                bsCollapsePanel("Multiple Comparison Table", "",
                                                                                DT::dataTableOutput("MultCombTable")
                                                                                , style = "multComb"),
                                                                bsCollapsePanel("Cut Points", "",
                                                                                tabsetPanel(id = "tabGroupCutPoints",
                                                                                            tabPanel(id="TabCutPointsCombination",title = "Combination Score",
                                                                                                     DT::dataTableOutput("CutPointsCombination")
                                                                                            ),
                                                                                            tabPanel(id="TabCutPointsMarker1",title = "Marker 1",
                                                                                                     DT::dataTableOutput("CutPointsMarker1")
                                                                                            ),
                                                                                            tabPanel(id="TabCutPointsMarker2",title = "Marker 2",
                                                                                                     DT::dataTableOutput("CutPointsMarker2")
                                                                                            )
                                                                                ), style = "multComb"),
                                                                
                                                                bsCollapsePanel("Performance Measures", "",
                                                                                tabsetPanel(id = "tabGroupDiagStat",
                                                                                            tabPanel(id="tabDiagStatCombination",title = "Combination Score",
                                                                                                     DT::dataTableOutput("DiagStatCombination"),
                                                                                                     HTML('<br>'),
                                                                                                     DT::dataTableOutput("DiagStatCombinationDetail")
                                                                                            ),
                                                                                            tabPanel(id="tabDiagStatMarker1",title = "Marker 1",
                                                                                                     DT::dataTableOutput("DiagStatMarker1"),
                                                                                                     HTML('<br>'),
                                                                                                     DT::dataTableOutput("DiagStatMarker1Detail")
                                                                                            ),
                                                                                            tabPanel(id="tabDiagStatMarker2",title = "Marker 2",
                                                                                                     DT::dataTableOutput("DiagStatMarker2"),
                                                                                                     HTML('<br>'),
                                                                                                     DT::dataTableOutput("DiagStatMarker2Detail")
                                                                                            )
                                                                                )
                                                                                , style = "diagStat")
                                                                
                                                     ),
                                                     
                                                     # verbatimTextOutput("modelFit"),
                                                     # verbatimTextOutput("modelFitAUC"),
                                                     # verbatimTextOutput("modelFitStat")
                                            ),
                                            tabPanel(id ="plot",title= "Plots", 
                                                     fluidRow(
                                                       column(10, align="center",
                                                              
                                                              plotOutput(outputId = "ROCplot", width  = "500px",height = "400px")
                                                       ),
                                                       column(2, align="right",
                                                              actionButton("printRocBtn", "print",icon("print"))
                                                       )),
                                                     fluidRow(
                                                       column(12, align="left",
                                                              bsCollapse(id = "collapsePlot", open = "",
                                                                         
                                                                         bsCollapsePanel("Distrubution Graphs", "",
                                                                                         tabsetPanel(id = "tabGroupdistribution",
                                                                                                     tabPanel(id="tabDistplotC",title = "Combination Score",
                                                                                                              fluidRow(
                                                                                                                column(10, align="center",
                                                                                                                       plotOutput(outputId = "DistplotC", width  = "500px",height = "400px")
                                                                                                                ),
                                                                                                                column(2, align="right",
                                                                                                                       actionButton("printDistplotCBtn", "print",icon("print"))
                                                                                                                ))
                                                                                                     ),
                                                                                                     tabPanel(id="tabDistplot2",title = "Marker 1",
                                                                                                              fluidRow(
                                                                                                                column(10, align="center",
                                                                                                                       plotOutput(outputId = "Distplot2", width  = "500px",height = "400px")
                                                                                                                ),
                                                                                                                column(2, align="right",
                                                                                                                       actionButton("printDistplot2Btn", "print",icon("print"))
                                                                                                                ))
                                                                                                     ),
                                                                                                     tabPanel(id="tabDistplot3",title = "Marker 2",
                                                                                                              fluidRow(
                                                                                                                column(10, align="center",
                                                                                                                       plotOutput(outputId = "Distplot3", width  = "500px",height = "400px") 
                                                                                                                ),
                                                                                                                column(2, align="right",
                                                                                                                       actionButton("printDistplot3Btn", "print",icon("print"))
                                                                                                                ))
                                                                                                     )
                                                                                                     
                                                                                         ), style = "distrCurv"),
                                                                         bsCollapsePanel("Scatter Graphs", "",
                                                                                         tabsetPanel(id = "tabGroupSctplot2",
                                                                                                     tabPanel(id="tabSctplotC",title = "Combination Score",
                                                                                                              fluidRow(
                                                                                                                column(10, align="center",
                                                                                                                       plotOutput(outputId = "SctplotC", width  = "500px",height = "400px") 
                                                                                                                ),
                                                                                                                column(2, align="right",
                                                                                                                       actionButton("printSctplotCBtn", "print",icon("print"))
                                                                                                                ))
                                                                                                     ),
                                                                                                     tabPanel(id="tabSctplot2",title = "Marker 1",
                                                                                                              fluidRow(
                                                                                                                column(10, align="center",
                                                                                                                       plotOutput(outputId = "Sctplot2", width  = "500px",height = "400px") 
                                                                                                                ),
                                                                                                                column(2, align="right",
                                                                                                                       actionButton("printSctplot2Btn", "print",icon("print"))
                                                                                                                ))
                                                                                                     ),
                                                                                                     tabPanel(id="tabSctplot3",title = "Marker 2",
                                                                                                              fluidRow(
                                                                                                                column(10, align="center",
                                                                                                                       plotOutput(outputId = "Sctplot3", width  = "500px",height = "400px")
                                                                                                                ),
                                                                                                                column(2, align="right",
                                                                                                                       actionButton("printSctplot3Btn", "print",icon("print"))
                                                                                                                ))
                                                                                                     )
                                                                                         )
                                                                                         , style = "scattCurv"),
                                                                         bsCollapsePanel("Sens. & Spec. Curve", "",
                                                                                         tabsetPanel(id = "tabGroupSensSpeplot2",
                                                                                                     tabPanel(id="tabSctplotC",title = "Combination Score",
                                                                                                              fluidRow(
                                                                                                                column(10, align="center",
                                                                                                                       plotOutput(outputId = "SensSpecPlotC", width  = "500px",height = "400px") 
                                                                                                                ),
                                                                                                                column(2, align="right",
                                                                                                                       actionButton("printSensSpeplotCBtn", "print",icon("print"))
                                                                                                                ))
                                                                                                     ),
                                                                                                     tabPanel(id="tabSctplot2",title = "Marker 1",
                                                                                                              fluidRow(
                                                                                                                column(10, align="center",
                                                                                                                       plotOutput(outputId = "SensSpecPlot2", width  = "500px",height = "400px") 
                                                                                                                ),
                                                                                                                column(2, align="right",
                                                                                                                       actionButton("printSensSpeplot2Btn", "print",icon("print"))
                                                                                                                ))
                                                                                                     ),
                                                                                                     tabPanel(id="tabSctplot3",title = "Marker 2",
                                                                                                              fluidRow(
                                                                                                                column(10, align="center",
                                                                                                                       plotOutput(outputId = "SensSpecPlot3", width  = "500px",height = "400px")
                                                                                                                ),
                                                                                                                column(2, align="right",
                                                                                                                       actionButton("printSenSpeplot3Btn", "print",icon("print"))
                                                                                                                ))
                                                                                                     )
                                                                                         )
                                                                                         , style = "SensSpecPlot")
                                                                         
                                                              )))
                                                     
                                                     
                                                     
                                                     
                                            )
                                ))
             ),
             
             tabPanel("Predict",
                      
                      sidebarPanel(
                        "Input new data",width = 3,
                        HTML('<br>'),
                        
                        checkboxInput(inputId = "uploadModel", label = "Upload Model", value = FALSE),
                        conditionalPanel(condition = "input.uploadModel == '1'",
                                         fileInput("modelUpload", "Upload Model", multiple = FALSE, accept = ".rda"),
                        ),
                        #HTML('<i class="fa fa-beer fa-lg"></i>'),
                        fileInput("newUpload", "Upload Data", multiple = FALSE),
                        radioButtons("fileSepDF2", "Delimiter:", 
                                     list("Comma" = 1, "Tab" = 2, "Semicolon" = 3, "Space" = 4),
                                     selected = 2),
                        
                        
                        
                        HTML('<br>'),
                        HTML('<p>You can upload your data separated by comma, tab, semicolon or space.</p>'),
                        HTML('<p><b>Note</b>: First row must be the header including the variable names.</p>'),
                        actionButton("calculate","Calculate")
                        
                      ),
                      mainPanel(id="outputPredictMainPanel", width = 9,
                                tabsetPanel(id = "tabGroupres",
                                            tabPanel(id="data",title= "Data Upload",
                                                     DT::dataTableOutput("TestData")
                                            ),
                                            
                                            tabPanel(id="result2",title= "Result",
                                                     
                                                     verbatimTextOutput("warningMessage"),
                                                     DT::dataTableOutput("outputPredict")))
                      )
                      
             ),
             
             tabPanel(title="Manual",
                      sidebarPanel(class="aboutSidebarPanel",includeHTML("manuelMenu.html"),width = 3),
                      mainPanel(
                        h3("Usage of the web-tool:"),
                        h4("Data upload", id = "DataUpload"),
                        HTML('<p> Load your data set in *.txt file format using this tab.</p>'),
                        HTML('<ul><li><p> Rows must represent the observations and each column must represent the variables.</p></li></ul>'),
                        HTML('<ul><li><p> First row must be a header which indicates the variable names.</p></li></ul>'),
                        HTML('<div style="left;"><img src="images/manual/dataUpload.png" width=400/><img src="images/manual/dataUpload2.png" width=840/></div>'),
                        HTML('<br>'),
                        
                        h4("Combination Approach",id="combAppro"),
                        HTML('<p>Use this tab to combine diagnostic tests. <b>dtComb</b> supports 142 combination methods for combining diagnostic tests.</p>'),
                        HTML('<ul><li><p>First, it is decided which one to choose in the 4 main combination approaches. There are 8 combination methods in linear combination approach, 7 in non-linear combination approach, 14 in mathematical operators and 113 combination methods in machine learning algorithms.</p></li></ul>'),
                        HTML('<ul><li><p>After selected the combination approach, one of the methods available in the approach is selected. </p></li></ul>'),
                        HTML('<div style="left;"><img src="images/manual/CombinationApp.png" width=400/></div>'),
                        
                        HTML('<b id = "linComb">Linear Combination Methods</b> '),
                        HTML('<ul><li><p> <i> Scoring </i></p></li></ul>'),
                        HTML('<ul> The binary logistic regression model is used. However, for a more straightforward interpretation, slope values are rounded to a given <b>digit number</b>, and the combination score is computed. </ul>'),
                        HTML('<ul><li><p> <i> Su & Liu’s </i></p></li></ul>'),
                        HTML('<ul> Su and Liu’s combination score is obtained by using <b>Fisher’s discriminant function</b> under the assumption of a multivariate normal distribution model and proportional covariance matrices.</ul>'),
                        HTML('<ul><li><p> <i> Logistic regression </i></p></li></ul>'), 
                        HTML('<ul> A binary logistic regression model is fitted using the <b>maximum-likelihood method</b>.</ul>'),
                        HTML('<ul><li><p> <i> Min-Max </i></p></li></ul>'), 
                        HTML('<ul> This method linearly combines the minimum and maximum values of the markers by finding a <b> parameter λ</b> that maximizes the corresponding <b>Mann-Whitney statistic</b>.</ul>'),
                        HTML('<ul><li><p> <i> Pepe & Thompson’s </i></p></li></ul>'), 
                        HTML('<ul>Uses the same binary logistic regression model. The combination score is obtained by proportioning the slope values to calculate the <b>λ parameter</b> .</ul>'),
                        HTML('<ul><li><p> <i> Pepe, Cai & Langton’s  </i></p></li></ul>'),    
                        HTML('<ul>Pepe, Cai, and Langton combination score is obtained by using <b>AUC as the parameter</b> of a logistic regression model</ul>'),
                        HTML('<ul><li><p> <i> Minimax </i></p></li></ul>'), 
                        HTML('<ul>Minimax method is an extension of Su & Liu’s method.</ul>'),
                        HTML('<ul><li><p> <i> Todor & Saplacan’s</i></p></li></ul>'), 
                        HTML('<ul>Todor and Saplacan’s method uses trigonometric functions to calculate the combination score. The combination score is obtained by the <b>θ value</b> that optimizes the corresponding AUC.</ul>'),
                        HTML('<div style="left;"><img src="images/manual/linComb.png" width=400/></div>'),
                        
                        HTML('<b id="nonlinComb">Nonlinear Combination Methods</b> '),
                        HTML('<ul><li><p> <i> Polynomial Regression </i></p></li></ul>'),
                        HTML('<ul> The method builds a logistic regression model with the feature space created and returns the probability of a positive event for each observation. It is implemented with <b>degrees</b> of the fitted polynomials taken from the user.</ul>'),
                        HTML('<ul><li><p> <i> Ridge Regression </i></p></li></ul>'),
                        HTML('<ul> Ridge regression is a penalizing method used to estimate the coefficients of highly correlated variables and in this case the polynomial feature space created from two biomarkers. For the implementation of the method, <a href="https://cran.r-project.org/web/packages/glmnet/index.html" target="_blank"> glmnet</a> library  is used with two functions: <a href="https://www.rdocumentation.org/packages/glmnet/versions/4.1-4/topics/cv.glmnet" target="_blank"> cv.glmnet()</a> to run a cross validation  model to determine the tuning parameter <b>λ</b> and <a href="https://www.rdocumentation.org/packages/glmnet/versions/4.1-4/topics/glmnet" target="_blank"> glmnet()</a> to fit the model with the selected tuning parameter.It is implemented with <b>degrees</b> of the fitted polynomials taken from the user.</ul>'),
                        HTML('<ul><li><p> <i> Lasso Regression </i></p></li></ul>'), 
                        HTML('<ul> Lasso regression is also a penalizing method with one difference is that at the end this method returns the coefficients of <b>some features as 0</b>, makes this method useful for feature elimination as well. The implementation is similar to Ridge regression, cross validation for parameter selection and model fit are implemented with <a href="https://cran.r-project.org/web/packages/glmnet/index.html" target="_blank"> glmnet</a> library.It is implemented with <b>degrees</b> of the fitted polynomials taken from the user.</ul>'),
                        HTML('<ul><li><p> <i> Elastic-Net Regression </i></p></li></ul>'), 
                        HTML('<ul> Elastic Net regression is obtained by combining the penalties of Ridge and Lasso regression to get the best of both models. The model again includes a tuning parameter λ as well as a <b>mixing parameter α</b> taken form the user which takes a value between 0 (ridge) and 1 (lasso) to determine the weights of the loss functions of Ridge and Lasso regressions. It is implemented with <b>degrees</b> of the fitted polynomials taken from the user.</ul>'),
                        HTML('<b>In nonlinear approaches, polynomial, ridge and lasso regression methods, an interaction that may exist between two diagnostic tests can be included in the model. For this, the Include of interaction option must be selected as TRUE.</b>'),
                        HTML('<ul><li><p> <i> Splines </i></p></li></ul>'), 
                        HTML('<ul>With the applications of regression models in a polynomial feature space the second non-linear approach to combining biomarkers comes from applying several regression models to the dataset using a function derived from piecewise polynomials. Splines are implemented with <b>degrees of freedom</b> and <b>degrees</b> of the fitted polynomials taken from the user. For the implementation <a href="https://www.rdocumentation.org/packages/splines/versions/3.6.2" target="_blank"> splines</a> library  is used to build piecewise logistic regression models with base splines.</ul>'),
                        HTML('<ul><li><p> <i> Smoothing Splines  and Natural Cubic Splines</i></p></li></ul>'),    
                        HTML('<ul>In addition to the basic spline structure, Generalized Additive Models are applied with natural cubic splines and smoothing splines using the <a href="https://cran.r-project.org/web/packages/gam/index.html" target="_blank"> gam</a> library in R. It is implemented with <font face="consolas">degrees of freedom</font> taken from the user.</ul>'),
                        HTML('<div style="left;"><img src="images/manual/nonlin.png" width=400/></div>'),
                        
                        HTML('<b id="mathComb">Mathematical Operators</b>'),
                        HTML('<ul><li><p> <i> Arithmetic Operators: </i></p></li></ul>'),
                        HTML('<ul> <font face="consolas">Add</font>, <font face="consolas">Subtract</font>, <font face="consolas">Multiply</font> ve <font face="consolas">Divide</font> methods represent basic arithmetic operators.</ul>'),
                        HTML('<ul><li><p> <i> Distance Measures:</i></p></li></ul>'),
                        HTML('<ul> The distance measures included in the package are <font face="consolas">Euclidean</font>, <font face="consolas">Manhattan</font>, <font face="consolas">Chebyshev</font>, <font face="consolas">Kulczynski_d</font>, <font face="consolas">Lorentzian</font>, <font face="consolas">Avg</font>, <font face="consolas">Taneja</font>, and <font face="consolas">Kumar-Johnson</font>.</ul>'),
                        HTML('<ul><li><p> <i> Exponential functions </i></p></li></ul>'),
                        HTML('<ul> These methods, in which one of the two diagnostic tests is taken as base and the other as an exponent, are indicated by the names <font face="consolas">baseinexp</font> (markers<sub>1</sub> <sup>markers<sub>2</sub></sup>) and <font face="consolas">expinbase</font> (markers<sub>2</sub> <sup>markers<sub>1</sub></sup>).</ul>'),
                        HTML('<div style="left;"><img src="images/manual/mathComb.png" width=400/></div>'),
                        
                        HTML('<b id="mlComb">Machine-Learning Algorithms</b>'),
                        HTML('<ul><li><p> 113 machine learning algorithms available in the <a href="https://cran.r-project.org/web/packages/caret/index.html" target="_blank"> caret</a>, library which are used to train classification models using machine learning algorithms and make predictions using these models, are used within the scope of Machine-Learning Algorithms.IMPORTANT: See <a href="https://topepo.github.io/caret/available-models.html" target="_blank"> available-model</a> for further information about the methods used in this methods. All resampling and Preprocessing methods included in the Caret package can also be used in dtComb while content is ML algorithms.</p></li></ul>'),
                        HTML('<div style="left;"><img src="images/manual/mlComb.png" width=400/></div>'),
                        HTML('<br>'),
                        
                        h4("Resampling methods",id="resampling"),
                        HTML('<ul><li><p> <b> Cross-validation</b> performs with the <font face="consolas">number of folds</font></p></li></ul>'),
                        HTML('<ul><li><p> <b> Repeated cross-validation</b> performs with the <font face="consolas">number of repeats</font> and <font face="consolas">number of folds</font> </p></li></ul>'),
                        HTML('<ul><li><p> <b> Bootstrap</b> performs with the <font face="consolas">number of resampling iterations</font>.</p></li></ul>'),
                        HTML('<div style="left;"><img src="images/manual/resample.png" width=400/></div>'),
                        HTML('<br>'),
                        
                        
                        h4("Standardization methods",id="standardiza"),
                        HTML('<ul><li><p> <b>Range:</b> Standardization to a range between 0 and 1. </p></li></ul>'),
                        HTML('<ul><li><p> <b>z-Score:</b> Standardization using z scores with mean equals to 0 and standard deviation equals to 1. </p></li></ul>'),
                        HTML('<ul><li><p> <b>t-Score:</b>  Standardization using T scores. The range varies between usually 20 and 80.</p></li></ul>'),
                        HTML('<ul><li><p> <b>Mean:</b> Standardization with sample mean equals to 1.</li></ul>'),
                        HTML('<ul><li><p> <b>Deviance:</b>  Standardization with sample standard deviation equals to 1.</p></li></ul>'),
                        HTML('<div style="left;"><img src="images/manual/standardize.png" width=400/></div>'),
                        HTML('<br>'),
                        
                        h4("There are options under the Advanced checkbox, such as cuttoff method, direction, and confidence levels."),
                        HTML('There are 34 methods available to determine the optimum cutoff. Cutoff methods can be found in the <a href="https://cran.r-project.org/web/packages/OptimalCutpoints/index.html" target="_blank">OptimalCutpoints</a> package of R.'),
                        HTML('<div style="left;"><img src="images/manual/advanced.png" width=400/></div>'),
                        HTML('<br>'),
                        HTML('<br>'),
                        
                        h3("Outputs",id="outputs"),
                        HTML('<p> When the analysis is complete, the <b>Download Model</b> button appears. With the help of this button, then the user can download the model he has trained to make predictions.</p>'),
                        HTML('<div style="left;"><img src="images/manual/downloadbutton.png" width=900></div>'),
                        h4("ROC Curve"),
                        HTML('<p>An <b>ROC Curve</b> appears you when the analysis is complete. Here you can see ROC Curve of both the generated Combination Score, Marker 1 and Marker 2.</p>'),
                        HTML('<div style="left;"><img src="images/manual/ROCplot.png" width=900></div>'),
                        HTML('<br>'),
                        HTML('<br>'),
                        
                        h4("AUC Table"),
                        HTML('<p>Under <b>AUC Table</b> subtab, you can get area under the curve (AUC) value and its standard error, confidence interval and statistical significance, instantly. </p>'),
                        HTML('<div style="left;"><img src="images/manual/AUCtable.png" width=900></div>'),
                        HTML('<br>'),
                        HTML('<br>'),
                        
                        h4("ROC Coordinates"),
                        HTML('<p>Each false positive and true positive points can be found under <b>ROC Coordinates</b> subtab for each marker. </p>'),
                        HTML('<div style="left;"><img src="images/manual/roccordinate.png" width=900></div>'),
                        HTML('<br>'),
                        HTML('<br>'),
                        
                        h4("Multiple Comparisons Table"),
                        HTML('<p><b>Multiple Comparisons Table</b> alt tab can be used to make pairwise statistical comparisons for ROC curves of two markers and combination scores.</p>'),
                        HTML('<div style="left;"><img src="images/manual/multiTable.png" width=900></div>'),
                        HTML('<br>'),
                        HTML('<br>'),
                        
                        h4("Cut points"),
                        HTML('<ul><li><p>The user can see the cut-off method selected in the <b>advance section</b>, the Criterion and the cut-off point determined by this method. </p></li></ul>'),
                        HTML('<div style="left;"><img src="images/manual/cuttoff.png" width=900></div>'),
                        HTML('<br>'),
                        HTML('<br>'),
                        
                        h4("Performance Measures"),
                        HTML('<ul><li><p>The <b>confusion matrix</b> and <b>Performance Measures</b> created with the cutoff method chosen by the user can be seen.</p></li></ul>'),
                        HTML('<br>'),
                        HTML('<br>'),
                        HTML('<div style="left;"><img src="images/manual/perform.png" width=900></div>'),
                        
                        h4("Plots", id = "plots"),
                        # HTML('<ul><li><p> <b>Distribution graphs, Scatter graphs and Sensitivity&Specifity graphs</b> belonging to Combination score, Marker 1 and Marker 2 can be viewed in the relevant sub-tabs. </p></li></ul>'),
                        tags$ul( 
                          tags$li(
                            p(tags$b("Distribution graphs, Scatter graphs and Sensitivity&Specifity graphs"), "belonging to Combination score, Marker 1 and Marker 2 can be viewed in the relevant sub-tabs.")
                          )
                        ),
                        
                        HTML('<div style="left;"><img src="images/manual/distPlot.png" width=350><img src="images/manual/sct.png" width=350/><img src="images/manual/sensspe.png" width=350></div>'),
                        HTML('<br>'),
                        HTML('<br>'),
                        h4("Predict", id = "predict"),
                        HTML('<ul><li><p> The user can predict by loading the test data with the model that has been trained. The user can predict by loading the test data with the model that has been trained. Another option is to make a prediction on the same model by reloading the previously downloaded model. </p></li></ul>'),
                        HTML('<div style="left;"><img src="images/manual/predict1.png" width=500><img src="images/manual/predict2.png" width=500></div>'),
                        HTML('<br>'),
                        HTML('<br>'),
                        tags$br(),
                        tags$br(),
                        tags$br())
             )
             
  )
  
)
