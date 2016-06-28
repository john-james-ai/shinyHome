# shinyHome
# Real Estate Analytics and Forecasting
# John James
# Date: June 27, 2016

#ui.R

dashboardPage(skin = "green",
              dashboardHeader(title = "shinyHome"),
              dashboardSidebar(
                sidebarMenu(id = "sbm",
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                  menuItem("Market Explorer", tabName = "explorer", icon = icon("search")),
                  conditionalPanel(
                    condition = "input.sbm == 'valueAnalysis' || input.sbm == 'trainModels' || input.sbm == 'compareModels' || input.sbm == 'forecast'",
                    uiOutput("stateUi"),
                    uiOutput("countyUi"),
                    uiOutput("cityUi"),
                    uiOutput("zipUi")
                  ),
                  menuItem("Value Analyzer", tabName = "valueAnalysis", icon = icon("area-chart")),
                  menuItem("Forecast Modeler", icon = icon("line-chart"),
                           menuSubItem("Train Models", icon = icon("gears"),tabName = "trainModels"),
                           menuSubItem("Compare Models", icon = icon("check-circle"), tabName = "compareModels")),
                  menuItem("Market Forecaster", tabName = "forecast", icon = icon("bar-chart")),
                  menuItem("Help", tabName = "help", icon = icon("question-circle"),
                           menuSubItem("About shinyHome", icon = icon("user"),tabName = "helpAbout"),
                           menuSubItem("Welcome", icon = icon("coffee"),tabName = "helpWelcome"),
                           menuSubItem("Dashboard", icon = icon("dashboard"),tabName = "helpDashboard"),
                           menuItem("Market Explorer", icon = icon("search"),
                                       menuSubItem("Build a Query", icon = icon("search"), tabName = "helpBuildQuery"),
                                       menuSubItem("Market Report", icon = icon("bar-chart"), tabName = "helpMarketReport")),
                           menuItem("Value Analyzer", icon = icon("area-chart"),
                                    menuSubItem("Non-Seasonal Series", icon = icon("line-chart"), tabName = "helpNonSeasonal"),
                                    menuSubItem("Seasonal Series", icon = icon("bar-chart"), tabName = "helpSeasonal")),
                           menuItem("Forecast Modeler", icon = icon("bar-chart"),
                                    menuSubItem("Set Parameters", icon = icon("caret-square-o-right"), tabName = "helpSetParameters"),
                                    menuSubItem("Analyze Models", icon = icon("gears"), tabName = "helpAnalyzeModels"),
                                    menuSubItem("Compare Models", icon = icon("check-circle"), tabName = "helpCompareModels")),
                           menuSubItem("Market Forecaster", icon = icon("line-chart"),tabName = "helpMarketForecaster"))
                )# end of sidebarMenu
              ),#end of dashboardSidebar
              
              dashboardBody(
                includeCSS("www/custom.css"),
                tabItems(
                  tabItem(tabName = "dashboard",
                          fluidPage(
                            title = "Dashboard",
                            fluidRow(
                              column(width = 12,
                                valueBoxOutput("usViBox", width = 3),
                                valueBoxOutput("highestViBox", width = 3),
                                valueBoxOutput("usAnnualBox", width = 3),
                                valueBoxOutput("highestAnnualBox", width = 3)
                              )#end of column
                            ),# end of row
                            fluidRow(
                              column(width = 4,
                                     box(
                                       title = "Analytics for the Real Estate Market",
                                       width = 12,
                                       height = 530,
                                       background = "orange",
                                       solidHeader = FALSE,
                                       collapsible = FALSE,
                                       collapsed = FALSE,
                                       h3("Welcome to shinyHome"),
                                       p(
                                         paste("Here, we use statistical inference and forecast modeling techniques to 
                                               explore and forecast over 13,000 real estate markets in the United States.  
                                               This tool will enable you to:")),
                                       tags$ul(
                                         tags$li("get a snapshot and timeseries of the states and cities with the highest annual increase in median home values
                                                 on this", span("Dashboard page,", style = "color:white")),
                                         tags$li("explore home price indices and growth rates across various markets at several levels of granularity in 
                                                 the", span("Market Explorer,", style = "color:white")),
                                         tags$li("select a market and analyze and decompose price movements into their seasonal, trend and irregular components in the"
                                                 , span("Value Analyzer,", style = "color:white")),
                                         tags$li("train the most popular forecasting models and compare predictive accuracies in the", span("Forecast Modeler,", style = "color:white"), "and"),
                                         tags$li("use these models to forecast home prices in virtually every US real estate market in the", span("Market Forecaster.", 
                                                                                                                                                  style = "color:white"))
                                         ),
                                       p(
                                         paste("The menus to the left will walk you through the process of exploring markets, reviewing price trends, training 
                                               forecast models, evaluating model performance accuracy and predict home prices, 3, 5 or 10 years out.")),
                                       p(
                                         paste("To get started, click on the Market Explorer menu on the left.  For help, click on the help tab on 
                                               the sidebar panel.")),
                                       p("Enjoy!")
                                     )# end of box
                               ),# end of column
                              column(width = 8,
                                box(
                                  title = "Top 10 States by Annual Home Value Growth",
                                  status = "primary",
                                  width = 12,
                                  height = 255,
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  showOutput("top10StatesBar", "nvd3")
                                ),
                                box(
                                  title = "Top 10 Cities by Annual Home Value Growth",
                                  status = "primary",
                                  width = 12,
                                  height = 255,
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  showOutput("top10CitiesBar", "nvd3")
                                ) #End of Box
                              ) # End of column
                          ), # End of Fluid Row
                          fluidRow(
                            column(width = 6,
                                   box(
                                     title = "Top 10 States by Annual Home Value Growth Time Series",
                                     status = "primary",
                                     width = 12,
                                     solidHeader = FALSE,
                                     collapsible = TRUE,
                                     showOutput("top10StatesTS", "nvd3")
                                   ) #End of Box
                            ),# end of column
                            column(width = 6,
                                   box(
                                     title = "Top 10 Cities by Annual Home Value Growth Time Series",
                                     status = "primary",
                                     width = 12,
                                     solidHeader = FALSE,
                                     collapsible = TRUE,
                                     showOutput("top10CitiesTS", "nvd3")
                                   ) #End of Box
                            )# end of column
                          ),#end of fluidrow
                          fluidRow(
                            column(width = 12,
                               valueBoxOutput("numStatesBox", width = 3),
                               valueBoxOutput("numCountiesBox", width = 3),
                               valueBoxOutput("numCitiesBox", width = 3),
                               valueBoxOutput("numZipsBox", width = 3)
                            )# end of column
                          )# end of fluidrow
                      ) # End of fluidPage
                  ), # End of tabItem
                  tabItem(tabName = "explorer",
                    fluidPage(
                      title = "Market Explorer",
                      column(width = 2,
                        box(
                           title = "Query Builder",
                           status = "primary",
                           width = 12,
                           solidHeader = TRUE,
                           background = "navy",
                           box(
                             width = 12,
                             status = "primary",
                             solidHeader = FALSE,
                             background = "navy",
                             uiOutput("levelQueryUi")
                           ),# end of box
                           conditionalPanel(
                             condition = "input.analysisLevel == 2",
                             box(
                               status = "primary",
                               solidHeader = FALSE,
                               width = 12,
                               background = "navy",
                               uiOutput("stateQuery2Ui")
                             )# end of box
                           ),# end of conditional panel  
                           conditionalPanel(
                             condition = "input.analysisLevel == 3",
                             box(
                               status = "primary",
                               solidHeader = FALSE,
                               width = 12,
                               background = "navy",
                               uiOutput("stateQuery3Ui"),
                               uiOutput("countyQuery3Ui")
                             )# end of box
                           ),# end of conditionalpanel    
                           conditionalPanel(
                             condition = "input.analysisLevel == 4",
                             box(
                               status = "primary",
                               solidHeader = FALSE,
                               width = 12,
                               background = "navy",
                               uiOutput("stateQuery4Ui"),
                               uiOutput("countyQuery4Ui"),
                               uiOutput("cityQuery4Ui")
                             )# end of box
                           ),# end of conditionalpanel
                           box(
                             status = "primary",
                             solidHeader = FALSE,
                             width = 12,
                             background = "navy",
                             sliderInput("hviQuery", label = "Home Value Range ($000)", min = 0, max = 2000, value = c(0,1000)),
                             checkboxInput("maxValue", label = "Include all values exceeding $2m", value = FALSE)
                           ), # end of box
                           box(
                             status = "primary",
                             solidHeader = FALSE,
                             width = 12,
                             background = "navy",
                             selectInput("horizon", label = "Time Horizon:", 
                                         choices = c("Monthly", "Quarterly", "Annual", "5 Year", "10 Year"),
                                         selected = "Annual",
                                         selectize = FALSE),
                             numericInput("minGrowth", label = "Minimum Growth Rate (%)", value = 1)
                           ),# end of box
                           actionButton("query", label = "Go") 
                        )# end of box
                      ),# end of column
                      conditionalPanel(
                        condition = "input.query",
                        column(width = 10,
                            box(
                              title = "Market Data",
                              status = "primary",
                              width = 12,
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              fluidRow(
                                box(
                                  title = "Value Growth by Value Scatterplot",
                                  status = "primary",
                                  width = 12,
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  plotlyOutput("valueByGrowth")
                                )# end of box
                              ),# end of fluidrow
                              fluidRow(
                                column(width = 12,
                                  box(
                                    title = "Distribution of Median Home Values",
                                    status = "primary",
                                    width = 6,
                                    solidHeader = FALSE,
                                    collapsible = TRUE,
                                    plotOutput("valueHist")
                                  ),# end of box
                                  box(
                                    title = "Markets Table",
                                    status = "primary",
                                    width = 6,
                                    solidHeader = FALSE,
                                    collapsible = TRUE,
                                    dataTableOutput("marketTbl")
                                  )# end of box
                                ),# end of column
                                column(width = 12,
                                       box(
                                         title = "Top Markets by Growth",
                                         status = "primary",
                                         width = 12,
                                         height = 400,
                                         solidHeader = FALSE,
                                         collapsible = TRUE,
                                         showOutput("topByGrowth", "nvd3")
                                       )# end of box
                                )# end of column
                            ),# end of fluidRow
                            fluidRow(
                              box(
                                title = "Median Home Value Time Series for Top Growth Markets",
                                status = "primary",
                                width = 12,
                                height = 700,
                                solidHeader = FALSE,
                                collapsible = TRUE,
                                showOutput("topMarketsTS", "nvd3")
                              ) #End of Box
                            )# end of fluidrow
                          )# end of box
                        )#end of column
                      ) # end of conditionalpanel
                    ) # End of fluidPage
                ), # End of tabItem 
                tabItem(tabName = "valueAnalysis",
                        fluidPage(
                          fluidRow(
                             box(
                               status = "primary",
                               title = "Market Selector",
                               solidHeader = FALSE,
                               width = 3,
                               background = "navy",
                               p("Select a market to analyze, then press 'Go' to run the analysis"),
                               actionButton("analyze", label = "Go")
                             ),# end of box
                            conditionalPanel(
                              condition = "input.analyze",
                              valueBoxOutput("hviBox", width = 3),
                              valueBoxOutput("annualBox", width = 2),
                              valueBoxOutput("fiveYearBox", width = 2),
                              valueBoxOutput("tenYearBox", width = 2),
                              fluidRow(
                                column(width = 12,
                                  box(
                                    title = "Home Value Time Series Exploration", status = "primary",
                                    solidHeader = TRUE, height = 800, width = 12,
                                    tabBox(
                                      title = "Seasonal and Non-Seasonal Time Series Decomposition",
                                      id = "exploreTab", height = 660, width = 12,
                                      tabPanel("Non-Seasonal", 
                                               box(
                                                 title = "Span Order",
                                                 status = "success",
                                                 solidHeader = FALSE, width = 3,
                                                 p(
                                                   class = "text-muted",
                                                   paste("Adjust span order until the simple moving average has smoothed random fluctuations
                                                         and the trend component emerges")),
                                                 sliderInput("span", label = "Span Order", min = 1, max = 10, value = 3, step = 1)
                                                   ),
                                               box(
                                                 title = "Estimate Trend Component with Simple Moving Average (SMA)",
                                                 status = "success",
                                                 solidHeader = FALSE, height = 600, width = 9,
                                                 plotOutput("nsPlot")
                                               )# end of box
                                      ), # end of tabPanel
                                      tabPanel("Seasonal", 
                                               box(
                                                 title = "Estimate Trend Seasonal, and Irregular Components of the Time Series",
                                                 status = "success",
                                                 solidHeader = FALSE, height = 600, width = 12,
                                                 plotOutput("tsiPlot")
                                               )
                                      )# end of tab panel
                                    )# end of tabbox
                                  )# end of box
                                )# end of column
                              )#end of fluidrow
                            )# end of conditional panel
                          )# end of fluidrow
                        )#end of fluidPage
                ), # end of tabItem                  
                tabItem(tabName = "trainModels",
                        fluidPage(
                          column(width = 12,
                            fluidRow(
                              box(
                                title = "Model Training Parameters",
                                status = "primary", width = 12,
                                solidHeader = TRUE,
                                box(
                                  title = "Cross Validation",
                                  status = "primary", width = 3,
                                  solidHeader = FALSE,
                                  p(
                                    class = "text-muted",
                                    paste("The time series contains median housing prices, measured monthly, from 2000 thru 2015. Here, we
                            split the time series data into training and validation sets.  Indicate here, the end year for
                            the training set. The remaining years will be used to validate the predictions.")),
                                  sliderInput("split", label = "Training Set Split", min = 2004, max = 2014, value = 2014, step = 1)
                                ),# end of box
                                box(
                                  title = "Model Selection",
                                  status = "primary", width = 3,
                                  solidHeader = FALSE,
                                  p(
                                    class = "text-muted",
                                    paste("Select the forecast model algorithm.")),
                                  uiOutput("modelsUi")
                                ),# end of box
                                box(
                                  title = "Model Description",
                                  status = "primary", width = 6,
                                  solidHeader = FALSE,
                                  h3(textOutput("modelNameUi")),
                                  textOutput("modelDescUi"),
                                  br(),
                                  tags$strong("Please confirm that you have selected a market in the sidebar, then press 'Train Forecast Model' to train the selected model."),
                                  actionButton("train", label = "Train Forecast Model")
                                )# end of box
                              )# end of box
                          ),# end of fluidrow
                          conditionalPanel(
                            condition = "input.train",
                            fluidRow(
                              box(
                                title = "Prediction",
                                status = "primary", width = 7,
                                solidHeader = TRUE,
                                plotOutput("modelPlot", height = 460)
                              ),# end of box
                              box(
                                title = "Prediction Accuracy",
                                status = "primary", width = 5,
                                solidHeader = TRUE,
                                dataTableOutput("accuracy")
                              )# end of box
                            )# end of fluidrow
                          )# end of conditional panel
                      )# end of column
                    )# end of fluidPage
                ),# end of tabItem
                tabItem(tabName = "compareModels",
                        fluidPage(
                          fluidRow(
                             box(
                               status = "primary",
                               title = "Market Selector",
                               solidHeader = FALSE,
                               width = 3,
                               background = "navy",
                               p("Select a market to analyze, then press 'Go' to run the analysis"),
                               actionButton("compare", label = "Go")
                             ),# end of box
                            conditionalPanel(
                              condition = "input.compare",
                              valueBoxOutput("hviBox2", width = 3),
                              valueBoxOutput("annualBox2", width = 2),
                              valueBoxOutput("fiveYearBox2", width = 2),
                              valueBoxOutput("tenYearBox2", width = 2),
                              column(width = 12,
                                     fluidRow(
                                       box(
                                         status = "warning",
                                         width = 12,
                                         title = "Model Performance Summary",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         box(
                                           status = "warning",
                                           width = 6,
                                           title = "Model Performance Error Metrics",
                                           solidHeader = FALSE,
                                           selectInput("measurements", label = "Measurements", choices = 
                                                         c("ME: Mean Error" = "ME",
                                                           "RMSE: Root Mean Square Error" = "RMSE",
                                                           "MAE: Mean Absolute Error" = "MAE",
                                                           "MPE: Mean Percentage Error" = "MPE",
                                                           "MAPE: Mean Absolute Percentage Error" = "MAPE",
                                                           "MASE: Mean Absolute Scaled Error" = "MASE",
                                                           "ACF1: Autocorrelation of Errors at Lag 1" = "ACF1",
                                                           "Theil’s U" = "THEILS"),
                                                       multiple = FALSE, selectize = FALSE, selected = "MASE"),
                                           showOutput("measurementsBar", "nvd3")
                                         ),# end of box
                                         box(
                                           status = "warning",
                                           width = 6,
                                           title = "Model Performance Error Metrics",
                                           solidHeader = FALSE,
                                           dataTableOutput("modelsumm")
                                         )# end of box
                                       )# end of box
                                     ),#end of fluidrow
                                     fluidRow(
                                       box(
                                         status = "primary",
                                         width = 12,
                                         title = "Arima / ETS Model Performance",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                            box(
                                              status = "primary",
                                              width = 6,
                                              title = "Arima Model Performance",
                                              solidHeader = FALSE,
                                              plotOutput("arima")
                                            ),# end of box
                                            box(
                                              status = "primary",
                                              width = 6,
                                              title = "Exponential Smoothing (ETS) Model Performance",
                                              solidHeader = FALSE,
                                              plotOutput("ets")
                                            )# end of box
                                       )# end of box
                                     ),# end of fluidrow
                                     fluidRow(
                                       box(
                                         status = "primary",
                                         width = 12,
                                         title = "Naive / Neural Network Model Performance",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         box(
                                           status = "primary",
                                           width = 6,
                                           title = "Naive Model Performance",
                                           solidHeader = FALSE,
                                           plotOutput("naive")
                                         ),# end of box
                                         box(
                                           status = "primary",
                                           width = 6,
                                           title = "Neural Network Model Performance",
                                           solidHeader = FALSE,
                                           plotOutput("neural")
                                         )# end of box
                                       )# end of box
                                     ),# end of fluidrow
                                     fluidRow(
                                       box(
                                         status = "primary",
                                         width = 12,
                                         title = "BATS / TBATS Model Performance",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         box(
                                           status = "primary",
                                           width = 6,
                                           title = "BATS Model Performance",
                                           solidHeader = FALSE,
                                           plotOutput("bats")
                                         ),# end of box
                                         box(
                                           status = "primary",
                                           width = 6,
                                           title = "TBATS Model Performance",
                                           solidHeader = FALSE,
                                           plotOutput("tbats")
                                         )# end of box
                                       )# end of box
                                     ),# end of fluidrow
                                     fluidRow(
                                       box(
                                         status = "primary",
                                         width = 12,
                                         title = "STLM / STS Model Performance",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         box(
                                           status = "primary",
                                           width = 6,
                                           title = "STLM Model Performance",
                                           solidHeader = FALSE,
                                           plotOutput("stlm")
                                         ),# end of box
                                         box(
                                           status = "primary",
                                           width = 6,
                                           title = "STS Model Performance",
                                           solidHeader = FALSE,
                                           plotOutput("sts")
                                         )# end of box
                                       )# end of box
                                     )# end of fluidrow
                              )# end of column
                        )# end of conditional panel
                    )# end of fluidrow
                  )# end of fluidPage
                ),# end of tabItem
                tabItem(tabName = "forecast",
                        fluidPage(
                          fluidRow(
                                 box(
                                   status = "primary",
                                   title = "Forecast Options",
                                   solidHeader = FALSE,
                                   width = 4,
                                   background = "navy",
                                   p("Select a market to analyze, and number of years to forecast, then press 'Go' to run the analysis"),
                                   sliderInput("forecastRange", label = NULL, min = 1, 
                                               max = 10, value = 5),
                                   actionButton("forecast", label = "Go")
                                 ),# end of box
                            conditionalPanel(
                              condition = "input.forecast",
                              column(width = 8,
                                     fluidRow(
                                       valueBoxOutput("minPredictionBox", width = 4),
                                       valueBoxOutput("maxPredictionBox", width = 4),
                                       valueBoxOutput("meanPredictionBox", width = 4)
                                     ),# end of fluidrow
                                     fluidRow(
                                       valueBoxOutput("BATSBox", width = 2),
                                       valueBoxOutput("TBATSBox", width = 2),
                                       valueBoxOutput("STLMBox", width = 2),
                                       valueBoxOutput("STSBox", width = 2)
                                     )# end of fluidrow
                               ),#end of column
                              column(width = 12,
                                   fluidRow(
                                     box(
                                       status = "primary",
                                       width = 12,
                                       title = "Forecast Summary",
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       box(
                                         status = "primary",
                                         width = 8,
                                         height = 450,
                                         title = "Forecast Summary Plot",
                                         solidHeader = FALSE,
                                         tags$style(' {width: 900px}'),
                                         showOutput("forecastSummaryPlot", "nvd3")
                                       ),# end of box
                                       box(
                                         status = "primary",
                                         width = 4,
                                         height = 450,
                                         title = "Prediction Summary Plot",
                                         solidHeader = FALSE,
                                         showOutput("predictionPlot", "nvd3")
                                       )# end of box
                                     )# end of box
                                   ),# end of fluidrow
                                   fluidRow(
                                     box(
                                       status = "primary",
                                       width = 12,
                                       title = "Arima / ETS Model Forecast",
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       box(
                                         status = "primary",
                                         width = 6,
                                         title = "Arima Model Forecast",
                                         solidHeader = FALSE,
                                         plotOutput("arimaForecastPlot")
                                       ),# end of box
                                       box(
                                         status = "primary",
                                         width = 6,
                                         title = "Exponential Smoothing (ETS) Model Forecast",
                                         solidHeader = FALSE,
                                         plotOutput("etsForecastPlot")
                                       )# end of box
                                     )# end of box
                                   ),# end of fluidrow
                                   fluidRow(
                                     box(
                                       status = "primary",
                                       width = 12,
                                       title = "Naive / Neural Network Model Forecast",
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       box(
                                         status = "primary",
                                         width = 6,
                                         title = "Naive Model Forecast",
                                         solidHeader = FALSE,
                                         plotOutput("naiveForecastPlot")
                                       ),# end of box
                                       box(
                                         status = "primary",
                                         width = 6,
                                         title = "Neural Network Model Forecast",
                                         solidHeader = FALSE,
                                         plotOutput("neuralForecastPlot")
                                       )# end of box
                                     )# end of box
                                   ),# end of fluidrow
                                   fluidRow(
                                     box(
                                       status = "primary",
                                       width = 12,
                                       title = "BATS / TBATS Model Forecast",
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       box(
                                         status = "primary",
                                         width = 6,
                                         title = "BATS Model Forecast",
                                         solidHeader = FALSE,
                                         plotOutput("batsForecastPlot")
                                       ),# end of box
                                       box(
                                         status = "primary",
                                         width = 6,
                                         title = "TBATS Model Forecast",
                                         solidHeader = FALSE,
                                         plotOutput("tbatsForecastPlot")
                                       )# end of box
                                     )# end of box
                                   ),# end of fluidrow
                                   fluidRow(
                                     box(
                                       status = "primary",
                                       width = 12,
                                       title = "STLM / STS Model Forecast",
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       box(
                                         status = "primary",
                                         width = 6,
                                         title = "STLM Model Forecast",
                                         solidHeader = FALSE,
                                         plotOutput("stlmForecastPlot")
                                       ),# end of box
                                       box(
                                         status = "primary",
                                         width = 6,
                                         title = "STS Model Forecast",
                                         solidHeader = FALSE,
                                         plotOutput("stsForecastPlot")
                                       )# end of box
                                     )# end of box
                                   )# end of fluidrow
                                )# end of column
                              )# end of conditional panel
                          )# end of fluidrow
                        )# end of fluidPage
                      ),# end of tabItem
                tabItem(tabName = "helpAbout",
                        fluidPage(
                          column(width  = 4
                                 ),# end of column
                          column(width = 4,
                                 box(
                                   title = "About shinyHome",
                                   width = 12,
                                   background = "green",
                                   solidHeader = FALSE,
                                   collapsible = FALSE,
                                   collapsed = FALSE,
                                   h3("Introduction"),
                                   p(paste("Project Name: shinyHome")),
                                   p(paste("Coursera Building Data Products course, Data Science Specialization")),
                                   p(paste("Project Description: This shiny based application allows users to:")),
                                   tags$ul(
                                     tags$li("Explore current and historical median home value data for over 20,000 US markets"),
                                     tags$li("Analyze price trends using time series decomposition techniques"),
                                     tags$li("Create and evaluate prediction modules using eight time series forecasting algorithms"),
                                     tags$li("Forecast home values using the prediction algorithms")
                                   ),# end of tags$ul
                                   h3("Configuration"),
                                   p(
                                     paste("The application was written on R 3.3.1 for Windows.")),
                                   h3("Requirements"),
                                   p(
                                     paste("This application requires the following R packages:")),
                                   tags$ul(
                                     tags$li("datasaets"),
                                     tags$li("dplyr"),
                                     tags$li("forecast"),
                                     tags$li("ggplot2"),
                                     tags$li("plotly"),
                                     tags$li("plyr"),
                                     tags$li("rCharts"),
                                     tags$li("shiny"),
                                     tags$li("shinydashboard"),
                                     tags$li("TTR"),
                                     tags$li("xslx")
                                   ),#end of tags$ul
                                   h3("File Manifest"),
                                   p(
                                     paste("The file manifest is as follows:")),
                                   tags$ul(
                                     tags$li("currentCity: Current home value index and growth rates by city"),
                                     tags$li("currentCounty: Current home value index and growth rates by county"),
                                     tags$li("currentState:  Current home value index and growth rates by state"),
                                     tags$li("currentZip: Current home value index and growth rates by zip code"),
                                     tags$li("geo: State, county, city and zip code cross-reference file"),
                                     tags$li("hviAllCity: Historical home value data by city"),
                                     tags$li("hviAllCounty: Historical home value data by county"),
                                     tags$li("hviAllState: Historical home value data by state"),
                                     tags$li("hviAllZip: Historical home value data by zip code"),
                                     tags$li("models: Descriptions of forecasting algorithms employed")
                                   ),#end of tags$ul
                                   h3("Copyright"),
                                   p(
                                     paste("Copyright John James, 2016")),
                                   h3("Contact"),
                                   p(
                                     paste("John James, Developer, Maintainer, john.james.sf@gmail.com")),
                                   h3("Known Bugs"),
                                   p(
                                     paste("When running the structural model for time series by maximum 
                                           likelihood (StructTS) the application occasionally throws the following 
                                           error: Error in optim(start, f, method = method, hessian = TRUE, ...) :    
                                           L-BFGS-B needs finite values of 'fn'")),
                                   h3("Credits and Acknowledgements"),
                                   tags$ul(
                                     tags$li("Huge acknowledgement to Zillow Research for the data. http://www.zillow.com/research/data/"),
                                     tags$li("Ramnath Vaidyanathan  for a beautiful charting package"),
                                     tags$li("Joe Cheng for his active support in the githubsphere"),
                                     tags$li("Avril Coghlan for the not so little book on R for time series."),
                                     tags$li("The universe of shiny programmers that seem to have already asked and answered all my questions before I knew I had them")
                                   )#end of tags$ul
                                 )# end of box
                             )# end of column 
                        )# end of fluidpage
                ), #end of tabItem
                tabItem(tabName = "helpWelcome",
                        column(width = 4,
                               box(
                                   title = "Analytics for the Real Estate Market",
                                   width = 12,
                                   height = 550,
                                   background = "green",
                                   solidHeader = FALSE,
                                   collapsible = FALSE,
                                   collapsed = FALSE,
                                   h3("Welcome to shinyHome"),
                                   p(
                                     paste("Here, we use statistical inference and forecast modeling techniques to 
                                           explore and forecast over 13,000 real estate markets in the United States.  
                                           This tool will enable you to:")),
                                   tags$ul(
                                     tags$li("get a snapshot and timeseries of the states and cities with the highest annual increase in median home values
                                             on this", span("Dashboard page,", style = "color:white")),
                                     tags$li("explore home price indices and growth rates across various markets at several levels of granularity in 
                                             the", span("Market Explorer,", style = "color:white")),
                                     tags$li("select a market and analyze and decompose price movements into their seasonal, trend and irregular components in the"
                                             , span("Value Analyzer,", style = "color:white")),
                                     tags$li("train the most popular forecasting models and compare predictive accuracies in the", span("Forecast Modeler,", style = "color:white"), "and"),
                                     tags$li("use these models to forecast home prices in virtually every US real estate market in the", span("Market Forecaster.", 
                                                                                                                                              style = "color:white"))
                                     )#end of ul tag
                               )# end of box
                        ), #end of column
                        column(width = 4,
                               box(
                                 title = "Application Organization",
                                 width = 12,
                                 height = 550,
                                 background = "green",
                                 solidHeader = FALSE,
                                 collapsible = FALSE,
                                 collapsed = FALSE,
                                 h3("Explore, Train, Compare, Forecast"),
                                 p(
                                   paste("The app is organized to take you from market exploration, to selection, model training and forecasting.  
                                         The pages are summarized as follows:")),
                                 tags$ul(
                                   tags$li("Dashboard: Provides some basic statistics on home values and home value growth in the United States"),
                                   tags$li("Market Explorer: This is a query based page that allows you to explore markets by home value, rate
                                           of home value growth, and geography."),
                                   tags$li("Value Analyzer: Once you have selected a market, you can analyze price movements from a seasonal, 
                                           trend, and irregular components"),
                                   tags$li("Forecast Modeler - Train Models: You will select a market, designate a training and test set, select a 
                                                   forecast algorithm, train the model and evaluate its performance."),
                                   tags$li("Forecast Modeler - Compare Models: This page will allow you to run eight of the most popular forecast 
                                           algorithms, and compare their predictive accuracy, side-by-side."),
                                   tags$li("Market Forecaster: You will be able to select a market and forecast home values for up to 10 years.")),
                                 p(
                                   paste("To get started, click on the Market Explorer menu on the left.  For help, click on the help tab on 
                                         the sidebar panel.")),
                                 p(
                                   paste("Enjoy!"))
                               )# end of box
                         )# end of column
                ),# end oftabItem
                tabItem(tabName = "helpDashboard",
                        column(width = 4,
                               box(
                                 width = 12,
                                 background = "green",
                                 solidHeader = FALSE,
                                 collapsible = FALSE,
                                 collapsed = FALSE,
                                 h3("Dashboard"),
                                 p(
                                   paste("The Dashboard provides an introduction to the site and some basic 
                                         statistics on the US housing market such as:")),
                                 tags$ol(
                                   tags$li("US Home Value Index – Median Home Price [1]"),
                                   tags$li("Market with highest median home value [2]"),
                                   tags$li("The US annual growth in median home values [3]"),
                                   tags$li("Market with highest annual growth in home values [4]"),
                                   tags$li("The top 10 states by median home value growth [5]"),
                                   tags$li("The top 10 cities by median home value growth [6]"),
                                   tags$li("Top 10 states by median home value growth home value price time series [7]"),
                                   tags$li("Top 10 cities by median home value growth home value time series [8]")
                                 ),# end of tags$ol
                                 p(
                                   paste("The data set for this site includes current and historical home value 
                                         indices for over 20,000 markets.  The Market Explorer page will allow you to query 
                                         and report on markets from the state to the zip code level."))
                               )# end of box
                        ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "dashboard1.png", height = 400, width = 1020),
                                 img(src = "dashboard2.png", height = 300, width = 1020)
                                 )# end of box
                        )# end of column
                ), #end of tabItem
                tabItem(tabName = "helpBuildQuery",
                        column(width = 4,
                               box(
                                 title = "Market Explorer",
                                 width = 12,
                                 background = "green",
                                 solidHeader = FALSE,
                                 collapsible = FALSE,
                                 collapsed = FALSE,
                                 p(
                                   paste("The Market Explorer ranks real estate markets by median home value 
                                         and/or growth in home value according to your query that we help you build.  
                                         The query builder enables you to filter markets by home value range, rate of growth, 
                                         and geography.")),
                                 h3("Query Builder"),
                                 tags$strong("Set Level of Analysis [1]"),
                                 p(
                                   paste("You can analyze markets at four levels:")),
                                 tags$ol(
                                   tags$li("State: State level analysis including all 50 states"),
                                   tags$li("County: County level analysis, including one or more counties 
                                           within a selected state.  If the state is not selected, all US counties are presented"),
                                   tags$li("City: City level analysis, including one or more cities within a selected state or 
                                           county.  If no city is selected, all US cities are presented"),
                                   tags$li("Zip:  Zip code level analysis, including one or more zip codes, within a selected 
                                           state, and county or city.  If no state, county or city is selected, all markets at the 
                                           zip code level are presented.")
                                  ),# end of tags$ol
                                 tags$strong("Set Geographic Filter [2]"),
                                 p(
                                   paste("The Geographic Filter enables you to select the Level of Analysis, as well as 
                                         the specific geography to analyze. Once you select the level of analysis, state, 
                                         county, and city selectors appear which will allow you to further filter markets by a 
                                         geography that accords with the level of analysis.")),
                                 tags$strong("Set Home Value Range [3]"),
                                 tags$ul(
                                   tags$li("Use the Home Value Range slider to set the lower and upper bound on home values you wish to analyze."),
                                   tags$li("If you wish to include all homes with values over $2m, check the box labeled 'Include all values exceeding $2m'.")
                                   ),# end of tags$ul
                                 tags$strong("Set Growth Rate [4]"),
                                 p(
                                   paste("First, select the Time Horizon over which home value growth will be filtered.  Options are:")),
                                 tags$ul(
                                   tags$li("Monthly"),
                                   tags$li("Quarterly"),
                                   tags$li("Annual"),
                                   tags$li("Five Year"),
                                   tags$li("Ten Year")
                                 ),# end of tags$ul
                                 p(
                                   paste("Next, enter the minimum growth rate in percentage for the time horizon selected.")),
                                 tags$strong("Go!"),
                                 p(
                                   paste("Once you have made your selection, press 'Go' to process your query."))
                               )# end of box
                        ),#end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "queryBuilder.png", height = 600, width = 1020)
                               )# end of box
                        )# end of column
                ),# end of tabItem
                tabItem(tabName = "helpMarketReport",
                        fluidPage(
                          column(width = 4,
                                 box(
                                   title = "Market Explorer Report",
                                   width = 12,
                                   background = "green",
                                   solidHeader = FALSE,
                                   collapsible = FALSE,
                                   collapsed = FALSE,
                                   p(
                                     paste("This page provides several plots and a table that allows you to analyze and 
                                           compare markets by home values, home value growth and geography.  The page includes:")),
                                   tags$ul(
                                     tags$li("Value Growth by Value Scatterplot: This graphic illuminates the relationship between home values and home value growth.  [1]"),
                                     tags$li("Distribution of Median Home Values:  This illuminates the distribution of current home values according your query. [2]"),
                                     tags$li("Markets Table: This table lists the markets, their median home values, and home value growth rates, according to your query [3]"),
                                     tags$li("Top Markets By Growth: This bar chart shows the top markets by growth over the time horizon selected [4]"),
                                     tags$li("Median Home Values for Top Growth Markets:  This chart provides the historical price movements for the top markets by growth listed above [5]")
                                   )# end of tags$ul
                                 )# end of box
                          ),# end of column
                          column(width = 8,
                                 box(
                                   status = "primary",
                                   width = 12,
                                   solidHeader = FALSE,
                                   img(src = "marketExplorer1.png", height = 500, width = 1020),
                                   img(src = "marketExplorer2.png", height = 500, width = 1020)
                                 )# end of box
                          )# end of column
                    )# end of fluidpage
                ), #end of tabItem
                tabItem(tabName = "helpNonSeasonal",
                        column(width = 4,
                               box(
                                 title = "Value Analysis",
                                 width = 12,
                                 background = "green",
                                 solidHeader = FALSE,
                                 collapsible = FALSE,
                                 collapsed = FALSE,
                                 p(
                                   paste("The Value Analyzer allows you to analyze home value price movements for selected markets.  
                                         You will be able to analyze and decompose price movements into their seasonal and non-seasonal components. ")),
                                 h3("Explore Non-Seasonal Home Value Time Series"),
                                 tags$strong("Select a Market [1:3]"),
                                 p(
                                   paste("Whereas we queried multiple markets in the Market Explorer, here you will be selecting a specific market at either the state, 
                                         county, city or zip code level.  Once you have selected a market, press the 'Go' button to reveal some key statistics [3] and 
                                         the seasonal and non-seasonal time series.")),
                                 tags$strong("Non-Seasonal Home Value Time Series [4]"),
                                 p(
                                   paste("A non-seasonal time series consists of a trend component and an irregular component. 
                                         Decomposing the time series involves the separation of the time series into these components, 
                                         that is, estimating the trend component and the irregular component.  Here we show the trend 
                                         component by calculating the simple moving average (SMA) of the time series.")),
                                 tags$em("Span Order"),
                                 p(
                                   paste("To conducted a SMA, you need to specify the order (span) of the simple moving average.  
                                         Using the Span Order slider, select a span order between 1 and 10.  Through trial-and-error, 
                                         you will unveil a smooth SMA showing the trend component without excessive random fluctuation. [1]")),
                                 tags$strong("Simple Moving Average"),
                                 p(
                                   paste("The plot shows an estimate of home value trend with a simple moving average of median home values for the market, from 2000 thru 2015. 
                                         A simple moving average (SMA) is a simple, or arithmetic, moving average that is calculated by adding the 
                                         median home value of homes in the selected market for a number of time periods and then dividing this total 
                                         by the number of time periods. Short-term averages respond quickly to changes in the home values of the underlying, 
                                         while long-term averages are slow to react. [2]"))
                               )#end of box
                        ),#end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "valueAnalyzer1.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column
                      ),#end tabItem
                tabItem(tabName = "helpSeasonal",
                        column(width = 4,
                               box(
                                 title = "Value Analysis",
                                 width = 12,
                                 background = "green",
                                 solidHeader = FALSE,
                                 collapsible = FALSE,
                                 collapsed = FALSE,
                                 h3("Explore Seasonal Home Value Time Series [5]"),
                                 p(
                                   paste("A seasonal time series consists of a trend component, a seasonal component and an irregular component. 
                                         Decomposing the time series means separating the time series into these three components.  
                                         Click the tab labeled “Seasonal”.  This will reveal four charts:")),
                                 tags$ul(
                                   tags$li("Observed Trend: This is the original observed time series from the data."),
                                   tags$li("Trend Component: The Trend Component at time t, that reflects the long-term progression of the series (secular variation). 
                                           A trend exists when there is an increasing or decreasing direction in the data. "),
                                   tags$li("Seasonal Component: The Seasonal Component at time t, reflecting seasonality (seasonal variation). A seasonal pattern exists 
                                           when a time series is influenced by seasonal factors. Seasonality is always of a fixed and known period (e.g., the quarter of 
                                           the year, the month, or day of the week)."),
                                   tags$li("Random Component: The Random Component (or 'noise') at time t, that describes random, irregular influences. It represents the 
                                           residuals or remainder of the time series after the other components have been removed."),
                                   tags$li("Median Home Values for Top Growth Markets:  This chart provides the historical price movements for the top markets by growth listed above [5]")
                                   )# end of tags$ul
                                   )#end of box
                                   ),#end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "valueAnalyzer2.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column                
                ), #end of tabItem
                tabItem(tabName = "helpSetParameters",
                        column(width = 4,
                               box(
                                 title = "Forecast Modeler",
                                 width = 12,
                                 background = "green",
                                 solidHeader = FALSE,
                                 collapsible = FALSE,
                                 collapsed = FALSE,
                                 h3("Set Model Training Parameters"),
                                 tags$strong("Select a Market [1]"),
                                 p(
                                   paste("Select Market: Confirm or select a market using the selectors in the sidebar panel  [1]")),
                                 tags$strong("Cross Validation [2]"),
                                 p(
                                   paste("The home value time series data ranges from January 2000 thru December 2015.  
                                         To train the forecast algorithm, and to ascertain predictive accuracy, 
                                         the data must be split into a training set and a test set. The training 
                                         set will contain n series, starting at January 2000.  The test set will 
                                         start at series n +1 and continue through December 2015. Use the Training 
                                         Set Split slider to set the last year of the training set.  The default value is 2014. [2]")),
                                 tags$strong("Select Forecast Algorithm [3]"),
                                 p(
                                   paste("There are eight time series forecast algorithms available for modeling, and they are:")),
                                 tags$ul(
                                         tags$li("Arima -Autoregressive Integrated Moving Average"),
                                         tags$li("ETS – Automated Time Series Forecasting with Exponential Smoothing"),
                                         tags$li("Naïve Forecasting"),
                                         tags$li("Neural Networks – Artificial Neural Networks for Forecasting"),
                                         tags$li("BATS - Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components"),
                                         tags$li("TBATS - Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components"),
                                         tags$li("STLM - Seasonal-Trend Decomposition Procedure Based on Loess"),
                                         tags$li("STS - Basic Structural Model")
                                 ),# end of tags$ul
                                 p(
                                   paste("Use the selector to select the forecast algorithm, then press the 'Train Forecast Model'
                                         button to create the forecast model based upon the training set and 
                                         to calculate predictions on the test set. [4]"))
                               )# end of box
                        ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "trainParameters.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column
                ), #end of tabItem
                tabItem(tabName = "helpAnalyzeModels",
                        column(width = 4,
                               box(
                                 title = "Forecast Modeler",
                                 width = 12,
                                 background = "green",
                                 solidHeader = FALSE,
                                 collapsible = FALSE,
                                 collapsed = FALSE,
                                 h3("Analyze Models"),
                                 p(
                                   paste("The following exhibits include the model prediction and the prediction accuracy report.")),
                                 tags$strong("Model Prediction [5]"),
                                 p(
                                   paste("This plot includes the training component of the home value time series in black, 
                                         as well as a training prediction in red with the confidence interval shaded in light blue.  
                                         The dark blue line depicts actual test data.  [1]")),
                                 tags$strong("Prediction Accuracy [6]"),
                                 p(
                                   paste("The prediction accuracy report provides several indices for assessing prediction accuracy 
                                         on both the training and test sets, and they are:")),
                                 tags$ul(
                                   tags$li("ME: Mean Error"),
                                   tags$li("RMSE: Root Mean Squared Error"),
                                   tags$li("MAE: Mean Absolute Error"),
                                   tags$li("MPE: Mean Percentage Error"),
                                   tags$li("MAPE: Mean Absolute Percentage Error"),
                                   tags$li("MASE: Mean Absolute Scaled Error"),
                                   tags$li("ACF1: Autocorrelation of errors at lag 1.")
                                 ),# end of tags$ul
                                 p(
                                   paste("You may select a column to sort the data by the statistic selected.")),
                                 p(
                                   paste("To evaluate other training models, change the selection in the ‘Prediction Models’ 
                                         selector [3] and press the ‘Train Forecast Model’ button. [4]"))
                               )# end of box
                         ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "trainModel.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column
                ), #end of tabItem
                tabItem(tabName = "helpCompareModels",
                        fluidPage(
                          column(width = 4,
                                 box(
                                   title = "Forecast Modeler",
                                   width = 12,
                                   background = "green",
                                   solidHeader = FALSE,
                                   collapsible = FALSE,
                                   collapsed = FALSE,
                                   h3("Compare Models"),
                                   p(
                                     paste("The purpose of this page is to provide you with a convenient way to train all 
                                           eight models on a market at once and to evaluate and compare performance among and 
                                           between the various forecast algorithms side-by-side.")),
                                   tags$strong("Market Selector  [1,2]"),
                                   p(
                                     paste("As before confirm or select a market then press “Go” to run the training models.  
                                           Please be patient as we are running eight algorithms.  The progress bar in the upper 
                                           right corner reveals progress through the training process.")),
                                   tags$strong("Market Summary [3]"),
                                   p(
                                     paste("The value boxes indicate basic statistics on the market selected, such as median home 
                                           value and annual, five-year, and ten-year growth percentages.")),
                                   tags$strong("Model Performance Summary"),
                                   p(
                                     paste("Model Performance Error Metrics Chart:  Select a measurement and see the relative 
                                           performance of each of the models, for the selected metric. [4]")),
                                   p(
                                     paste("Model Performance Error Metrics Table: The prediction accuracy measures are calculated for each 
                                           forecast algorithm and are presented in a sortable datatable format.  To rank the forecast algorithms 
                                           by a specific error statistic, click on the caret next to the column heading for the error statistic. [5].")),
                                   p(
                                     paste("The rest of the page presents the prediction plots for each of the eight forecast models 
                                           in a 4 by 2 arrangement. [6:13]"))
                                 )# end of box
                          ),# end of column
                          column(width = 8,
                                 box(
                                   status = "primary",
                                   width = 12,
                                   solidHeader = FALSE,
                                   img(src = "modelCompare1.png", height = 574, width = 1020),
                                   img(src = "modelCompare2.png", height = 574, width = 1020),
                                   img(src = "modelCompare3.png", height = 574, width = 1020)
                                 )# end of box
                          )# end of column
                        )# end of fluidpage
                ), #end of tabItem
                tabItem(tabName = "helpMarketForecaster",
                        fluidPage(
                          column(width = 4,
                                 box(
                                   title = "Market Forecaster",
                                   width = 12,
                                   background = "green",
                                   solidHeader = FALSE,
                                   collapsible = FALSE,
                                   collapsed = FALSE,
                                   p(
                                     paste("This page enables you to create between 1 and 10 year home value forecasts for your selected market.  
                                           Forecasts produced by each of the eight forecast models are presented with confidence intervals 
                                           for comparative purposes.")),
                                   tags$strong("Set Forecast Options"),
                                   tags$ul(
                                     tags$li("Select a market using the selectors in the sidebar panel [1]"),
                                     tags$li("Indicate the number of years to forecast using the slider and press “Go” [2]")
                                   ),# end of tags$ul
                                   tags$strong("Review Forecast"),
                                   tags$ul(
                                     tags$li("Prediction Summary Valuebox:  The value boxes on the top of the page indicate 
                                             the minimum, maximum and mean predictions among the eight forecast algorithms. [3]"),
                                     tags$li("Forecast Summary Plot:  This chart shows the time series forecast for all eight 
                                             algorithms on a single chart [4]"),
                                     tags$li("Prediction Summary Plot:  This plot shows the forecasted home value at the end 
                                             of the forecast period for each of the forecast algorithms [5]"),
                                     tags$li("Model Forecasts:  The rest of the page reveals forecast plots for each of the forecast algorithms. [6:13]")
                                   )# end of tags$ul
                                )# end of box
                           ),# end of column
                          column(width = 8,
                                 box(
                                   status = "primary",
                                   width = 12,
                                   solidHeader = FALSE,
                                   img(src = "marketForecast1.png", height = 574, width = 1020),
                                   img(src = "marketForecast2.png", height = 574, width = 1020),
                                   img(src = "marketForecast3.png", height = 574, width = 1020)
                                 )# end of box
                          )# end of column          
                      )#end of fluidpage
                 ) #end of tabItem
          ) # end of tabITems
    )# end of dashboard body
)# end of dashboard page