# Coursera Data Science Specialization
# Building Data Products Course
# Housing Prices Time Series Forecasting
# John James
# Start Date: May 23, 2015

# server.R

################################################################################
#                               SHINYSERVER                                    #
################################################################################

shinyServer(function(input, output, session) {
  
  ################################################################################
  ##                        DASHBOARD SERVER FUNCTIONS                          ##
  ################################################################################
  #Render National Home Value Index Box
  output$usViBox <- renderValueBox({
    current <- currentState[ which(currentState$State == "United States"), ]
    valueBox(
      paste0("$", current$Value), paste(current$State, " Median Home Value "), 
      icon = icon("dollar"), color = "green"
    )
  })
  
  #Highest Home Value Index by City Box
  output$highestViBox <- renderValueBox({
    current <- currentCity[ which.max(currentCity$Value), ]
    valueBox(
      paste0("$", current$Value), paste("Highest Median Home Value in ", current$location), 
      icon = icon("money"), color = "blue"
    )
  })
  
  #Render Annual Price Growth  Box
  output$usAnnualBox <- renderValueBox({
    current <- currentState[ which(currentState$State == "United States"), ]
    valueBox(
      paste0(round(current$Annual * 100,4), "%"), paste(current$State,
      " Annual Change in Home Values"), icon = icon("bar-chart"), color = "red"
    )
  })
  
  #Render Highest Annual Price Growth  Box
  output$highestAnnualBox <- renderValueBox({
    current <- currentCity[ which.max(currentCity$Annual), ]
    valueBox(
      paste0(round(current$Annual * 100,4), "%"), paste("Highest Annual Change in Home Values in ", current$location), 
      icon = icon("line-chart"), color = "purple"
    )
  })
  
  #Render number of states box
  output$numStatesBox <- renderValueBox({
    valueBox(
      paste0(nrow(currentState)), paste("States"), 
      icon = icon("map-marker"), color = "green"
    )
  })

  #Render number of counties box
  output$numCountiesBox <- renderValueBox({
    valueBox(
      paste0(nrow(currentCounty)), paste("Counties"), 
      icon = icon("map"), color = "yellow"
    )
  })

  #Render number of cities box
  output$numCitiesBox <- renderValueBox({
    valueBox(
      paste0(nrow(currentCity)), paste("Cities"), 
      icon = icon("map-pin"), color = "red"
    )
  })
  
  #Render number of cities box
  output$numZipsBox <- renderValueBox({
    valueBox(
      paste0(nrow(currentZip)), paste("Zipcodes"), 
      icon = icon("map-o"), color = "navy"
    )
  })
  
  #Render Top 10 States bar chart
  output$top10StatesBar <- renderChart({
    current <- currentState[ which(currentState$State != "United States"), ]
    current <- arrange(current, desc(Annual))
    current$Annual <- round(current$Annual * 100,2)
    current <- subset(current[1:10,], select = c(location, Annual))
    p <- nPlot(Annual~location, data = current, type = "discreteBarChart", dom = "top10StatesBar", height = 180, width = 750)
    p$xAxis(staggerLabels = TRUE)
    p$yAxis(axisLabel = "Annual Growth (%)", width = 50)
    return(p)
  })
  
  #Render Top 10 Cities bar chart
  output$top10CitiesBar <- renderChart({
    current <- currentCity
    current <- arrange(current, desc(Annual))
    current$Annual <- round(current$Annual * 100,2)
    current <- subset(current[1:10,], select = c(location, Annual))
    p <- nPlot(Annual~location, data = current, type = "discreteBarChart", dom = "top10CitiesBar", height = 180, width = 750)
    p$xAxis(staggerLabels = TRUE)
    p$yAxis(axisLabel = "Annual Growth (%)", width = 50)
    return(p)
  })
  
  #Render Top 10 States by Home Value Growth TimeSeries
  output$top10StatesTS <- renderPlot({
    current <- currentState[ which(currentState$State != "United States"), ]
    current <- arrange(current, desc(Annual))
    current <- data.frame(current[1:10,3])
    colnames(current) <- "State"
    stateDF <- hviAllState
    stateDF <- merge(current, stateDF, by = "State")
    stateDF <- subset(stateDF, select = c(State, X2000.01:X2015.12))
    stateDF <- t(stateDF)
    colnames(stateDF) <- stateDF[1,]
    stateDF <- stateDF[-1,]
    stateTS <- ts(stateDF, start = c(2000,1), end = c(2015,12), frequency = 12)
    plot(stateTS, plot.type = "single", col = 1:ncol(stateTS), ylab = "Median Home Value")
    legend("topleft", colnames(stateTS), col = 1:ncol(stateTS), lty = 1)
  })
  
  #Render Top 10 Cities by Home Value Growth TimeSeries
  output$top10CitiesTS <- renderPlot({
    current <- currentCity
    current <- arrange(current, desc(Annual))
    current <- data.frame(current[1:10,])
    current <- subset(current, select = location)
    cityDF <-  hviAllCity
    cityDF$location <- paste0(cityDF$City, ", ", cityDF$State)
    cityDF <- merge(current, cityDF, by = "location")
    cityDF <- subset(cityDF, select = c(location, X2000.01:X2015.12))
    cityDF <- t(cityDF)
    colnames(cityDF) <- cityDF[1,]
    cityDF <- cityDF[-1,]
    cityTS <- ts(cityDF, start = c(2000,1), end = c(2015,12), frequency = 12)
    plot(cityTS, plot.type = "single", col = 1:ncol(cityTS), ylab = "Median Home Value")
    legend("topleft", colnames(cityTS), col = 1:ncol(cityTS), lty = 1)
  })
  
  
  
  ################################################################################
  ##                        MARKET EXPLORER FUNCTIONS                           ##
  ################################################################################
  #Level of Analysis UI
  output$levelQueryUi <- renderUI({
    radioButtons("analysisLevel", label = "Level of Analysis",
                 choices = list("State" = 1, "County" = 2, "City" = 3, "Zip"  = 4), 
                 selected = 2)
  })
  
  # State query UI
  output$stateQuery2Ui <- renderUI({
    states <- sort(unique(geo$StateName))
    selectInput("state2", label = "State:", choices = c(Choose='', as.character(states)), selected = dflt$state, selectize = FALSE)
  })

  # State query UI
  output$stateQuery3Ui <- renderUI({
    states <- sort(unique(geo$StateName))
    selectInput("state3", label = "State:", choices = c(Choose='', as.character(states)), selected = dflt$state, selectize = FALSE)
  })

  # State query UI
  output$stateQuery4Ui <- renderUI({
    states <- sort(unique(geo$StateName))
    selectInput("state4", label = "State:", choices = c(Choose='', as.character(states)), selected = dflt$state, selectize = FALSE)
  })

  # County Query UI  
  output$countyQuery3Ui <- renderUI({
    if (!is.null(input$state3)) {
      if (input$state3 != "") {
        state <- input$state3
      } else {
        state <- dflt$state  
      }
    } else {
      state <- dflt$state
    }
    counties <- arrange(unique(subset(geo, StateName == state, select = County)), County)
    selectInput("county3", label = "County:", choices = c(Choose='', as.character(counties$County)), selected = dflt$county, selectize = FALSE)
  })

  # County Query UI  
  output$countyQuery4Ui <- renderUI({
    if (!is.null(input$state4)) {
      if (input$state4 != "") {
        state <- input$state4
      } else {
        state <- dflt$state  
      }
    } else {
      state <- dflt$state
    }
    counties <- arrange(unique(subset(geo, StateName == state, select = County)), County)
    selectInput("county4", label = "County:", choices = c(Choose='', as.character(counties$County)), selected = dflt$county, selectize = FALSE)
  })
  
  output$cityQuery4Ui <- renderUI({
    cities <- NULL
    
    if (!is.null(input$state4)) {
      if (input$state4 != "") {
        if (!is.null(input$county4)) {
          if (input$county4 != "") {
            cities  <- arrange(unique(subset(geo, StateName == input$state4 & County == input$county4, select = City)), City)
          } else {
            cities  <- arrange(unique(subset(geo, StateName == input$state4, select = City)), City)
          }
        } else {
          cities  <- arrange(unique(subset(geo, StateName == input$state4, select = City)), City)
        }
      } 
    }  
    selectInput("city4", label = "City:", choices = c(Choose='', as.character(cities$City)), selected = dflt$city, selectize = FALSE)
  })
  
  
  # Get and screen data based upon home value and growth rates
  screenData <- function() {
    # Get Deta
    d <- switch(input$analysisLevel,
                  "1" = currentState,
                  "2" = currentCounty,
                  "3" = currentCity,
                  "4" = currentZip
      )

    # Screen based upon home value index
    minValue <- as.numeric(input$hviQuery[1]) * 1000
    if (input$maxValue == TRUE) {
      maxValue <- dfltMaxValue
    } else {
      maxValue <- as.numeric(input$hviQuery[2]) * 1000
    }
    d <- subset(d, Value >= minValue & Value <= maxValue)

    # Screen based upon growth variable
    minGrowth <- as.numeric(input$minGrowth) / 100
    
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    d <- switch(horizon,
           Monthly = d[ which(d$Monthly >= minGrowth),],
           Quarterly = d[ which(d$Quarterly >= minGrowth),],
           Annual = d[ which(d$Annual >= minGrowth),], 
           Five = d[ which(d$Five_Year >= minGrowth),],
           Ten = d[ which(d$Ten_Year >= minGrowth),])
    
     return(d)
  }
  

  # Get Data for State level of analysis
  getStateData <- function() {
    
    # Get data screened by value and growth rates
    d <- screenData()
    
    # Format growth record
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    d <- switch(horizon,
                Monthly   = select(d, State, Value, Monthly, location),
                Quarterly = select(d, State, Value, Quarterly, location),
                Annual    = select(d, State, Value, Annual, location),
                Five      = select(d, State, Value, Five_Year, location),
                Ten       = select(d, State, Value, Ten_Year, location))

    return(d)
                
  }
  
  
  # Get Data for County level of analysis
  getCountyData <- function() {
    # Get data screened by value and growth rates
    d <- screenData()
    
    # Filter based upon state 
    if (!is.null(input$state2) & (input$state2 != "")) {
      d <- d[ which(d$StateName == input$state2),]
    } 
    
    # Format growth record
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    d <- switch(horizon,
                Monthly   = select(d, StateName, County, Value, Monthly, location),
                Quarterly = select(d, StateName, County, Value, Quarterly, location),
                Annual    = select(d, StateName, County, Value, Annual, location),
                Five      = select(d, StateName, County, Value, Five_Year, location),
                Ten       = select(d, StateName, County, Value, Ten_Year))    
    return(d)
    
  }
  
  
  
  # Get Data for City level of analysis
  getCityData <- function() {
    
    # Get data screened by value and growth rates
    d <- screenData()
    
    # Filter based upon state and county entered
    if (!is.null(input$county3) & (input$county3 != "")) {
      d <- d[ which(d$County == input$county3),]
    } else if (!is.null(input$state3) & (input$state3 != "")) {
      d <- d[ which(d$StateName == input$state3),]
    } 
    
    # Format growth record
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    d <- switch(horizon,
                Monthly = select(d, StateName, County, City, Value, Monthly, location),
                Quarterly = select(d, StateName, County, City, Value, Quarterly, location),
                Annual = select(d, StateName, County, City, Value, Annual, location),
                Five = select(d, StateName, County, City, Value, Five_Year, location),
                Ten = select(d, StateName, County, City, Value, Ten_Year, location))       
    return(d)
    
  }
  
  
  
  # Get Data for Zip level of analysis
  getZipData <- function() {
    # Get data screened by value and growth rates
    d <- screenData()

    # Filter based upon state
    if (!is.null(input$state4)) {
      if (input$state4 != "") {
        d <- d[ which(d$StateName == input$state4),]
      }
    }
        
    # Filter based upon county
    if (!is.null(input$county4)) {
      if (input$county4 != "") {
        d <- d[ which(d$County == input$county4 & d$StateName == input$state4),]
      }
    }
    # Filter based upon state and county entered
    if (!is.null(input$city4)) {
      if (input$city4 != "") {
        d <- d[ which(d$City == input$city4 & d$StateName == input$state4),]
      }
    }
    
    # Format growth record
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    d <- switch(horizon,
                Monthly = select(d, StateName, County, City, Zip, Value, Monthly, location),
                Quarterly = select(d, StateName, County, City, Zip, Value, Quarterly, location),
                Annual = select(d, StateName, County, City, Zip, Value, Annual, location),
                Five = select(d, StateName, County, City, Zip, Value, Five_Year, location),
                Ten = select(d, StateName, County, City, Zip, Value, Ten_Year, location))           
    
    return(d)
    
  }
  
  
  
  # Retrieves Data for Value Presentation
  getData <- eventReactive(input$query, {
    
    d <- switch(input$analysisLevel,
                "1" = getStateData(),
                "2" = getCountyData(),
                "3" = getCityData(),
                "4" = getZipData()
                )

    validate(
      need(nrow(d)>0, "No markets meet your search criteria.  Please select adjust home value range, minimum growth rate, and/or the geographic filter.")
    )
    
      return(d)
  }, ignoreNULL = FALSE)


  # Get Growth Data
  getGrowthData <- eventReactive(input$query, {
    
    d <- getData()
    
    # Configure Chart based upon input horizon
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    
    # Sort by horizon
    if (nrow(d) != 0) {
      d <- switch(horizon,
                  Monthly = arrange(d, desc(Monthly)),
                  Quarterly = arrange(d, desc(Quarterly)),
                  Annual = arrange(d, desc(Annual)),
                  Five = arrange(d, desc(Five_Year)),
                  Ten = arrange(d, desc(Ten_Year)))
    } 
    
    
  }, ignoreNULL = FALSE)
  
  
  # Merge current and historical data for number of markets requested.
  mergeMarketData <- function(d,n) {

    #if state level of analysis, remove row for USA from data frame.
    if (input$analysisLevel == 1) {
      d <- d[ which(d$State != "United States"), ]
    }
    
    #Subset current data to top n markets by growth
    if (!is.null(d)) {
      numMarkets <- n
      if (nrow(d) < numMarkets) {
        numMarkets <- nrow(d)
      }
      d <- d[1:numMarkets,]
    }
    
    #Get historical pricing data based upon level of analysis
    h <- switch (isolate(input$analysisLevel),
                 "1" = hviAllState,
                 "2" = hviAllCounty,
                 "3" = hviAllCity,
                 "4" = hviAllZip
    )
    
    m <- merge(d, h, by = "location")

    return(m)
  }

  output$valueByGrowth <- renderPlotly({
    # Get Data
    d <- getGrowthData()
    
    # Subset into top results
    if (!is.null(d)) {
      numBars <- 1000
      if (nrow(d) < numBars) {
        numBars <- nrow(d)
      }
      d <- d[1:numBars,]
    }
    
    # Prepare data based upon input horizon
    horizon <- isolate(input$horizon)
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    
    d <- switch(horizon,
                Monthly = subset(d, select = c(location, Value, Monthly)),
                Quarterly = subset(d, select = c(location, Value, Quarterly)),  
                Annual = subset(d, select = c(location, Value, Annual)),            
                Five = subset(d, select = c(location, Value, Five_Year)),            
                Ten = subset(d, select = c(location, Value, Ten_Year))
    )
    colnames(d) <- c("Location", "Value", "Growth")
    
    # Convert Growth Rate to Percentage
    d$Growth = as.numeric(d$Growth) * 100
    
    # Designate axis labels
    x <- list(title = "Median Home Value")
    y <- list(title = "Percent Value Growth")
    
    # Prepare plot
    p <- plot_ly(d, x = Value, y = Growth, text = Location, mode = "markers", color = Value, size = Value) %>%
      layout(xaxis = x, yaxis = y)
  })
  
  #Render Top Markets by Growth
  output$topByGrowth <- renderChart({
    
    # Get Data
    d <- getGrowthData()
    
    # Subset into top results
    if (!is.null(d)) {
      numBars <- 10
      if (nrow(d) < numBars) {
        numBars <- nrow(d)
      }
      d <- d[1:numBars,]
    }
    
    # Prepare data based upon input horizon
    horizon <- isolate(input$horizon)
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }

    d <- switch(horizon,
                Monthly = subset(d, select = c(location, Monthly)),
                Quarterly = subset(d, select = c(location, Quarterly)),            
                Annual = subset(d, select = c(location, Annual)),            
                Five = subset(d, select = c(location, Five_Year)),            
                Ten = subset(d, select = c(location, Ten_Year))
    )
    colnames(d) <- c("location", "Growth")
    d$Growth <- as.numeric(d$Growth) * 100
    
    #Prepare plot
    p <- nPlot(Growth~location, data = d)
    p$addParams(dom = "topByGrowth", type = "discreteBarChart")
    p$set(width = 1200, height = 300)
    p$xAxis(staggerLabels = TRUE)
    p$yAxis(axisLabel = "Growth Rate (%)", width = 50)
    return(p)
  })
  

  # Render Market Data Table
  output$marketTbl <- renderDataTable({
    
    d <- getGrowthData()
    
    #Drop location variable
    d$location <- NULL 
    return(d)
  }, options = list(lengthMenu = c(5, 30, 50), autowidth = TRUE, pageLength = 5))
  

  output$valueHist <- renderPlot({
    # Get Data
    d <- getData()

    # Render error message if not enough data to produce histogram
    validate(
      need(nrow(d) > 1, "Not enough data to produce histogram.")
    )
    
    # Subset Data
    d <- subset(d, select = c("location", "Value"))
    
    # Convert Value to ($000)
    d$Value <- as.numeric(d$Value) / 1000
    
    #Set Parameters
    bins <- seq(min(d$Value), max(d$Value), length.out = 31)
    
    #Draw Histogram
    hist(as.numeric(d$Value), breaks = bins, col = "skyblue", border = "white",
         xlab = "Median Home Values ($000)", 
         main = "Histogram of Median Home Values")
  })
  

  #Render Top Markets by Home Value Growth TimeSeries
  output$topMarketsTS <- renderPlot({
    # Get Data
    d <- getGrowthData()
    
    # Set number of markets to plot.  Markets are sorted by Growth (desc)
    numMarkets <- 10
    
    # Merge current with historical data for number of markets
    d <- mergeMarketData(d, numMarkets)

    # Format for time series 
    d <- subset(d, select = c(location, X2000.01:X2015.12))
    d <- t(d)
    colnames(d) <- d[1,]
    d <- d[-1,]
    dTS <- ts(d, start = c(2000,1), end = c(2015,12), frequency = 12)
    
    # Render plot
    plot(dTS, plot.type = "single", col = 1:ncol(dTS), ylab = "Median Home Value")
    legend("topleft", colnames(dTS), col = 1:ncol(dTS), lty = 1)
  })
  
  ################################################################################
  ##                        MARKET SELECTOR FUNCTIONS                           ##
  ################################################################################
  
  # State query UI
  output$stateUi <- renderUI({
    states <- sort(unique(geo$StateName))
    selectInput("state", label = "State:", choices = c(Choose='', as.character(states)), selected = dflt$state, selectize = FALSE)
  })
  
  # County Query UI  
  output$countyUi <- renderUI({
    counties <- NULL
    if (!is.null(input$state)) {
      counties <- unique(subset(geo, StateName == input$state, select = County))
      counties <- sort(counties$County)
    }
    selectInput("county", label = "County:", choices = c(Choose='', as.character(counties)), selected = dflt$county, selectize = FALSE)
  })
  
  
  output$cityUi <- renderUI({
    cities <- NULL
    
    if (!is.null(input$state)) {
      if (input$state != "") {
        if (!is.null(input$county)) {
          if (input$county != "") {
            cities  <- unique(subset(geo, StateName == input$state & County == input$county, select = City))
          } else {
            cities  <- unique(subset(geo, StateName == input$state, select = City))
          }
        } else {
          cities  <- unique(subset(geo, StateName == input$state, select = City))
        }
      } 
    }
    
    cities <- sort(cities$City)
    selectInput("city", label = "City:", choices = c(Choose='', as.character(cities)), selected = dflt$city, selectize = FALSE)
  })
  
  
  output$zipUi <- renderUI({
    zips <- NULL
    
    if (!is.null(input$state)) {
      if (input$state != "") {
        zips <- unique(subset(geo, StateName == input$state))
      }
    }
    
    if (!is.null(input$county)) {
      if (input$county != "") {
        zips <- unique(subset(zips, StateName == input$state & County == input$county))
      }
    }
    
    if (!is.null(input$city)) {
      if (input$city != "") {
        zips <- unique(subset(zips, StateName == input$state & City == input$city))
      }
    }
    
    zips <- sort(zips$Zip)
    
    selectInput("zip", label = "Zip:", choices = c(Choose='', as.character(zips)), selected = dflt$zip, selectize = FALSE)
    
  })
  
  #Determine level of market selected.
  getLevel <- reactive({
    
    #Initialize Level
    level <- "0"
    
    # Get State Data 
    if (!is.null(input$state)) {
      if (input$state != "") {
        level <- "1"
      }
    }
    
    if (!is.null(input$county)) {
      if (input$county != "") {
        level <- "2"
      }
    } 
    
    if (!is.null(input$city)) {
      if (input$city != "") {
        level <- "3"
      }
    } 
    
    if (!is.null(input$zip)) {
      if (input$zip != "") {
        level <- "4"
      }
    } 
    
    return(level)
  })
  
  # Get Current Data
  selectCurrentData <- reactive({
    level <- getLevel()
    
    d <- switch(level,
                "1" = subset(currentState, State == input$state),
                "2" = subset(currentCounty, StateName == input$state & County == input$county),
                "3" = subset(currentCity, StateName == input$state & City == input$city),
                "4" = subset(currentZip, Zip == input$zip)
                )
  })

  # Get Historical Data
  selectHistoricalData <- reactive({
    level <- getLevel()

    d <- switch(level,
                "1" = subset(hviAllState, State == input$state, select = X2000.01:X2015.12),
                "2" = subset(hviAllCounty, StateName == input$state & County == input$county, select = X2000.01:X2015.12),
                "3" = subset(hviAllCity, StateName == input$state & City == input$city, select = X2000.01:X2015.12),
                "4" = subset(hviAllZip, Zip == input$zip, select = X2000.01:X2016.01)
                )
    
    validate(
      need(!is.null(d), "There are no data to analyze. Please select a market and press 'Go' to process the analysis.")
    )
    
    validate(
      need(!any(is.na(d)), "Unable to produce a timeseries with NA values. Please select a different market in the sidebar. ")
    )
    
    return(d)
  })

  ################################################################################
  ##                        VALUE ANALYSIS FUNCTIONS                            ##
  ################################################################################
  
  #Render Home Value Index Box for selected market
  output$hviBox <- renderValueBox({
    
    input$analyze
    
    d <- isolate(selectCurrentData())
    
    validate(
      need(!is.null(d),"")
    )
    
    valueBox(
      paste0("$", d$Value), paste(d$location, " Median Home Value "), 
      icon = icon("dollar"), color = "green"
    )
  })
  
  #Render Five Year Growth Box for selected market
  output$annualBox <- renderValueBox({
    
    input$analyze
    
    d <- isolate(selectCurrentData())    
    
    validate(
      need(!is.null(d), "")
    )
    
    valueBox(
      paste0(round(d$Annual * 100,4), "%"), paste(d$location,
        " Annual Change in Home Values"), icon = icon("bar-chart"), color = "red"    )
  })
  
  #Render Annual Growth Box for selected market
  output$fiveYearBox <- renderValueBox({

    input$analyze
    
    d <- isolate(selectCurrentData())
    
    validate(
      need(!is.null(d), "")
    )
    valueBox(
      paste0(round(d$Five_Year * 100,4), "%"), paste(d$location,
      " Five Year Change in Home Values"), icon = icon("bar-chart"), color = "orange"    )
  })
  
  #Render Annual Growth Box for selected market
  output$tenYearBox <- renderValueBox({
    
    input$analyze
    
    d <- isolate(selectCurrentData())
    
    validate(
      need(!is.null(d), "")
    )
    
    valueBox(
      paste0(round(d$Ten_Year * 100,4), "%"), paste(d$location,
      " Ten Year Change in Home Values"), icon = icon("bar-chart"), color = "blue"    )
  })
  
  #Gets time series for a selected market.
  getTimeSeries <- eventReactive(input$marketSelect, {
    d <- selectHistoricalData()
    
    d <- as.numeric(as.vector(d))
    timeSeries <- ts(d, frequency = 12, start = c(2000,1))
    return(timeSeries)

  }, ignoreNULL = FALSE)
  
  
  
  # Render non-seasonal trend time series
  output$nsPlot <- renderPlot({
    Price <- SMA(getTimeSeries(), n = input$span)
    autoplot(Price, ts.colour = "blue") + theme_bw()
  })
  
  # Render seasonal time series decomposition
  output$tsiPlot <- renderPlot({
    Price <- decompose(getTimeSeries())
    autoplot(Price, ts.colour = "blue") + theme_bw()
  })
  
  ################################################################################
  ##              FORECAST MODEL TRAINING FUNCTIONS                             ##
  ################################################################################

  # Render model select
  output$modelsUi <- renderUI({
    selectInput("model", label = "Prediction Models:", choices = c(Choose='',as.character(modelData$code)), selected = dflt$model, selectize = FALSE)
  })
  
  # Render model name
  output$modelNameUi <- renderText({
    if (is.null(input$model)) {
      m <- dflt$model
    } else {
      m <- input$model
    }
    paste(modelData[ which(modelData$code == m), ]$name)
  })
  
  # Render model description
  output$modelDescUi <- renderText({
    if (is.null(input$model)) {
      m <- dflt$model
    } else {
      m <- input$model
    }
    paste(modelData[ which(modelData$code == m), ]$desc)  
  })
  
  
  # Split data into training and test/validation set
  splitData <- function() {
    
    if (is.null(input$split)) {
      y <- dflt$split
    } else {
      y <- as.numeric(input$split)
    }
    
    validate(
      need(input$state != "", "Please select a market using the geographic selectors in the sidebar. ")
    )
    
    d <- selectHistoricalData()

    # Create time series object on full data
    marketPrices  <- as.numeric(as.vector(d))
    tSeries       <- ts(marketPrices, frequency = 12, start = c(2000,1))
    
    #Split into training and test set
    tsTest  <- window(tSeries, start = c(y+1,1))
    tsTrain <- window(tSeries, end = c(y,12))
    
    #Combine into a list
    l <- list("train" = tsTrain, "test" = tsTest)
    return(l)
  }
  

  # Get plot options, specifically, number of periods to forecast and to include
  getForecastOptions <- function() {
 
    # Determine number of periods to forecast
    if (is.null(input$split) || input$split == "") {
      periods <- 12
    } else {
      periods <- (2015 - as.integer(input$split)) * 12
    }
    
    # Determine number of back periods to include
    if ((periods * 3) > (192 - periods)) {
      include <- 192 - periods
    } else {
      include <- periods * 3
    }
    
    # Determine ylimit at peak price
    maximum <- as.integer(max(selectHistoricalData()))
    
    #Combine into a list and return
    l <- list(periods = periods, include = include, maximum = maximum)
    
  }
  
  
  # Prepare predictions
  trainModel <- function(model, data) {
    m <- model()
    d <- data()
    
    validate(
      need(!any(is.na(d)), "There are NA values in the training set for this market. Please change your selection criteria in Market Selector")
    )
    
    
    if (!is.null(d)) {
      switch (m,
              ARIMA = auto.arima(d, ic='aicc', stepwise=FALSE),
              ETS = ets(d, ic='aicc', restrict=FALSE),
              NEURAL = nnetar(d, p=12, size=25),
              TBATS = tbats(d, ic='aicc', seasonal.periods=12),
              BATS = bats(d, ic='aicc', seasonal.periods=12),
              STLM = stlm(d, s.window=12, ic='aicc', robust=TRUE, method='ets'),
              STS = StructTS(d, type = "level"),
              NAIVE = naive(d, getForecastOptions()$periods)
      )
    }
  }
  
  
  #Format Accuracy Results into a table
  formatAccuracy <- function(r) {
    
    if (!is.null(r)) {
      measure    <-c("Mean Error (ME)",
                     "Root Mean Squared Error (RMSE)",
                     "Mean Absolute Error (MAE)",
                     "Mean Percentage Error (MPE)",
                     "Mean Absolute Percentage Error (MAPE)",
                     "Mean Absolute Scaled Error (MASE)",
                     "Autocorrelation of errors at lag 1. (ACF1)",
                     "ThEIl's U")
      trainingSet <- round(r[,1], 3)
      testSet     <- round(r[,2], 3)
      results     <- data.frame(measure, trainingSet, testSet)
      names(results) <- c("Measure", "Training Set", "Test Set")
      return(results)
    }
  }
  
  # Get training forecast and test data for single plot on train page
  getSinglePlotData <- eventReactive(input$train, {
    
    d <- splitData()
    m <- trainModel(reactive(input$model), reactive(d$train))
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    r <- t(a)
    ma <- formatAccuracy(r)
    
    #Combine into a list
    l <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r, "accuracy" = ma)
    
    #Return list
    return(l)
  }, ignoreNULL = FALSE)
  
  
  # Render training forecast plot
  output$modelPlot <- renderPlot({
    
    p <- getSinglePlotData()
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })
  
  output$accuracy <- renderDataTable({
    a <- getSinglePlotData()
    a$accuracy
  })
  
  ################################################################################
  ##                        MODEL COMPARISON FUNCTIONS                          ##
  ################################################################################
  #Render Home Value Index Box for selected market
  output$hviBox2 <- renderValueBox({
    
    input$compare
    
    d <- isolate(selectCurrentData())
    
    validate(
      need(!is.null(d),"")
    )
    
    valueBox(
      paste0("$", d$Value), paste(d$location, " Median Home Value "), 
      icon = icon("dollar"), color = "green"
    )
  })
  
  #Render Five Year Growth Box for selected market
  output$annualBox2 <- renderValueBox({
    
    input$compare
    
    d <- isolate(selectCurrentData())
    
    validate(
      need(!is.null(d), "")
    )
    
    valueBox(
      paste0(round(d$Annual * 100,4), "%"), paste(d$location,
                                                  " Annual Change in Home Values"), icon = icon("bar-chart"), color = "red"    )
  })
  
  #Render Annual Growth Box for selected market
  output$fiveYearBox2 <- renderValueBox({
    
    input$compare
    
    d <- isolate(selectCurrentData())
    
    validate(
      need(!is.null(d), "")
    )
    valueBox(
      paste0(round(d$Five_Year * 100,4), "%"), paste(d$location,
                                                     " Five Year Change in Home Values"), icon = icon("bar-chart"), color = "orange"    )
  })
  
  #Render Annual Growth Box for selected market
  output$tenYearBox2 <- renderValueBox({

    input$compare
    
    d <- isolate(selectCurrentData())
    
    validate(
      need(!is.null(d), "")
    )
    
    valueBox(
      paste0(round(d$Ten_Year * 100,4), "%"), paste(d$location,
                                                  " Ten Year Change in Home Values"), icon = icon("bar-chart"), color = "blue"    )
  })


  # Get Arima Plot Data
  getArimaPlotData <- eventReactive(input$compare, {

    d <- splitData()
    m <- auto.arima(d$train, ic='aicc', stepwise=FALSE)
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    accuracyArima <- t(a)
    
    #Combine into a list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = accuracyArima)
    
    #Return list
    return(p)
  }, ignoreNULL = FALSE)
  

  # Get ETS Plot Data
  getEtsPlotData <- eventReactive(input$compare, {
    
    d <- splitData()
    m <- ets(d$train, ic='aicc', restrict=FALSE)
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    accuracyEts <- t(a)
    
    #Combine into a list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = accuracyEts)
    
    #Return list
    return(p)
  }, ignoreNULL = FALSE)
  

  # Get Naive Plot Data
  getNaivePlotData <- eventReactive(input$compare, {
    
    d <- splitData()
    m <- naive(d$train, getForecastOptions()$periods)
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    accuracyNaive <- t(a)
    
    #Combine into a list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = accuracyNaive)
    
    #Return list
    return(p)
  }, ignoreNULL = FALSE)
  
  
  # Get Neural Plot Data
  getNeuralPlotData <- eventReactive(input$compare, {
    
    d <- splitData()
    m <- nnetar(d$train, p=12, size=25)
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    accuracyNeural <- t(a)
    
    #Combine into a list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = accuracyNeural)
    
    #Return list
    return(p)
  }, ignoreNULL = FALSE)
  
  
  # Get BATS Plot Data
  getBATSPlotData <- eventReactive(input$compare, {
    
    d <- splitData()
    m <- bats(d$train, ic='aicc', seasonal.periods=12)
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    accuracyBats <- t(a)
    
    #Combine into a list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = accuracyBats)
    
    #Return list
    return(p)
  }, ignoreNULL = FALSE)
  
  
  # Get TBATS Plot Data
  getTBATSPlotData <- eventReactive(input$compare, {
    
    d <- splitData()
    m <- tbats(d$train, ic='aicc', seasonal.periods=12)
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    accuracyTbats <- t(a)
    
    #Combine into a list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = accuracyTbats)
    
    #Return list
    return(p)
  }, ignoreNULL = FALSE)
  
  # Get STLM Plot Data
  getSTLMPlotData <- eventReactive(input$compare, {
    
    d <- splitData()
    m <- stlm(d$train, s.window=12, ic='aicc', robust=TRUE, method='ets')
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    accuracyStlm <- t(a)
    
    #Combine into a list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = accuracyStlm)
    
    #Return list
    return(p)
  }, ignoreNULL = FALSE)
  
  # Get STS Plot Data
  getSTSPlotData <- eventReactive(input$compare, {
    
    d <- splitData()
    m <- StructTS(d$train, type = "level")
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    accuracySts <- t(a)
  
    #Combine into a list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = accuracySts)    
    
    #Return list
    return(p)
  }, ignoreNULL = FALSE)
  
  #Summarize performance error statistics 
  errorStats <- eventReactive(input$compare, {
    
    accuracyTbl <- NULL
    
    validate(
      need(input$state != "", "Please select a market using the geographic selectors in the sidebar. ")
    )
    
    accuracyArima  <-	isolate(getArimaPlotData())
    accuracyEts	   <-	isolate(getEtsPlotData())
    accuracyNaive	 <-	isolate(getNaivePlotData())
    accuracyNeural <-	isolate(getNeuralPlotData())
    accuracyBats	 <-	isolate(getBATSPlotData())
    accuracyTbats	 <-	isolate(getTBATSPlotData())
    accuracyStlm	 <-	isolate(getSTLMPlotData())
    accuracySts	   <-	isolate(getSTSPlotData())
    
    `%then%` <- shiny:::`%OR%`
    
    validate(
      need(accuracyArima  != "", "Unable to prepare performance metrics with this data.  Please select a different market in the sidebar.")  %then%
      need(accuracyEts  != "", "Unable to prepare performance metrics with this data.  Please select a different market in the sidebar.")  %then%
      need(accuracyNaive  != "", "Unable to prepare performance metrics with this data.  Please select a different market in the sidebar.")  %then%
      need(accuracyNeural  != "", "Unable to prepare performance metrics with this data.  Please select a different market in the sidebar.")  %then%
      need(accuracyBats  != "", "Unable to prepare performance metrics with this data.  Please select a different market in the sidebar.")  %then%
      need(accuracyTbats  != "", "Unable to prepare performance metrics with this data.  Please select a different market in the sidebar.")  %then%
      need(accuracyStlm  != "", "Unable to prepare performance metrics with this data.  Please select a different market in the sidebar.")  %then%
      need(accuracySts  != "", "Unable to prepare performance metrics with this data.  Please select a different market in the sidebar.")  %then%
      need(accuracySts  != "", "Unable to prepare performance metrics with this data.  Please select a different market in the sidebar.")
    )
    
    accuracyTbl <-  as.data.frame(rbind(round(accuracyArima$results[,2],3),
                                         round(accuracyEts$results[,2],3),
                                         round(accuracyNaive$results[,2],3),
                                         round(accuracyNeural$results[,2],3),
                                         round(accuracyBats$results[,2],3),
                                         round(accuracyTbats$results[,2],3),
                                         round(accuracyStlm$results[,2],3),
                                         round(accuracySts$results[,2],3)))

    modelNames <- as.vector(c("Arima", "ETS", "Naive", "Neural", "BATS", "TBATS", "STLM", "STS"))
    accuracyTbl <- data.frame(modelNames, accuracyTbl)
    colnames(accuracyTbl) <- c("Model", "ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1", "THEILS")
    accuracyTbl
    
  }, ignoreNULL = FALSE)
  
  
  
  #Plot Arima model prediction 
  output$arima <- renderPlot({
    
    withProgress(message = "Preparing Arima Forecast", {
    
      p <- getArimaPlotData()
      plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
      lines(p$test, col = "red")
      legend("bottomright",
             inset=.05,
             cex = 1,
             title="Legend",
             c("Actual Values","Predicted Values"),
             horiz=FALSE,
             lty=c(1,1),
             lwd=c(2,2),
             col=c("red","blue"),
             bg="white",
             text.font=3)
    })
  })
  
  #Plot ETS forecast
  output$ets <- renderPlot({
    
    withProgress(message = "Preparing ETS Forecast", {
      
      p <- getEtsPlotData()
      plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
      lines(p$test, col = "red")
      legend("bottomright",
             inset=.05,
             cex = 1,
             title="Legend",
             c("Actual Values","Predicted Values"),
             horiz=FALSE,
             lty=c(1,1),
             lwd=c(2,2),
             col=c("red","blue"),
             bg="white",
             text.font=3)
    })
  })
  
  #Preparing NAIVE forecast plot
  output$naive <- renderPlot({
    
    withProgress(message = "Preparing Naive Forecast", {
      
      p <- getNaivePlotData()
      plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
      lines(p$test, col = "red")
      legend("bottomright",
             inset=.05,
             cex = 1,
             title="Legend",
             c("Actual Values","Predicted Values"),
             horiz=FALSE,
             lty=c(1,1),
             lwd=c(2,2),
             col=c("red","blue"),
             bg="white",
             text.font=3)
    })
  })

  #Preparing Neural Forecast Plot
  output$neural <- renderPlot({
    
    withProgress(message = "Preparing Neural Forecast", {
      
      p <- getNeuralPlotData()
      plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
      lines(p$test, col = "red")
      legend("bottomright",
             inset=.05,
             cex = 1,
             title="Legend",
             c("Actual Values","Predicted Values"),
             horiz=FALSE,
             lty=c(1,1),
             lwd=c(2,2),
             col=c("red","blue"),
             bg="white",
             text.font=3)
    })
  })
  
  #Preparing TBATS forecast plot
  output$bats <- renderPlot({
    
    withProgress(message = "Preparing BATS Forecast", {
      
      p <- getBATSPlotData()
      plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
      lines(p$test, col = "red")
      legend("bottomright",
             inset=.05,
             cex = 1,
             title="Legend",
             c("Actual Values","Predicted Values"),
             horiz=FALSE,
             lty=c(1,1),
             lwd=c(2,2),
             col=c("red","blue"),
             bg="white",
             text.font=3)
    })
  })
  
  
  #Preparing TBATS forecast plot
  output$tbats <- renderPlot({
    
    withProgress(message = "Preparing TBATS Forecast", {
      
      p <- getTBATSPlotData()
      plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
      lines(p$test, col = "red")
      legend("bottomright",
             inset=.05,
             cex = 1,
             title="Legend",
             c("Actual Values","Predicted Values"),
             horiz=FALSE,
             lty=c(1,1),
             lwd=c(2,2),
             col=c("red","blue"),
             bg="white",
             text.font=3)
    })
  })
  
  
  #Preparing STLM forecast plot
  output$stlm <- renderPlot({
    
    withProgress(message = "Preparing STLM Forecast", {
      
      p <- getSTLMPlotData()
      plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
      lines(p$test, col = "red")
      legend("bottomright",
             inset=.05,
             cex = 1,
             title="Legend",
             c("Actual Values","Predicted Values"),
             horiz=FALSE,
             lty=c(1,1),
             lwd=c(2,2),
             col=c("red","blue"),
             bg="white",
             text.font=3)
    })
  })
  
  
  #Prepare STS Forecast Plot
  output$sts <- renderPlot({
    
    withProgress(message = "Preparing STS Forecast", {
      
      p <- getSTSPlotData()
      plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
      lines(p$test, col = "red")
      legend("bottomright",
             inset=.05,
             cex = 1,
             title="Legend",
             c("Actual Values","Predicted Values"),
             horiz=FALSE,
             lty=c(1,1),
             lwd=c(2,2),
             col=c("red","blue"),
             bg="white",
             text.font=3)
    })
  })
  
  #Render Performance Model Error Statistics Barchart
  output$measurementsBar <- renderChart({
    
    e <- errorStats()
    
    p <- switch (input$measurements,
      ME = nPlot(ME~Model, data = e, type = "discreteBarChart", dom = "measurementsBar", height = 400, width = 550),
      RMSE = nPlot(RMSE~Model, data = e, type = "discreteBarChart", dom = "measurementsBar", height = 400, width = 550),
      MAE = nPlot(MAE~Model, data = e, type = "discreteBarChart", dom = "measurementsBar", height = 400, width = 550),
      MPE = nPlot(MPE~Model, data = e, type = "discreteBarChart", dom = "measurementsBar", height = 400, width = 550),
      MAPE = nPlot(MAPE~Model, data = e, type = "discreteBarChart", dom = "measurementsBar", height = 400, width = 550),
      MASE = nPlot(MASE~Model, data = e, type = "discreteBarChart", dom = "measurementsBar", height = 400, width = 550),
      ACF1 = nPlot(ACF1~Model, data = e, type = "discreteBarChart", dom = "measurementsBar", height = 400, width = 550),
      THEILS = nPlot(THEILS~Model, data = e, type = "discreteBarChart", dom = "measurementsBar", height = 400, width = 550)
    )
    p$xAxis(staggerLabels = TRUE)
    p$yAxis(axisLabel = "Error Rate", width = 65)
    return(p)
  })
  
  
  #Summary Table
  output$modelsumm <- renderDataTable({
    
    validate(
      need(input$state != "", "Please select a market using the geographic selectors in the sidebar. ")
    )
    
    e <- errorStats()

    e <- rename(e, c("THEILS" = "Theil's U"))  
    e

  })
  
  ################################################################################
  ##                        MARKET FORECAST FUNCTIONS                           ##
  ################################################################################
  #Calculate Arima Forecast
  arimaForecastData <- reactive({
    
    isolate({
      periods <- as.integer(input$forecastRange) * 12
    })
      
    #Get data and convert to timeseries
    d  <- selectHistoricalData()
    v  <- as.numeric(as.vector(d))
    d  <- ts(v, frequency = 12, start = c(2000,1))
    m  <- auto.arima(d, ic='aicc', stepwise=FALSE)
    f  <- forecast(m, periods)

  })
  
  #Render Arima forecast
  output$arimaForecastPlot <- renderPlot({
    
    input$forecast
    
    withProgress(message = "Preparing Arima Forecast", {
      
      d <- isolate(arimaForecastData())
      
      plot.forecast(d, include = 36)
        
    })
  })

  
  #Calculate ETS Forecast
  etsForecastData <- reactive({
    
    isolate({
      periods <- as.integer(input$forecastRange) * 12
    })
    
    #Get data and convert to timeseries
    d  <- selectHistoricalData()
    v  <- as.numeric(as.vector(d))
    d  <- ts(v, frequency = 12, start = c(2000,1))
    m  <- ets(d, ic='aicc', restrict=FALSE)
    f  <- forecast(m, periods)
    
  })
  
  
  # Render ETS Forecast
  output$etsForecastPlot <- renderPlot({
    
    input$forecast
    
    withProgress(message = "Preparing ETS Forecast", {
      
      d <- isolate(etsForecastData())
      
      plot.forecast(d, include = 36)
      
    })
  })
  
  
  #Calculate Naive Forecast
  naiveForecastData <- reactive({
    
    isolate({
      periods <- as.integer(input$forecastRange) * 12
    })
    
    #Get data and convert to timeseries
    d  <- selectHistoricalData()
    v  <- as.numeric(as.vector(d))
    d  <- ts(v, frequency = 12, start = c(2000,1))
    m  <- naive(d, periods)
    f  <- forecast(m, periods)
    
  })
  
  # Render Naive Forecast
  output$naiveForecastPlot <- renderPlot({
    
    input$forecast
    
    withProgress(message = "Preparing Naive Forecast", {
      
      d <- isolate(naiveForecastData())
      
      plot.forecast(d, include = 36)      
      
    })
  })
  
  
  #Calculate Neural Network  Forecast
  neuralForecastData <- reactive({
    
    isolate({
      periods <- as.integer(input$forecastRange) * 12
    })
    
    #Get data and convert to timeseries
    d  <- selectHistoricalData()
    v  <- as.numeric(as.vector(d))
    d  <- ts(v, frequency = 12, start = c(2000,1))
    m  <- nnetar(d, p=12, size=25)
    f  <- forecast(m, periods)
    
  })
  
  
  # Render Neural Network  Forecast
  output$neuralForecastPlot <- renderPlot({
    
    input$forecast
    
    withProgress(message = "Preparing Neural Forecast", {

      d <- isolate(neuralForecastData())
      
      plot.forecast(d, include = 36)      
      

    })
  })
  
  
  #Calculate BATS Network  Forecast
  batsForecastData <- reactive({
    
    isolate({
      periods <- as.integer(input$forecastRange) * 12
    })
    
    #Get data and convert to timeseries
    d  <- selectHistoricalData()
    v  <- as.numeric(as.vector(d))
    d  <- ts(v, frequency = 12, start = c(2000,1))
    m  <- bats(d, ic='aicc', seasonal.periods=12)
    f  <- forecast(m, periods)
    
  })
  
  # Render BATS Forecast
  output$batsForecastPlot <- renderPlot({
    
    input$forecast
    
    withProgress(message = "Preparing BATS Forecast", {
      
      d <- isolate(batsForecastData())
      
      plot.forecast(d, include = 36)      
      
      
    })
  })

  
  #Calculate TBATS Network  Forecast
  tbatsForecastData <- reactive({
    
    isolate({
      periods <- as.integer(input$forecastRange) * 12
    })
    
    #Get data and convert to timeseries
    d  <- selectHistoricalData()
    v  <- as.numeric(as.vector(d))
    d  <- ts(v, frequency = 12, start = c(2000,1))
    m  <- tbats(d, ic='aicc', seasonal.periods=12)
    f  <- forecast(m, periods)
    
  })
  
  # Render TBATS Forecast
  output$tbatsForecastPlot <- renderPlot({
    
    input$forecast
    
    withProgress(message = "Preparing TBATS Forecast", {
      
      d <- isolate(tbatsForecastData())
      
      plot.forecast(d, include = 36)      
      
    })
  })
  
  
  #Calculate STLM Network  Forecast
  stlmForecastData <- reactive({
    
    isolate({
      periods <- as.integer(input$forecastRange) * 12
    })
    
    #Get data and convert to timeseries
    d  <- selectHistoricalData()
    v  <- as.numeric(as.vector(d))
    d  <- ts(v, frequency = 12, start = c(2000,1))
    m  <- stlm(d, s.window=12, ic='aicc', robust=TRUE, method='ets')
    f  <- forecast(m, periods)
    
  })
  
  # Render STLM  Forecast
  output$stlmForecastPlot <- renderPlot({
    
    input$forecast
    
    withProgress(message = "Preparing STLM Forecast", {
      
      d <- isolate(stlmForecastData())
      
      plot.forecast(d, include = 36)      
      
    })
  })
  
  
  #Calculate STLM Network  Forecast
  stsForecastData <- reactive({

    isolate({
      periods <- as.integer(input$forecastRange) * 12
    })
    
    #Get data and convert to timeseries
    d  <- selectHistoricalData()
    v  <- as.numeric(as.vector(d))
    d  <- ts(v, frequency = 12, start = c(2000,1))
    m  <- StructTS(d, type = "level")
    f  <- forecast(m, periods)
    
  })
  
  # Render STS Forecast
  output$stsForecastPlot <- renderPlot({
    
    input$forecast
    
    withProgress(message = "Preparing STS Forecast", {
      
      d <- isolate(stsForecastData())
      
      plot.forecast(d, include = 36)      
      
    })
  })
  
  #Combine all model forecasts into a list
  getForecasts <- reactive({
    
    withProgress(message = "Summarizing Forecast Results", {
    
      ARIMA   <-   arimaForecastData()
      ETS     <-   etsForecastData()
      NAIVE   <-   naiveForecastData()
      NEURAL  <-   neuralForecastData()
      BATS    <-   batsForecastData()
      TBATS   <-   tbatsForecastData()
      STLM    <-   stlmForecastData()
      STS     <-   stsForecastData()
      
      
      l <- list(ARIMA, ETS, NAIVE, NEURAL, BATS, TBATS, STLM, STS)
    })
    
    return(l)
    
  })
  
  #Forecast Summary Plot
  output$forecastSummaryPlot <- renderPlot({

    input$forecast
    
    f <- isolate(getForecasts())
      
    ARIMA   <-   f[[1]]
    ETS     <-   f[[2]]
    NAIVE   <-   f[[3]]
    NEURAL  <-   f[[4]]
    BATS    <-   f[[5]]
    TBATS   <-   f[[6]]
    STLM    <-   f[[7]]
    STS     <-   f[[8]]

    lgnd <- c("ARIMA", "ETS", "NAIVE", "NEURAL", "BATS", "TBATS", "STLM", "STS")
    forecasts <- ts.union(ARIMA$mean, ETS$mean, NAIVE$mean, NEURAL$mean, BATS$mean, TBATS$mean, STLM$mean, STS$mean)
    plot(forecasts, plot.type = "single", col = 1:ncol(forecasts))
    legend("topleft", lgnd, col = 1:ncol(forecasts), lty = 1)
    
  })
  
  predictionData <- reactive({
    
    input$forecast
    
    f <- isolate(getForecasts())
    
    ARIMA   <-   f[[1]]
    ETS     <-   f[[2]]
    NAIVE   <-   f[[3]]
    NEURAL  <-   f[[4]]
    BATS    <-   f[[5]]
    TBATS   <-   f[[6]]
    STLM    <-   f[[7]]
    STS     <-   f[[8]]
    
    
    prediction_ARIMA	<-	as.vector(as.numeric(ARIMA$mean[length(ARIMA$mean)]))
    prediction_ETS	<-	as.vector(as.numeric(ETS$mean[length(ETS$mean)]))
    prediction_NAIVE	<-	as.vector(as.numeric(NAIVE$mean[length(NAIVE$mean)]))
    prediction_NEURAL	<-	as.vector(as.numeric(NEURAL$mean[length(NEURAL$mean)]))
    prediction_BATS	<-	as.vector(as.numeric(BATS$mean[length(BATS$mean)]))
    prediction_TBATS	<-	as.vector(as.numeric(TBATS$mean[length(TBATS$mean)]))
    prediction_STLM	<-	as.vector(as.numeric(STLM$mean[length(STLM$mean)]))
    prediction_STS	<-	as.vector(as.numeric(STS$mean[length(STS$mean)]))
    
    prediction <-	as.vector(c( prediction_ARIMA , prediction_ETS , prediction_NAIVE , prediction_NEURAL , prediction_BATS , prediction_TBATS , prediction_STLM , prediction_STS ))
    
    models <- c("Arima", "ETS", "Naive", "Neural", "BATS", "TBATS", "STLM", "STS")
    
    d <- data.frame(models, prediction)
    
    colnames(d) <- c("Model","Prediction")

    return(d)
    
  })
  
  #Prediction Summary Chart
  output$predictionPlot <- renderChart({

    input$forecast
    
      
    d <- predictionData()
    p <- nPlot(Prediction~Model, data = d, type = "discreteBarChart", dom = "predictionPlot", height = 400, width = 600)
    p$set(title = "Predicted Median Home Values at End of Forecast Period")
    p$xAxis(staggerLabels = TRUE)
    return(p)
 })
  
  #Render Minimum prediction box
  output$minPredictionBox <- renderValueBox({
    
    input$forecast
    
    d <- isolate(predictionData())
    l <- isolate(selectCurrentData()$location)

    validate(
      need(!is.null(d),"")
    )
      
    valueBox(
      paste0("$", round(min(d$Prediction),0)), paste("Minimum median home value prediction at end of forecast period for the ", l," market."), 
             icon = icon("dollar"), color = "red")
  })
  
  #Render Maxium prediction box
  output$maxPredictionBox <- renderValueBox({
    
    input$forecast
    
    d <- isolate(predictionData())
    l <- isolate(selectCurrentData()$location)
    
    validate(
      need(!is.null(d),"")
    )
    
    valueBox(
      paste0("$", round(max(d$Prediction),0)), paste("Maximum median home value prediction at end of forecast period for the ", l," market."), 
      icon = icon("dollar"), color = "orange")
  })
  
  #Render Maxium prediction box
  output$meanPredictionBox <- renderValueBox({
    
    input$forecast
    
    d <- isolate(predictionData())
    l <- isolate(selectCurrentData()$location)
    
    validate(
      need(!is.null(d),"")
    )
    
    valueBox(
      paste0("$", round(mean(d$Prediction),0)), paste("Mean median home value prediction at end of forecast period for the ", l," market."), 
      icon = icon("dollar"), color = "blue")
  })
})