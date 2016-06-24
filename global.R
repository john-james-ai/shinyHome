# Coursera Data Science Specialization
# Building Data Products Course
# Housing Prices Time Series Forecasting
# John James
# Start Date: May 23, 2015

# global.R

###############################################################################
#                         LOAD PACKAGES AND MODULES                          #
###############################################################################
require(rCharts)
options(RCHART_LIB = 'polycharts')
library(datasets)
library(dplyr)
library(forecast)
library(ggplot2)
library(plotly)
library(plyr)
library(rCharts)
library(shiny)
library(shinydashboard)
library(TTR)
library(xlsx)

################################################################################
#                             GLOBAL VARIABLES                                 #
################################################################################
#Default  Values
dflt <- list(state = "", county = "", city = "", zip = "", model = "ARIMA", 
                   split = as.integer(2014), maxValue = as.integer(1000000), stringsAsFactors = FALSE)

###############################################################################
#                               LOAD DATA                                     #
###############################################################################
#Delete the following line before deploying this to shiny.io
home <- getwd()

setwd("processedData")

currentZip    = read.csv("currentZip.csv", header = TRUE, stringsAsFactors = FALSE)
currentCity   = read.csv("currentCity.csv", header = TRUE, stringsAsFactors = FALSE)
currentCounty = read.csv("currentCounty.csv", header = TRUE, stringsAsFactors = FALSE)
currentState  = read.csv("currentState.csv", header = TRUE, stringsAsFactors = FALSE)
hviAllZip     = read.csv("hviAllZip.csv", header = TRUE, stringsAsFactors = FALSE)
hviAllCity    = read.csv("hviAllCity.csv", header = TRUE, stringsAsFactors = FALSE)
hviAllCounty  = read.csv("hviAllCounty.csv", header = TRUE, stringsAsFactors = FALSE)
hviAllState   = read.csv("hviAllState.csv", header = TRUE, stringsAsFactors = FALSE)

# Read model data
modelData <- read.xlsx("models.xlsx", sheetIndex = 1, header = TRUE)

# File containing unique geo codes, state,city, zip
geo <- read.csv("geo.csv", header = TRUE)

#Set directory back to project home directory
setwd(home)