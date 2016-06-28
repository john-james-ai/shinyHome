# shinyHome
# Real Estate Analytics and Forecasting
# John James
# Date: June 27, 2016

# prePareDataLite
library(datasets)
library(data.table)
library(plyr)

################################################################################
##                          CONTROL FLAGS                                     ##
################################################################################
downloadData <- TRUE
readData <- TRUE


################################################################################
##                          DOWNLOAD DATA                                     ##
################################################################################
home <- getwd()
# Create Data Directory
if (!file.exists("rawData")) {
  dir.create("rawData")
}

setwd("rawData")

if (downloadData == TRUE) {
  # Current Month Data
  currentZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_Summary_AllHomes.csv"
  currentCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_Summary_AllHomes.csv"
  currentCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_Summary_AllHomes.csv"
  currentStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_Summary_AllHomes.csv"
  
  download.file(currentZipURL, destfile="currentZip.csv")
  download.file(currentCityURL, destfile="currentCity.csv")
  download.file(currentCountyURL, destfile="currentCounty.csv")
  download.file(currentStateURL, destfile="currentState.csv")
  
  # All Home Price Index Time Series
  hviAllZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_AllHomes.csv"
  hviAllCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_AllHomes.csv"
  hviAllCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_AllHomes.csv"
  hviAllStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_AllHomes.csv"
  
  download.file(hviAllZipURL, destfile="hviAllZip.csv")
  download.file(hviAllCityURL, destfile="hviAllCity.csv")
  download.file(hviAllCountyURL, destfile="hviAllCounty.csv")
  download.file(hviAllStateURL, destfile="hviAllState.csv")
  

}
  
################################################################################
##                              READ DATA                                     ##
################################################################################

if (readData == TRUE) {
  # Current Month Data
  print("Reading current data")
  currentZip <- read.csv("currentZip.csv", header = TRUE)
  currentCity <- read.csv("currentCity.csv", header = TRUE)
  currentCounty <- read.csv("currentCounty.csv", header = TRUE)
  currentState <- read.csv("currentState.csv", header = TRUE)
  
  
  # All Home Price Index Time Series
  print("Reading All Home data")
  hviAllZip <- read.csv("hviAllZip.csv", header = TRUE)
  hviAllCity <- read.csv("hviAllCity.csv", header = TRUE)
  hviAllCounty <- read.csv("hviAllCounty.csv", header = TRUE)
  hviAllState <- read.csv("hviAllState.csv", header = TRUE)

}


################################################################################
##                          PREPROCESS DATA                                   ##
################################################################################
# Combine state abbreviations and state names into a data frame.
stateInfo <- data.frame(state.abb, state.name)
colnames(stateInfo) <- c("State", "StateName")

# Current Month Data
print("Processing current month data")
currentZip <- merge(currentZip, stateInfo)
currentCity <- merge(currentCity, stateInfo)
currentCounty <- merge(currentCounty, stateInfo)


# All Home Price Index Time Series
print("Processing a homes  data")
hviAllZip <- merge(hviAllZip, stateInfo)
hviAllCity <- merge(hviAllCity, stateInfo)
hviAllCounty <- merge(hviAllCounty, stateInfo)

#Create geo file that contains sorted state, city and zip for dropdowns.
geo <-  select(currentZip, State, StateName, County, City, RegionName)
geo <-  rename(geo, c("RegionName" = "Zip"))
geo <-  arrange(geo, StateName, County, City, Zip)

## Change RegionName variable
print("Change RegionName variable")
setnames(currentZip, "RegionName", "Zip")
setnames(currentCity, "RegionName", "City")
setnames(currentCounty, "RegionName", "County")
setnames(currentState, "RegionName", "State")
setnames(hviAllZip, "RegionName", "Zip")
setnames(hviAllCity, "RegionName", "City")
setnames(hviAllCounty, "RegionName", "County")
setnames(hviAllState, "RegionName", "State")

## Create location variable
print("Creating location variable")
currentZip$location    <- paste0(currentZip$City, ", ", currentZip$State, " ", currentZip$Zip)
currentCity$location   <- paste0(currentCity$City, ", ", currentCity$State)
currentCounty$location <- paste0(currentCounty$County, ", ", currentCounty$State)
currentState$location  <- paste0(currentState$State)
hviAllZip$location <- paste0(hviAllZip$City, ", ", hviAllZip$State, " ",hviAllZip$Zip)
hviAllCity$location <- paste0(hviAllCity$City, ", ", hviAllCity$State)
hviAllCounty$location <- paste0(hviAllCounty$County, ", ", hviAllCounty$State)
hviAllState$location <- paste0(hviAllState$State)

## Change Monthly Growth variable
print("Change Monthly Growth variable")
setnames(currentZip, "MoM", "Monthly")
setnames(currentCity, "MoM", "Monthly")
setnames(currentCounty, "MoM", "Monthly")
setnames(currentState, "MoM", "Monthly")

## Change Quarterly Growth variable
print("Change Quarterly Growth variable")
setnames(currentZip, "QoQ", "Quarterly")
setnames(currentCity, "QoQ", "Quarterly")
setnames(currentCounty, "QoQ", "Quarterly")
setnames(currentState, "QoQ", "Quarterly")

## Change Annual Growth variable
print("Change Annual Growth variable")
setnames(currentZip, "YoY", "Annual")
setnames(currentCity, "YoY", "Annual")
setnames(currentCounty, "YoY", "Annual")
setnames(currentState, "YoY", "Annual")

## Change 5 Year Growth variable
print("Change 5 Year Growth variable")
setnames(currentZip, "X5Year", "Five_Year")
setnames(currentCity, "X5Year", "Five_Year")
setnames(currentCounty, "X5Year", "Five_Year")
setnames(currentState, "X5Year", "Five_Year")

## Change 10 Year Growth variable
print("Change 10 Year Growth variable")
setnames(currentZip, "X10Year", "Ten_Year")
setnames(currentCity, "X10Year", "Ten_Year")
setnames(currentCounty, "X10Year", "Ten_Year")
setnames(currentState, "X10Year", "Ten_Year")

## Change Value variable
print("Change 10 Year Growth variable")
setnames(currentZip, "Zhvi", "Value")
setnames(currentCity, "Zhvi", "Value")
setnames(currentCounty, "Zhvi", "Value")
setnames(currentState, "Zhvi", "Value")


################################################################################
##                             WRITE  DATA                                    ##
################################################################################
# Change working directory to data directory
setwd(home)

# Create Data Directory
if (!file.exists("processedData")) {
  dir.create("processedData")
}

setwd("processedData")

# Current Month Data
print("Writing current data")
write.csv(currentZip, file = "currentZip.csv", row.names = FALSE, na = "")
write.csv(currentCity, file = "currentCity.csv", row.names = FALSE, na = "")
write.csv(currentCounty, file = "currentCounty.csv", row.names = FALSE, na = "")
write.csv(currentState, file = "currentState.csv", row.names = FALSE, na = "")


# All Home Price Index Time Series
print("Writing all homes data")
write.csv(hviAllZip, file = "hviAllZip.csv", row.names = FALSE, na = "")
write.csv(hviAllCity, file = "hviAllCity.csv", row.names = FALSE, na = "")
write.csv(hviAllCounty, file = "hviAllCounty.csv", row.names = FALSE, na = "")
write.csv(hviAllState, file = "hviAllState.csv", row.names = FALSE, na = "")

#Write Geo File
write.csv(geo, file = "geo.csv", row.names = FALSE, na = "")

 #Reset working directory
setwd(home)