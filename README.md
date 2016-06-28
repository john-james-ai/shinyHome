# shinyHome
Real Estate Market Forecasting and Analytics

## Introduction
shinyHome allows the user to explore real estate market statistics and to employ the most acknowledged time series 
forecasting algorithms to predict home values for up to 10 years, for over 20,000 markets.  Users will:

* Explore current and historical median home value data,
* Analyze price trends using time series decomposition techniques,
*	Create and evaluate prediction modules using eight time series forecasting algorithms, and 
*	Forecast home values using the prediction algorithms.

## Configuration 
The application was written on R 3.3.1 for Windows.

## Requirements 
This application requires the following R packages.
*	datasets
*	dplyr
*	forecast
*	ggplot2
*	plotly
*	plyr
*	rCharts
*	shiny
*	shinydashboard
*	TTR
*	xlsx
	
## File Manifest
The file manifest is as follows:
* currentCity: Current home value index and growth rates by city 
* currentCounty: Current home value index and growth rates by county
* currentState:  Current home value index and growth rates by state
* currentZip: Current home value index and growth rates by zip code
* geo: State, county, city and zip code cross-reference file
* hviAllCity: Historical home value data by city
* hviAllCounty: Historical home value data by county
* hviAllState: Historical home value data by state
* hviAllZip: Historical home value data by zip code
* models: Descriptions of forecasting algorithms employed

## Copyright
©John James, 2016

## Contact 
Developer: John James john.james.sf@gmail.com

## Known Bugs
When running the structural model for time series by maximum likelihood (StructTS) the application occasionally throws the following error:
Error in optim(start, f, method = method, hessian = TRUE, ...) :    L-BFGS-B needs finite values of 'fn'

## Credits and Acknowledgements
* Huge acknowledgement to Zillow Research for the data.
* Ramnath Vaidyanathan  for a beautiful charting package
* Joe Cheng for his active support in the githubsphere
* Avril Coghlan for the not so little book on R for time series.
* The universe of shiny programmers that seem to have already asked and answered all my questions before I knew I had them.
