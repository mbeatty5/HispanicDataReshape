## Title: Hispanic Data Reshape
## Author: Michael Beatty

## This R script is utilized to reshape data from horizontal to vertical,
## in such a way that is useful for further analysis and presentation
## in Excel or Tableau. In the case of this project, the data will be
## mapped out in Tableau for use in a presentation. The data is exported
## from the US Census American Fact Finder's guided search web tool. 
## https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml
## The specific dataset has an ID of B03001 using 5yr ACS Estimates,
## and is titled Hispanic or Latino Origin by Specific Origin.
## This helps show the detailed Hispanic demographic makeup and change
## within the Total US population, and was used in this project to help
## support the case for expanded distribution of an authentic Hispanic
## brand.

## STATES

## Clear workspace and memory
rm(list = ls())
gc()
# CTRL + L

## Load libraries
library(reshape2)

## FUNCTIONS:

## datamelt function explained by line:
##   Change column names for easier understanding
##   Reshape data horizontal to vertical
##   Subset by column, removing useless
##   Add column for year
##   Output the melted data frame
datamelt <- function(data = data.frame, year = 2017) {
  colnames(data) <- c("ID", 
                      "ID2", 
                      "Geography", 
                      "EstimateTotalPopulation", 
                      "MarginTotalPopulation", 
                      "EstimateTotalNotHispanic", 
                      "MarginTotalNotHispanic", 
                      "EstimateTotalHispanic", 
                      "MarginTotalHispanic", 
                      "EstimateMexican", 
                      "MarginMexican", 
                      "EstimatePuertoRican", 
                      "MarginPuertoRican", 
                      "EstimateCuban", 
                      "MarginCuban", 
                      "EstimateDominican", 
                      "MarginDominican", 
                      "EstimateCentralAmerican", 
                      "MarginCentralAmerican", 
                      "EstimateCostaRican", 
                      "MarginCostaRican", 
                      "EstimateGuatemalan", 
                      "MarginGuatemalan", 
                      "EstimateHonduran", 
                      "MarginHonduran", 
                      "EstimateNicaraguan", 
                      "MarginNicaraguan", 
                      "EstimatePanamanian", 
                      "MarginPanamanian", 
                      "EstimateSalvadoran", 
                      "MarginSalvadoran", 
                      "EstimateOtherCentralAmerican", 
                      "MarginOtherCentralAmerican", 
                      "EstimateSouthAmerican", 
                      "MarginSouthAmerican", 
                      "EstimateArgentinean", 
                      "MarginArgentinean", 
                      "EstimateBolivian", 
                      "MarginBolivian", 
                      "EstimateChilean", 
                      "MarginChilean", 
                      "EstimateColombian", 
                      "MarginColombian", 
                      "EstimateEcuadorian", 
                      "MarginEcuadorian", 
                      "EstimateParaguayan", 
                      "MarginParaguayan", 
                      "EstimatePeruvian", 
                      "MarginPeruvian", 
                      "EstimateUruguayan", 
                      "MarginUruguayan", 
                      "EstimateVenezuelan", 
                      "MarginVenezuelan", 
                      "EstimateOtherSouthAmerican", 
                      "MarginOtherSouthAmerican", 
                      "EstimateOtherHispanic", 
                      "MarginOtherHispanic", 
                      "EstimateSpaniard", 
                      "MarginSpaniard", 
                      "EstimateSpanish", 
                      "MarginSpanish", 
                      "EstimateSpanishAmerican", 
                      "MarginSpanishAmerican", 
                      "EstimateAllOtherHispanicLatino", 
                      "MarginAllOtherHispanicLatino")
  data.melt <- melt(data, 
                    id = c("ID", 
                           "ID2", 
                           "Geography"), 
                    na.rm = TRUE, 
                    variable.name="Variable", 
                    value.name = "Estimate")
  data.melt <- data.melt[, c("Geography", 
                             "Variable", 
                             "Estimate")]
  data.melt["Year"] <- year
  data.melt
}

left = function(text, num_char) {
  substr(text, 1, num_char)
}

## Read in data files corresponding to each year
hispanic.data.2017 <- read.csv(file.choose(), header = TRUE, skip = 1)
hispanic.data.2012 <- read.csv(file.choose(), header = TRUE, skip = 1)

## Reshape the data horizontal to vertical
data.melted.2017 <- datamelt(hispanic.data.2017, year = 2017)
data.melted.2012 <- datamelt(hispanic.data.2012, year = 2012)
##View(data.melted.2017)
##View(data.melted.2012)

## Merge data
data.merged <- merge(data.melted.2017, 
                     data.melted.2012, 
                     by.x = c("Geography", "Variable"), 
                     by.y = c("Geography", "Variable"))
##View(data.merged)

## Column names cleaned up
colnames(data.merged) <- c("Geography", 
                           "Variable", 
                           "Estimate2017", 
                           "Year2017", 
                           "Estimate2012", 
                           "Year2012")

## Remove Year columns
data.merged <- data.merged[, c("Geography", 
                               "Variable", 
                               "Estimate2017", 
                               "Estimate2012")]

## Add Var column to help subset
data.merged["Var"] <- left(data.merged$Variable, 8)

## Keep only Estimate rows, removing Margin
data.merged <- subset(data.merged, data.merged$Var =="Estimate")

## Remove Var column
data.merged <- data.merged[, c("Geography", 
                               "Variable", 
                               "Estimate2017", 
                               "Estimate2012")]

## Clean Variable column
data.merged$Variable <- gsub("Estimate", "", data.merged$Variable)
##View(data.merged)

## Create new data frame with only TotalPopulation rows
totalpopulation <- data.merged[data.merged$Variable == "TotalPopulation",]
##View(totalpopulation)

## Clean column names of new data frame
colnames(totalpopulation) <- c("Geography", 
                               "Variable", 
                               "TotalPopulation2017", 
                               "TotalPopulation2012")

## Remove Variable column
totalpopulation <- totalpopulation[, c("Geography", 
                                       "TotalPopulation2017", 
                                       "TotalPopulation2012")]

## Create new data frame with all except TotalPopulation rows
data.final <- data.merged[data.merged$Variable != "TotalPopulation",]
##View(data.final)

## Merge both new data frames so that TotalPopulation 2012 and 2017 are now columns matched by Geo
data.final <- merge(data.final, 
                    totalpopulation, 
                    by.x = "Geography", 
                    by.y = "Geography")
##View(data.final)


setwd("C:/Users/mbeat/Documents/IMPORTANT FILES/Employment Documents/Hispanic Dashboard")
write.csv(data.final, "5yrHispanicMeltStates.csv", row.names=FALSE, na="")




## ZIP CODES

## Clear workspace and memory
rm(list = ls())
gc()
# CTRL + L

## Load libraries
library(reshape2)

## FUNCTIONS:

## datamelt function explained by line:
##   Change column names for easier understanding
##   Reshape data horizontal to vertical
##   Subset by column, removing useless
##   Add column for year
##   Output the melted data frame
datamelt <- function(data = data.frame, year = 2017) {
  colnames(data) <- c("ID", 
                      "ID2", 
                      "Geography", 
                      "EstimateTotalPopulation", 
                      "MarginTotalPopulation", 
                      "EstimateTotalNotHispanic", 
                      "MarginTotalNotHispanic", 
                      "EstimateTotalHispanic", 
                      "MarginTotalHispanic", 
                      "EstimateMexican", 
                      "MarginMexican", 
                      "EstimatePuertoRican", 
                      "MarginPuertoRican", 
                      "EstimateCuban", 
                      "MarginCuban", 
                      "EstimateDominican", 
                      "MarginDominican", 
                      "EstimateCentralAmerican", 
                      "MarginCentralAmerican", 
                      "EstimateCostaRican", 
                      "MarginCostaRican", 
                      "EstimateGuatemalan", 
                      "MarginGuatemalan", 
                      "EstimateHonduran", 
                      "MarginHonduran", 
                      "EstimateNicaraguan", 
                      "MarginNicaraguan", 
                      "EstimatePanamanian", 
                      "MarginPanamanian", 
                      "EstimateSalvadoran", 
                      "MarginSalvadoran", 
                      "EstimateOtherCentralAmerican", 
                      "MarginOtherCentralAmerican", 
                      "EstimateSouthAmerican", 
                      "MarginSouthAmerican", 
                      "EstimateArgentinean", 
                      "MarginArgentinean", 
                      "EstimateBolivian", 
                      "MarginBolivian", 
                      "EstimateChilean", 
                      "MarginChilean", 
                      "EstimateColombian", 
                      "MarginColombian", 
                      "EstimateEcuadorian", 
                      "MarginEcuadorian", 
                      "EstimateParaguayan", 
                      "MarginParaguayan", 
                      "EstimatePeruvian", 
                      "MarginPeruvian", 
                      "EstimateUruguayan", 
                      "MarginUruguayan", 
                      "EstimateVenezuelan", 
                      "MarginVenezuelan", 
                      "EstimateOtherSouthAmerican", 
                      "MarginOtherSouthAmerican", 
                      "EstimateOtherHispanic", 
                      "MarginOtherHispanic", 
                      "EstimateSpaniard", 
                      "MarginSpaniard", 
                      "EstimateSpanish", 
                      "MarginSpanish", 
                      "EstimateSpanishAmerican", 
                      "MarginSpanishAmerican", 
                      "EstimateAllOtherHispanicLatino", 
                      "MarginAllOtherHispanicLatino")
  data.melt <- melt(data, 
                    id = c("ID", 
                           "ID2", 
                           "Geography"), 
                    na.rm = TRUE, 
                    variable.name="Variable", 
                    value.name = "Estimate")
  data.melt <- data.melt[, c("Geography", 
                             "Variable", 
                             "Estimate")]
  data.melt["Year"] <- year
  data.melt
}

left = function(text, num_char) {
  substr(text, 1, num_char)
}

## Read in data files corresponding to each year
hispanic.data.2017 <- read.csv(file.choose(), header = TRUE, skip = 1)
hispanic.data.2012 <- read.csv(file.choose(), header = TRUE, skip = 1)

## Reshape the data horizontal to vertical
data.melted.2017 <- datamelt(hispanic.data.2017, year = 2017)
data.melted.2012 <- datamelt(hispanic.data.2012, year = 2012)
##View(data.melted.2017)
##View(data.melted.2012)

## Merge data
data.merged <- merge(data.melted.2017, 
                     data.melted.2012, 
                     by.x = c("Geography", 
                              "Variable"), 
                     by.y = c("Geography", 
                              "Variable"))
##View(data.merged)

## Column names cleaned up
colnames(data.merged) <- c("Geography", 
                           "Variable", 
                           "Estimate2017", 
                           "Year2017", 
                           "Estimate2012", 
                           "Year2012")

## Remove Year columns
data.merged <- data.merged[, c("Geography", 
                               "Variable", 
                               "Estimate2017", 
                               "Estimate2012")]

## Add Var column to help subset
data.merged["Var"] <- left(data.merged$Variable, 8)

## Keep only Estimate rows, removing Margin
data.merged <- subset(data.merged, data.merged$Var =="Estimate")

## Remove Var column
data.merged <- data.merged[, c("Geography", 
                               "Variable", 
                               "Estimate2017", 
                               "Estimate2012")]

## Clean Variable column
data.merged$Variable <- gsub("Estimate", "", data.merged$Variable)
##View(data.merged)

## Create new data frame with only TotalPopulation rows
totalpopulation <- data.merged[data.merged$Variable == "TotalPopulation",]
##View(totalpopulation)

## Clean column names of new data frame
colnames(totalpopulation) <- c("Geography", 
                               "Variable", 
                               "TotalPopulation2017", 
                               "TotalPopulation2012")

## Remove Variable column
totalpopulation <- totalpopulation[, c("Geography", 
                                       "TotalPopulation2017", 
                                       "TotalPopulation2012")]

## Create new data frame with all except TotalPopulation rows
data.final <- data.merged[data.merged$Variable != "TotalPopulation",]
##View(data.final)

## Merge both new data frames so that TotalPopulation 2012 and 2017 are now columns matched by Geo
data.final <- merge(data.final, 
                    totalpopulation, 
                    by.x = "Geography", 
                    by.y = "Geography")
##View(data.final)

## Clean Geography column
data.final$Geography <- gsub("ZCTA5 ","",data.final$Geography)
View(data.final)

## Split data into a large list by Variable factor
spl <- split(data.final, data.final$Variable)

## Apply write.csv function to names of list to write file for each level
setwd("C:/Users/mbeat/Documents/IMPORTANT FILES/Employment Documents/Hispanic Dashboard/New Zip Code Data")
lapply(names(spl), function(x){write.csv(spl[[x]], file = paste("Output", x, ".csv", sep = ""))})

