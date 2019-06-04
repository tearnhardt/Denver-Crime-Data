## Denver Crime Dataset 
## Goals:
## 1. Find any correlations and predictions.
## 2. Create a shape that maps things to the city 
## 3. Pull in verified datasets for other things going on in Denver
  ## i.e. Weather, income, news, birthrates, etc.

## Libraries ## 
library(tidyverse)
library(dplyr)
library(maptools)


## Directories ## 
setwd("C:/Users/p/Documents/R Programming/denver-crime-data")


## Reading in the Data ## 
denver_crime <- read.csv("crime.csv")
denver_crime$INCIDENT_ID <- as.character(denver_crime$INCIDENT_ID)
denver_crime$OFFENSE_ID <- as.character(denver_crime$OFFENSE_ID)
denver_crime$FIRST_OCCURRENCE_DATE <- as.character((denver_crime$FIRST_OCCURRENCE_DATE))
denver_crime$LAST_OCCURRENCE_DATE <- as.character(denver_crime$LAST_OCCURRENCE_DATE)
denver_crime$REPORTED_DATE <- as.character(denver_crime$REPORTED_DATE)

#test <- denver_crime[sample(1:nrow(denver_crime), 50, replace=FALSE),]
test <- denver_crime

## Date Cleaning ## 
date <- function(chr,dataset){
  vector <- chr
  x <- nrow(dataset)
  date <- 1:x
  for(i in 1:x) {
    date[i] <- substring(vector[i],1,10)
    for(j in 0:9){
      s <- as.character(j)
      symbol <- paste("", s)
      date[i] <- gsub(symbol,"", date[i])
    }
    date[i] <- gsub(" ", "",date[i])
  }
  return(date)
}



test$REPORTED_DATE <- date(test$REPORTED_DATE,test)

## Getting the month and year ## 
year <- function(chr,dataset){
  vector <- chr
  x <- nrow(dataset)
  year<- 1:x
  for(i in 1:x) {
    length <- nchar(vector[i], type = "chars")
    start <- length - 3
    year[i] <- substring(vector[i],start)
  }
  return(year)
}

test$REPORTED_YEAR<-year(test$REPORTED_DATE, test)

month <- function(chr,dataset){
  vector <- chr 
  x <- nrow(dataset)
  month <- 1:x
  for(i in 1:x) {
    month[i] <- substring(vector[i],1,2)
    month[i] <- gsub("/", "", month[i])
  }
  return(month)
}

test$REPORTED_MONTH<-month(test$REPORTED_DATE, test)


## General Plots ## 
test$REPORTED_YEAR <- as.factor(test$REPORTED_YEAR)
test$DISTRICT_ID <- as.factor(test$DISTRICT_ID)

cleaned <- test %>% select(REPORTED_YEAR,IS_CRIME, DISTRICT_ID)
cleaned <- cleaned %>% group_by(REPORTED_YEAR,DISTRICT_ID) %>% summarise(CRIME = sum(IS_CRIME))


ggplot(cleaned,aes(x= REPORTED_YEAR, y= CRIME, group=DISTRICT_ID)) + geom_point(aes(color=DISTRICT_ID)) + geom_line(aes(color = DISTRICT_ID))

traffic <- test %>% select(REPORTED_YEAR,IS_TRAFFIC, DISTRICT_ID)
traffic <- traffic %>% group_by(REPORTED_YEAR, DISTRICT_ID) %>% summarise(TRAFFIC = sum(IS_TRAFFIC))


ggplot(traffic,aes(x= REPORTED_YEAR, y= TRAFFIC, group=DISTRICT_ID)) + geom_point(aes(color=DISTRICT_ID)) + geom_line(aes(color = DISTRICT_ID))


## Mapping to a map ## 
setwd("C:/Users/p/Documents/R Programming/denver-crime-data/Maps")
map <- sf::st_read("crime.shp")
