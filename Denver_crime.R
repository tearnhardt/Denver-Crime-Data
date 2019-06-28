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
library(sf)
library(leaflet)
library(geojsonio)
library(htmlwidgets)
library(htmltools)



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
test$REPORTED_DATE <- as.factor(test$REPORTED_DATE)

cleaned <- test %>% select(REPORTED_YEAR,IS_CRIME, DISTRICT_ID)
cleaned <- cleaned %>% group_by(REPORTED_YEAR,DISTRICT_ID) %>% summarise(CRIME = sum(IS_CRIME))


ggplot(cleaned,aes(x= REPORTED_YEAR, y= CRIME, group=DISTRICT_ID)) + geom_point(aes(color=DISTRICT_ID)) + geom_line(aes(color = DISTRICT_ID))

traffic <- test %>% select(REPORTED_YEAR,IS_TRAFFIC, DISTRICT_ID)
traffic <- traffic %>% group_by(REPORTED_YEAR, DISTRICT_ID) %>% summarise(TRAFFIC = sum(IS_TRAFFIC))


ggplot(traffic,aes(x= REPORTED_YEAR, y= TRAFFIC, group=DISTRICT_ID)) + geom_point(aes(color=DISTRICT_ID)) + geom_line(aes(color = DISTRICT_ID))

all_dates <- test %>% select(REPORTED_DATE, IS_CRIME, DISTRICT_ID) %>% group_by(REPORTED_DATE, DISTRICT_ID) %>% 
  summarise(CRIME = sum(IS_CRIME))


ggplot(all_dates,aes(x= REPORTED_DATE, y= CRIME)) + geom_area(aes(color= DISTRICT_ID, fill = DISTRICT_ID), alpha= .5)
 
## Mapping to county boundary ## 
setwd("C:/Users/p/Documents/R Programming/denver-crime-data/Maps")
map <- sf::st_read("county_boundary.shp")




pdf("Map.pdf")
ggplot() + geom_sf(data = map, color = "black", fill = "red") + coord_sf()
dev.off()

## Testing mapping the crime points to a map ## 
  ## Offenses

sample <- denver_crime[sample(1:nrow(denver_crime), 50, replace=FALSE),]

sample <- sample %>% select(OFFENSE_CATEGORY_ID,GEO_LON,GEO_LAT) %>% drop_na(GEO_LON,GEO_LAT)
denver_map <- st_read("county_boundary.shp")

names(sample)<- c("Offense","Longitude", "Latitude")
Denver <- leaflet(sample) %>% addProviderTiles("Stamen.Toner") %>%
    addPolygons(data=denver_map, color = "red") %>% addCircleMarkers(label=~Offense,
                                                      weight = 3, 
                                                      radius=10, 
                                                      color="blue") 
Denver
  ## Districts
new_sample <- denver_crime[sample(1:nrow(denver_crime), 50, replace=FALSE),]

new_sample <- new_sample %>% select(GEO_LON,GEO_LAT, DISTRICT_ID) %>% drop_na(GEO_LAT,GEO_LON)
names(new_sample)<- c("Longitude", "Latitude","District")
new_sample$District <- as.character(new_sample$District)
new_Denver <- leaflet(new_sample) %>% addProviderTiles("Stamen.Toner") %>%
    addPolygons(data=denver_map, color= "red") %>% addCircleMarkers(label=~District,weight=3,
                                                                    radius=5,color="blue")


new_Denver

  ## Full data 
full_denver <- test %>% filter(REPORTED_YEAR == "2018" & REPORTED_MONTH == "1")

full_denver <- full_denver %>% select(GEO_LON, GEO_LAT, OFFENSE_CATEGORY_ID) %>% drop_na(GEO_LON,GEO_LAT)
names(full_denver)<- c("Longitude","Latitude","Offense")
full_denver$Offense <- as.factor(full_denver$Offense)
color_fact <- colorFactor(palette = c('lightsalmon3',
                                      'lightsalmon4',
                                      'lightseagreen',
                                      'lightskyblue',
                                      'lightskyblue1',
                                      'lightskyblue3',
                                      'mediumpurple3',
                                      'mediumpurple4',
                                      'mediumseagreen',
                                      'mediumslateblue',
                                      'mediumspringgreen',
                                      'mediumturquoise',
                                      'mediumvioletred',
                                      'midnightblue',
                                      'mintcream'), 
                          domain = full_denver$Offense)
full_map <- leaflet(full_denver) %>% addProviderTiles("Stamen.Toner") %>% 
    addPolygons(data=denver_map,color = "red") %>% addCircles(label=~Offense,
                                                                    weight= 3,
                                                                    radius=10, color = ~color_fact(Offense), fill=TRUE)
full_map

## Analysis / Models 
  ## Difference between districts by year? 
  denv <- test %>% select(IS_CRIME,DISTRICT_ID,REPORTED_YEAR) %>% filter(REPORTED_YEAR != "2019") %>% group_by(DISTRICT_ID,REPORTED_YEAR) %>% summarize(CRIME = sum(IS_CRIME))
  
  levels(denv$DISTRICT_ID)
  denv$DISTRICT_ID <- ordered(denv$DISTRICT_ID, levels=c("1", "2", "3", "4", "5", "6", "7"))  
res.aov <- aov(CRIME ~ DISTRICT_ID, data = denv)  
summary(res.aov)
TukeyHSD(res.aov)
plot(res.aov,2)
plot(res.aov,1)
