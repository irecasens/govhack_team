library(rmarkdown)
library(dplyr)
library(ggplot2)
library(data.table)
library(RSocrata)
library(googleway)
library(jsonlite)
library(rgdal)
library(rgeos)
library(geojsonio)
library(stringr)
library(sqldf)
library(ggmap)

token <- "cFazrCauPsdOSyn7AvVQ3detH"
df <- read.socrata("https://data.melbourne.vic.gov.au/resource/u2n7-9zb7.json", app_token = token)

url <- 'https://opendata.arcgis.com/datasets/c2a69622ebad42e7baaa8167daa72127_0.geojson'
df2 <- geojsonio::geojson_read(url,  what = "sp")

df3 = read.csv('pedestrian.csv')
