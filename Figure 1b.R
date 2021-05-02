library("rgdal")
library("rgeos")
library(tmap)
library(leaflet)
library(maptools)
library(readxl)
library("tidyverse")
library(shiny)
library(shinyjs)

US.Territories <- readOGR("4th Year/Project/R Code/us_state/cb_2018_us_state_500k.shp")
US.States <- subset(US.Territories, US.Territories$STUSPS != "AK" & US.Territories$STATEFP != 11 & US.Territories$STATEFP != 15 &  US.Territories$STATEFP < 58)

Covid_19_Cases_US <- read_excel("4th Year/Project/R Code/us_state/Covid-19 Cases US.xlsx")

US.Cases <- merge(US.States, Covid_19_Cases_US, by.x="NAME", by.y="State")

tm_shape(US.Cases) + 
  tm_polygons(c("Rate 4", "Rate 5","Rate 6","Rate 7"),palette="BuPu", title= "US Cases by State", style="cont") +
  tm_facets(sync= TRUE, nrow = 2, free.scales= FALSE) +
  tm_layout(bg.color="white")

qtm(US.Cases, fill="Day 1")
qtm(US.Cases, fill="Day 2")
qtm(US.Cases, fill="Day 3")
qtm(US.Cases, fill="Day 4")
qtm(US.Cases, fill="Day 5")
qtm(US.Cases, fill="Day 6")
qtm(US.Cases, fill="Day 7")
qtm(US.Cases, fill="7 Day Total")

qtm(US.Cases, fill="Rate 1")
qtm(US.Cases, fill="Rate 2")
qtm(US.Cases, fill="Rate 3")
qtm(US.Cases, fill="Rate 4")
qtm(US.Cases, fill="Rate 5")
qtm(US.Cases, fill="Rate 6")
qtm(US.Cases, fill="Rate 7")
qtm(US.Cases, fill="7 Day Rate")

qtm(US.Cases, fill="Total Cases",fill.palette="BuPu")
qtm(US.Cases, fill="Rate")

