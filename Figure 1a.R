library(ggplot2)
library(tmap)
library("maps")
library(readxl)
library(gstat)
library(lattice)
library(sp)
library("rgdal")
library("rgeos")

US_Hospitals_Sample <- read_excel("Data Sets/US Hospitals Sample.xlsx", sheet ="Sheet1")
US.States <- readOGR("R Code/cb_2018_us_state_500k.shx")

Hospital_plot <- ggplot(US_Hospitals_Sample) +             # plot points
  geom_point(aes(x = `Longitude` ,y = `Latitude`, colour= "red")) + 
  xlab("Longitude (deg)") +             # x-axis label
  ylab("Latitude (deg)") +              # y-axis label
  geom_path(data = map_data("state"),   # add US states map
            aes(x = long, y = lat, group = group)) 

print(Hospital_plot)


US_Hospitals <- read_excel("Data Sets/US Hospitals Sample.xlsx", sheet ="Sheet2")
US_States_Hospitals <-merge(US.States, US_Hospitals, x.by="STUSPS", y.by="State", duplicateGeoms = TRUE)

qtm(US_States_Hospitals, fill="Hospitals")

    