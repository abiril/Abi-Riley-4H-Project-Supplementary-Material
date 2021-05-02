library("rgdal")
library("rgeos")
library(tmap)
library(leaflet)
library(maptools)
library(readxl)
library("tidyverse")
library(shiny)
library(shinyjs)
library(MASS)
library(ggplot2)

par(mfrow=c(1,1))
UK.Regions <- readOGR("Region_(December_2015)_Boundaries.shp")
plot(UK.Regions)

UK_Dataset <- read_excel("SIR Dataset.xlsx", sheet = "Weekly")
UK.Cases <- merge(UK.Regions, UK_Dataset, by.x="rgn15nm", by.y="Region")

qtm(UK.Cases, fill="Population",fill.palette="BuPu")

qtm(UK.Cases, fill="O2", ylim=c(0,5000),bg.color="white", fill.palette="BuPu")
qtm(UK.Cases, fill="O5")
qtm(UK.Cases, fill="O10")

qtm(UK.Cases, fill="E2", bg.color="white", fill.palette="Purples")
qtm(UK.Cases, fill="E5")
qtm(UK.Cases, fill="E10")

par(mfrow=1)
tm_shape(UK.Cases) + 
  tm_polygons(c("O2","O7"), palette="BuPu", title= "UK Cases by Region", style = "log10_pretty") +
  tm_facets(sync= TRUE, ncol = 2, free.scales= FALSE)
  
UK_Total_Cases<- read_excel("SIR Dataset.xlsx", sheet = "Sheet5")
plot(UK_Total_Cases)

library(deSolve)

sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    N = S + I + R
    dS <- -(beta * S * I )/ N
    dI <- (beta * S * I )/ N - (gamma * I) / N
    dR <- (gamma * I) / N
    
    return(list(c(dS, dI, dR)))
  })
}

init       <- c(S = 30000, I = 2, R = 0)
parameters <- c(beta = 0.3, gamma = 50)
times      <- seq(0, 64, by = 1)


out <- ode(y = init, times = times, func = sir, parms = parameters)
out <- as.data.frame(out)
out$time <- NULL
head(out, 10)

matplot(x = times, y = out, type = "l",
        xlab = "Time", ylab = "Population", main = "SIR Model",
        lwd = 1, lty = 1, bty = "l", col = 2:4)

points(UK_Total_Cases$Day, UK_Total_Cases$Cases)
points(UK_Total_Cases$Day, UK_Total_Cases$Recovered)
legend(35, 20000, c("Susceptible", "Infected", "Recovered"), pch = 1, col = 2:4, bty = "n")

Observed_Cases<- read_excel("SIR Dataset.xlsx", sheet = "Sheet7")
Day <- 0:100
#par(mfrow=c(3,3))
plot(Day, 100000*dpois(Day, 44), type="l", ylab="Cases", main="London")
points(Observed_Cases$Day, Observed_Cases$London)
plot(Day, 30000*dpois(Day, 53), type="l", ylab="Cases", main="South West")
points(Observed_Cases$Day, Observed_Cases$'South West')
plot(Day, 65000*dpois(Day, 50), type="l", ylab="Cases", main="South East")
points(Observed_Cases$Day, Observed_Cases$'South East')
plot(Day, 45000*dpois(Day, 45), type="l", ylab="Cases", main="East of England")
points(Observed_Cases$Day, Observed_Cases$'East of England')
plot(Day, 45000*dpois(Day, 43), type="l", ylab="Cases", main="West Midlands")
points(Observed_Cases$Day, Observed_Cases$'West Midlands')
plot(Day, 32000*dpois(Day, 50), type="l", ylab="Cases", main="East Midlands")
points(Observed_Cases$Day, Observed_Cases$'East Midlands')
plot(Day, 55000*dpois(Day, 55), type="l", ylab="Cases", main="Yorkshire and The Humber")
points(Observed_Cases$Day, Observed_Cases$'Yorkshire and The Humber')
plot(Day, 70000*dpois(Day, 42), type="l", ylab="Cases", main="North West")
points(Observed_Cases$Day, Observed_Cases$'North West')
plot(Day, 38000*dpois(Day, 52), type="l", ylab="Cases", main="North East")
points(Observed_Cases$Day, Observed_Cases$'North East')


x <- rgamma(1000, shape = 2, rate = 0.2)
den <- density(x)
dat <- data.frame(x = den$x, y = den$y)

ggplot(data = dat, aes(x = x, y = y)) + 
  geom_point(size = 1)
points(Observed_Cases$Day, Observed_Cases$London)


plot(UK.Regions, border="blue", axes=TRUE, las=1)
sp2WB(as(UK.Regions, "SpatialPolygons"), filename="UKRegions.map")
x <- readAsciiGrid(system.file("grids/test.ag",
                               package="maptools")[1])
xp <- as(x, "SpatialPixelsDataFrame")
library(sp)
pp <- as.SpatialPolygons.SpatialPixels(xp)
sp2WB(as(UK.Regions,"SpatialPolygons"), filename="UKRegions.map")


qtm(UK.Cases, fill="Posterior", ylim=c(0,40),bg.color="white", fill.palette="BuPu")

tm_shape(UK.Cases) + 
  tm_polygons(c("Posterior"), palette="BuPu", title= "UK Cases by Region", style = "cont") +
  tm_facets(sync= TRUE, ncol = 1, free.scales= FALSE)
