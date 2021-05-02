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
library(TSstudio)

SIR_Cases <- read_excel("SIR Dataset.xlsx",sheet = "DailyT")
SIR_Cases2 <- read_excel("SIR Dataset.xlsx",sheet = "Daily")

ts_plot(SIR_Cases[,-11], title = "UK Cases by Region March 2020", Ytitle = "Number of Cases")
plot(SIR_Cases$Date, SIR_Cases$Total, type="l")

fitdistr(SIR_Cases$`Total`, "poisson")


library(deSolve)

sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    N = S + I + R
    dS <- -(beta/N) * S * I
    dI <- (beta/N) * S * I - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init       <- c(S = 55797158, I = 20, R = 0)
## beta: infection parameter; gamma: recovery parameter
parameters <- c(beta = 1/3, gamma = 1/6)
## Time frame
times      <- seq(0, 30, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)

## Plot
par(mfrow=c(1,2))
matplot(x = times, y = out$I, type = "l",
        xlab = "Time (days)", ylab = "Population", main = "beta = 0.483, gamma = 0.167",
        lwd = 2, lty = 1, bty = "l", col = "red")
plot(SIR_Cases$Date, SIR_Cases$Total, type="l")

library(fitdistrplus)
plotdist(SIR_Cases$Total)
fe <- fitdistr(SIR_Cases$Total, "exponential")
fe

UK.Regions <- readOGR("Region_(December_2015)_Boundaries.shp")
timecases <- merge(UK.Regions, SIR_Cases2, by.x="rgn17nm", by.y="Region")

time <- c(as.character(seq(from = 43891, to = 43921, by = 1)))

plot(UK.Regions)
casetimes <- tm_shape(timecases) + 
  tm_polygons(c("43891", "43892" ,"43893", "43894", "43895", "43896",
                "43897", "43898" ,"43899" ,"43900", "43901" ,"43902",
                "43903", "43904", "43905" ,"43906" ,"43907", "43908",
                "43909", "43910", "43911", "43912", "43913", "43914",
                "43915", "43916", "43917", "43918" ,"43919", "43920",
                "43921"), palette="BuPu", title= "UK Cases by Region", style = "log10_pretty")+
  tm_facets(ncol = 8, nrow = 5, sync= TRUE, free.scales= FALSE)
casetimes
tmap_animation(casetimes, delay=40)


rate <- out$I/55797178

NE.E <- rate*2657909
NW.E <- rate*7292093
YH.E <- rate*5479615
EM.E <- rate*4804149
WM.E <- rate*5900757
EE.E <- rate*6021214
LN.E <- rate*8908081
SE.E <- rate*9133625
SW.E <- rate*5599735
E <- rbind(NE.E, NW.E, YH.E, EM.E, WM.E, EE.E, LN.E, SE.E, SW.E)

library(nimble)

beta0 <- -0.3291
beta1 <- -0.8239
taun <- 0.06829
sigmaa <- 0.05726
taua <- 1/sigmaa
sigman <- 1/taun
Z <- matrix(c(0,	7,	2,	1,	2,	1,	4,	0,	3,
      1,	6,	3,	2,	4,	0,	12,	1,	8,
      0,	7,	5,	7,	1,	7,	6,	9,	10,
      1,	2,	2,	2,	3,	7,	18,	7,	6,
      2,	10,	1,	3,	6,	1,	14,	5,	3,
      1,	2,	1,	7,	2,	5,	24,	16,	11,
      2,	9,	5,	5,	6,	3,	15,	4,	5,
      0,	5,	3,	1,	4,	2,	22,	10,	3,
      3,	7,	8,	10,	8,	5,	52,	24,	5,
      4,	15,	18,	12,	17,	16,	92,	36,	11,
      1,	17,	10,	25,	29,	14,	183,	50,	14,
      2,	40,	14,	17,	47,	18,	164,	86,	18,
      6,	43,	12,	22,	39,	37,	134,	54,	30,
      6,	25,	12,	19,	31,	16,	150,	41,	10,
      10,	24,	9,	26,	44,	35,	165,	53,	17,
      7,	56,	28,	34,	56,	24,	233,	78,	18,
      16,	55,	31,	33,	69,	38,	311,	89,	21,
      14,	84,	47,	48,	99,	74,	355,	122,	44,
      19,	98,	55,	66,	122,	57,	332,	127,	21,
      26,	82,	59,	53,	151,	70,	426,	127,	43,
      23,	115,	77,	69,	125,	78,	350,	128,	42,
      20,	117,	94,	99,	111,	90,	438,	170,	44,
      58,	175,	144,	117,	239,	194,	689,	282,	62,
      76,	255,	157,	135,	250,	165,	635,	238,	68,
      76,	255,	151,	116,	240,	188,	761,	307,	104,
      97,	358,	178,	156,	314,	215,	766,	320,	119,
      102,	333,	153,	137,	279,	243,	818,	330,	129,
      113,	322,	206,	156,	323,	183,	576,	340,	102,
      107,	359,	228,	128,	365,	209,	596,	296,	101,
      176,	507,	285,	206,	431,	301,	867,	478,	164,
      210,	554,	319,	276,	415,	322,	935,	469,	141), nrow = 9) 

X = c(20.01604, 18.85756, 18.84762, 19.6516, 18.73255, 20.56904, 12.14083, 19.61123, 22.37836)
N=  c(2657909, 7292093, 5479615, 4804149, 5900757, 6021214,8908081, 9133625, 5599735,2657909)

num = c(2,4,3,5,4,3,2,5,2)
adj = c(3,2,5,4,3,1,4,2,1,6,5,2,8,3,8,9,2,4,7,8,4,6,8,7,6,9,5,4,8,5)
SumNumNeigh = 30
weights <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
length(weights)

M <- 9
T <- 31

alpha1 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
e <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

for (i in 2:T) {
  e[i] <- rnorm(1, 0, sigmaa)
  print(e[i])
  alpha1[i] <- alpha1[i-1] + e[i]
}

logmu <- matrix(0,9,31)
mu <- matrix(0,9,31)
P <- matrix(0,9,31)
phi <- c(0,0,0,0,0,0,0,0,0)


for (i in 1:M){
  phi[i] <- dcar_normal(rowSums(E), adj, weights, num, taun)
}

phi

for (i in 1:M) {
  for (j in 1:T){
    logmu[i,j] <- log(E[i,j]) + beta0 + beta1*X[i] + phi[i] + alpha1[j]
    mu[i,j] <- exp(logmu[i,j])
  }
}

SIR_Cases3 <- read_excel("SIR Dataset.xlsx",sheet = "PosteriorT")
ts_plot(SIR_Cases3, title = "UK Cases by Region March 2020", Ytitle = "Number of Cases")

SIR_Cases4 <- read_excel("SIR Dataset.xlsx",sheet = "Posterior")
post.timecases <- merge(UK.Regions, SIR_Cases4, by.x="rgn15nm", by.y="Region")

post.casetimes <- tm_shape(post.timecases) + 
  tm_polygons(c("43891", "43892" ,"43893", "43894", "43895", "43896",
                "43897", "43898" ,"43899" ,"43900", "43901" ,"43902",
                "43903", "43904", "43905" ,"43906" ,"43907", "43908",
                "43909", "43910", "43911", "43912", "43913", "43914",
                "43915", "43916", "43917", "43918" ,"43919", "43920",
                "43921"), palette="BuPu", title= "UK Cases by Region", style = "fixed", breaks = c(0,3,10,32,100,316,1000,1200))+
  tm_facets(ncol = 8, nrow = 5, sync= TRUE, free.scales= FALSE)
post.casetimes

post.casetimes2 <- tm_shape(post.timecases) + 
  tm_polygons(c("43891", "43892" ,"43893", "43894", "43895", "43896",
                "43897", "43898" ,"43899" ,"43900", "43901" ,"43902",
                "43903", "43904", "43905" ,"43906" ,"43907", "43908",
                "43909", "43910", "43911", "43912", "43913", "43914",
                "43915", "43916", "43917", "43918" ,"43919", "43920",
                "43921"), palette="BuPu", title= "UK Cases by Region", style = "log10_pretty")+
  tm_facets(ncol = 1, nrow = 1, sync= TRUE, free.scales= FALSE)
tmap_animation(post.casetimes2, delay=40)

SIR_R <- read_excel("SIR Dataset.xlsx",sheet = "PosteriorRT")
ts_plot(SIR_R, title = "UK Removed by Region March 2020", Ytitle = "Number of Cases")
SIR_S <- read_excel("SIR Dataset.xlsx",sheet = "PosteriorST")
ts_plot(SIR_R, title = "UK Susceptible by Region March 2020", Ytitle = "Number of Cases")
