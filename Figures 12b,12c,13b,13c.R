## https://archives.aidanfindlater.com/blog/2010/04/20/the-basic-sir-model-in-r/
## Load deSolve package
library(deSolve)

## Create an SIR function
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
init       <- c(S = 990, I = 1, R = 0)
## beta: infection parameter; gamma: recovery parameter
parameters <- c(beta = 1, gamma = 0.5)
  ## Time frame
times      <- seq(0, 80, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)

## Plot
matplot(x = times, y = out, type = "l",
        xlab = "Time (days)", ylab = "Population", main = "beta = 1, gamma = 0.5",
        lwd = 2, lty = 1, bty = "l", col = c("red","blue","green"))

## Add legend
legend(40, 750, c("Susceptible", "Infected", "Recovered"), pch = 1, col = c("red","blue","green"), bty = "n")



## Create an SI function
si <- function(time2, state2, parameters2) {
  
  with(as.list(c(state2, parameters2)), {
    N = S + I
    dS <- (-(beta/N) * S * I) + (gamma * I)
    dI <- ((beta/N) * S * I) - (gamma * I)
    
    return(list(c(dS, dI)))
  })
}

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init2       <- c(S = 999, I = 1)
## beta: infection parameter; gamma: recovery parameter
parameters2 <- c(beta = 0.5, gamma = 0.1)
## Time frame
times2      <- seq(0, 80, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out2 <- ode(y = init2, times = times2, func = si, parms = parameters2)
## change to data frame
out2 <- as.data.frame(out2)
## Delete time variable
out2$time <- NULL
## Show data
head(out2, 10)

## Plot
matplot(x = times, y = out2, type = "l",
        xlab = "Time (days)", ylab = "Population", main = "beta = 0.5, gamma = 0.1",
        lwd = 2, lty = 1, bty = "l", col = c("red","blue"))

## Add legend
legend(40, 750, c("Susceptible", "Infected"), pch = 1, col = c("red", "blue"), bty = "n")


