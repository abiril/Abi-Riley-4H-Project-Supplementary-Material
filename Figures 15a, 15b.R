library(SimInf)
n <- 1000
u0 <- data.frame(S = rep(999,n), I = rep(1,n), R = rep(0,n))
tspan <- seq(from = 1, to = 80, by = 1)
model <- SIR(u0 = u0, tspan = tspan, beta = 1, gamma = 0.5)
set.seed(123)
set_num_threads(1)
result <- run(model=model)
result
plot(result, main="beta = 1, gamma = 0.5")

