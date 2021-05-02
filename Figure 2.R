linear <- function(d, n, s){
  for (i in 1:length(d)) {
    if (d[i] > 0) {
      v[i] <- n + s*d[i]
    }
    else {
      v[i] <- 0
    }
  }
  return(v)
}

exponential <- function(d, n, s, r){
  for (i in 1:length(d)) {
    if (d[i] > 0) {
      v[i] <- n + s*(1-exp(-r*d[i]))
    }
    else {
      v[i] <- 0
    }
  }
  return(v)
}

spherical <- function(d, n, s, r){
  for (i in 1:length(d)) {
    if ((1/r)< d[i]){
      v[i] <- n + s
    }
    if (d[i] > 0 & d[i] <= (1/r)){
      v[i] <- n + s*(((3*r*d[i])/2)-0.5*(r*d[i])**3)
    }
    print(v[i])
  }
  
  return(v)
}

gaussian <- function(d, n, s, r) {
  for (i in 1:length(d)) {
    if (d[i] > 0) {
      v[i] <- n + s*(1-exp(-(r**2)*(d[i]**2)))
    }
    else {
      v[i] <- 0
    }
  }
  return(v)
}

matern <- function(d, n, s, r, m){
  for (i in 1:length(d)) {
    if (d[i] > 0) {
      v[i] <- n + s*(1-(((2*sqrt(m)*d*r)**v)/((2**(m-1))*gamma(m)))*besselK(2*sqrt(m)*d*r, m, expon.scaled = FALSE))
    }
    else {
      v[i] <- 0
    }
  }
  return(v)  
}
  
  
d <- c(seq(0,30,0.1))
par(mfrow=c(1,1))
plot(d, linear(d,0.2,0.03), ylim=c(0,1.2), type="l", ylab="γ(d)")
lines(d, exponential(d,0.2,1,0.1), col="red")
lines(d, spherical(d,0.2,1,0.1), col="blue")
lines(d, gaussian(d,0.2,1,0.1), col="green")
legend(19,0.75, legend=c("Linear","Exponential","Spherical","Gaussian"), lty=1,col=c("black","red","blue","green"), cex=0.6)
