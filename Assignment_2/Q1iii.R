dataiii <- read.csv(file="A2_datasetiii.csv", header=TRUE)

data = sort(dataiii$x)
hist(data, 10)

n = length(data)
mu = mean(data)
sigma = sd(data)

mu
sigma

discrepancy = function(data, mu, sigma){
  n = length(data)
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  
  Fx = pnorm(data, mu, sigma)
  U = abs(Fu-Fx)
  L = abs(Fl-Fx)
  
  d = max(c(U, L))
  return(d)
}

d = discrepancy(data, mu, sigma)
d

KS = function(n, d, m, mu, sigma){
  ddots = NULL
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  
  for(i in 1:m){
    newdata = sort(rnorm(n, mu, sigma))
    Fx = pnorm(newdata, mu, sigma)
    U = abs(Fu - Fx)
    L = abs(Fl - Fx)
    ddot = max(U, L)
    ddots = c(ddots, ddot)
  }
  
  p = length(ddots[ddots > d])/m
  return(p)
}

p = KS(n, d, 1000, mu, sigma)
p