dataii <- read.csv(file="A2_datasetii.csv", header=TRUE)

# Code specifically for dataset ii
data = sort(dataii$x)
hist(data, 30)
den <- density(data)
plot(den)
n = length(data)

mu = (sum(log(data)))/n
sigma = (sum((log(data)-mu)^2))/n
mu
sigma

discrepancy = function(data, mu, sigma){
  n = length(data)
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  
  Fx = plnorm(data, mu, sigma)
  U = abs(Fx - Fu)
  L = abs(Fx - Fl)
  d = max(U, L)
  return(d)
}

d = discrepancy(data, mu, sigma)
d

KS = function(d, n, m, mu, sigma) {
  ddots = NULL
  
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  
  for(i in 1:m) {
    newdata = sort(rlnorm(n, mu, sigma))
    new_mu = (sum(log(newdata)))/n
    new_sigma = (sum((log(newdata)-new_mu)^2))
    
    Fx = plnorm(newdata, new_mu, new_sigma)
    U = abs(Fx - Fu)
    L = abs(Fx - Fl)
    ddot = max(U, L)
    ddots = c(ddots, ddot)
  }
  
  p = length(ddots[ddots > d])/m
  return(p)
}

KS(d, n, 10000, mu, sigma)