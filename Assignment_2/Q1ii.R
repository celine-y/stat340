dataii <- read.csv(file="A2_datasetii.csv", header=TRUE)

# Code specifically for dataset ii
data = sort(dataii$x)
hist(data)
n = length(data)

b = var(data)/mean(data)
a = mean(data)/b

b
a

discrepancy = function(data, a, b){
  n = length(data)
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  
  Fx = pgamma(data, shape=a, scale=b)
  U = abs(Fx - Fu)
  L = abs(Fx - Fl)
  d = max(U, L)
  return(d)
}

d = discrepancy(data, a, b)
d

KS = function(d, n, m, a, b) {
  ddots = NULL
  
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  
  for(i in 1:m) {
    newdata = sort(rgamma(n, shape=a, scale=b))
    Fx = sort(pgamma(newdata, shape=a, scale=b))
    U = abs(Fx - Fu)
    L = abs(Fx - Fl)
    ddot = max(U, L)
    ddots = c(ddots, ddot)
  }
  
  p = length(ddots[ddots > d])/m
  return(p)
}

p = KS(d, n, 10000, mu, sigma)
p