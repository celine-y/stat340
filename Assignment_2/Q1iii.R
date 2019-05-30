dataiii <- read.csv(file="A2_datasetiii.csv", header=TRUE)

data = sort(dataiii$x)
hist(data, 10)
den <- density(data)
plot(den)

n = length(data)
df = 1
ncp = mean(data)

discrepancy = function(data, df, ncp){
  n = length(data)
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  
  Fx = pt(data, df, ncp)
  U = abs(Fu-Fx)
  L = abs(Fl-Fx)
  
  d = max(c(U, L))
  return(d)
}

d = discrepancy(data, df, ncp)
d

KS = function(n, d, m, df, ncp){
  ddots = NULL
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  
  for(i in 1:m){
    newdata = sort(rt(n, df, ncp))
    new_ncp = mean(newdata)
    Fx = pt(newdata, df, new_ncp)
    U = abs(Fu - Fx)
    L = abs(Fl - Fx)
    ddot = max(U, L)
    ddots = c(ddots, ddot)
  }
  
  p = length(ddots[ddots > d])/m
  return(p)
}

p = KS(n, d, 1000, df, ncp)
p