# Assignment 2, Question 1
datai <- read.csv(file="A2_dataseti.csv", header=TRUE)
dataiii <- read.csv(file="A2_datasetiii.csv", header=TRUE)

discrepancy = function(cdf_x, data){
  # cdf_x = CDF function
  # data = dataset
  n = length(data)
  
  F_u = c(1:n)/n
  F_l = F_u - 1/n
  Fu_diff = abs(cdf_x(data) - F_u)
  Fl_diff = abs(cdf_x(data) - F_l)
  
  diff = c(Fu_diff, Fl_diff)
  return (max(diff))
}

# Code specifically for dataset i
hist(datai$x)
xs = datai$x
n = length(xs)
l_est = 1/mean(xs)
# Predicted Distribution
F_x = function(x){
  return(pexp(x, l_est))
}

d = discrepancy(F_x, xs)
d

KS = function(d, n, m, l) {
  # d = discrepancy
  # n = sample size
  # m = number of iterations
  # l = lambda
  ddots = NULL
  
  F_u = c(1:n)/n
  F_l = F_u - 1/n
  
  for(i in 1:m){
    newdata = sort(rexp(n,l))
    new_l = 1/mean(newdata)
    F = pexp(newdata, new_l)
    U = abs(F_u-F)
    L = abs(F_l-F)
    ddot = max(U, L)
    ddots = c(ddots, ddot)
  }
  
  # p-value
  print(ddots)
  p = length(ddots[ddots > d])/m
  return(p)
}

p = KS(d, n, 10000, l_est)
p



# hist(dataiii$x)
