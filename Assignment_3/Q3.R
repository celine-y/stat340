F_inv = function(u){
  #denom = (1-(1/u)^(1/4))^(1/3)
  fourth = 1-(1/u)^(1/4)
  third = (abs(fourth))^(1/3)
  return (third)
}

n = 1000
u = runif(n)
# Generate random variables
o_data = F_inv(u)

print("H0: data follows specified distribution")
print("Ha: data doesn't follow specified distribution")

F = function(x){
  inside = 1+(2/x)^3
  outside = inside^-4
  return(outside)
}

discrep = function(data, n){
  data = sort(data)
  
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  Fx = F(data)
  
  U = abs(Fu - Fx)
  L = abs(Fl - Fx)
  
  diff = c(U, L)
  return (max(diff))
}

KS = function(d, n, m){
  ddots = NULL
  
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  
  for(i in 1:m){
    new_u = runif(n)
    new_data = sort(F_inv(new_u))
    Fx = F(new_data)
    
    U = abs(Fu - Fx)
    L = abs(Fl - Fx)
    ddot = max(U, L)
    ddots = c(ddots, ddot)
  }
  
  p = length(ddots[ddots > d])/m
}

d = discrep(o_data, n)
p = KS(d, n, 10000)

print(p)
print("Therefore, accept H0")