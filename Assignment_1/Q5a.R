# Question 5, Part A
rgam = function(m, l)
{
  n_gamma = 1
  # change the shape (m)
  for(i in 1:m){
    n_gamma = n_gamma * rgamma(1, 1/2, 1/2)
  }
  
  #change the rate
  n_gamma = n_gamma * (1/l)
  
  return(n_gamma)
} #end function

# Question 5, Part B
rex = function(l)
{
  gamm = rgam(1, l)
  return(gamm)
} #end function

# Question 5, Part C
rcs = function(w) {
  gamm = rgam(w/2, 1/2)
  return(gamm)
} #end function

#Example
rgam(1/2, 1/2)
rex(1)
rcs(2)