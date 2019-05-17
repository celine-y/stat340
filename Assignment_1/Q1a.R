#Question 1, Part A
ANR <- function(n, m)
{
  #n = num of tosses
  #m = number of games
  
  tot_rounds = 0
  #loop through number of games
  for (i in 1:m){
    #loop through number of tosses
    num_runs = 0
    sample = rbinom(n, 1, 0.5)
    #counts the number of runs in the sample
    past = NULL
    for (curr in sample){
      if (is.null(past)){
        past = curr
        num_runs = num_runs + 1
      } else if (past != curr) {
        past = curr
        num_runs = num_runs + 1
      }
    }
    tot_rounds = tot_rounds + num_runs
  }
  tot_rounds = tot_rounds/m
  print(tot_rounds)
} #end function

#Example
n = 10
m = 5000
ANR(n, m)