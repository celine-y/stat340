#Question 1, Part B
ANR2 = function(n, m, p){
  #n = num of tosses
  #m = number of games
  #p = prob of flipping heads
  
  tot_rounds = 0
  #loop through number of games
  for (i in 1:m){
    #loop through number of tosses
    num_runs = 0
    sample = rbinom(n, 1, p)
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
n=10
m=5000
p=0.75
ANR2(n, m, p)
