# Assignment 2, Question 2
data = c(0.415, 0.417, 0.423, 0.478, 0.479,
        0.482, 0.523, 0.591, 0.717, 1.196,
        1.294, 1.527, 1.693, 2.418, 8.001)

n = length(data)

F_x <- function(x) {
  return (x/x+1)
}
f_x <- function(x) {
  return (1/(x+1)^2)
}
k = 10