cars = c(0, 1, 2, 3)
shifts = c(5, 14, 5, 1)

rate = (1/25)*(t(cars)%*%shifts)
prob = ppois(cars, lambda = rate)
ei = 25*prob

print(rate)
print(prob)
print(ei)

d = sum(((ei-shifts)^2)/ei)

print(d)
p = 1- pchisq(d, 3)
print(p)

print(paste("Since p =", p, ", we have no evidence to reject H0"))