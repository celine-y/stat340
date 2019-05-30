dataiii <- read.csv(file="A2_datasetiii.csv", header=TRUE)

hist(dataiii$x, 20)
data = dataiii$x
l = mean(data)
l