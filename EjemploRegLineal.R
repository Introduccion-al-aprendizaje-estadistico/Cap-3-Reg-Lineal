data<-MyData <- read.csv(file="Advertising.csv", header=TRUE, sep=",")
data

lm(formula = data$sales ~ data$TV, data = data)
