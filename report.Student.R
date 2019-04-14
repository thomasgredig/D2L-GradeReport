## Student Report

# how is one particular student performing in this class

library(ggplot2)
library(knitr)
file.grades = file.latest
student.LastName = 'Palm'

# load grades and data
data = read.csv(file.grades)
data[,  colSums(is.na(data)) != nrow(data)] -> data
titles = names(data)
grep('.Subtotal.Numer',titles) -> q2
mydata <- data[,q2]
gsub('.Subtotal.Numerator','',names(mydata)) -> names(mydata)

grep(student.LastName,data$Last.Name) -> Student.column
mydata[Student.column,]

paste(data[Student.column,2], data[Student.column,3]) -> Student.name

rbind(
  data.frame(type=Student.name,mydata[Student.column,]),
  cbind(type='mean',rbind(apply(mydata, 2, mean))),
  cbind(type='sd',rbind(apply(mydata, 2, sd))),
  cbind(type='min',rbind(apply(mydata, 2, min))),
  cbind(type='max',rbind(apply(mydata, 2, max)))
) -> q
q = as.data.frame(q)

NN = ncol(q)
# ars[, 1:2] <- sapply(cars[, 1:2], as.factor)
q[,2:NN] = sapply(q[,2:NN], function(x) as.numeric(as.character(x)))

kable(q, digits=2)


