#########################################################
#
# Author: Thomas Gredig
# Date: 2016-08-22
#
# Quick Student Report from Gradebook
#
#########################################################

# use full or a portion of the student's lastname
student.LastName = 'P'

# loading libraries
library(ggplot2)
library(knitr)
source('config.R')


# load grades and data
data = read.csv(file.latest)
data[,  colSums(is.na(data)) != nrow(data)] -> data
titles = names(data)

grep('.Subtotal.Numerator$',titles) -> q2  
# all points grade not in category
q3 = which(grepl('.Points.Grade..Numeric.', titles) &
             !(grepl('.Category.', titles))) 
if (only.SUBCATs) {q3 = c()}
mydata <- data[,c(q2,q3)]
gsub('.Points.Grade.*','',names(mydata)) -> names(mydata)
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
q[,2:NN] = sapply(q[,2:NN], function(x) as.numeric(as.character(x)))

# output table
kable(q, digits=2)


