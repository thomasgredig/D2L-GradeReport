#########################################################
#
# Author: Thomas Gredig
# Date: 2016-08-22
#
# Gradebook correlations
#
#########################################################


# load libraries and configuration
library(ggplot2)
library(PerformanceAnalytics)
source('config.R')

# load the data
file.grades = file.latest
if (!file.exists(file.grades)) { print("ERROR: grade file not found.") }
data = read.csv(file.grades)

# remove all NA only columns
data[,  colSums(is.na(data)) != nrow(data)] -> data

titles = names(data)
titles

# find all categories
grep('.Subtotal.Numerator$',titles) -> q2  
# all points grade not in category
q3 = which(grepl('.Points.Grade..Numeric.', titles) &
  !(grepl('.Category.', titles))) 
if (only.SUBCATs) {q3 = c()}
mydata <- data[,c(q2,q3)]
gsub('.Points.Grade.*','',names(mydata)) -> names(mydata)
gsub('.Subtotal.Numerator','',names(mydata)) -> names(mydata)

cor(mydata)

## see http://www.gettinggeneticsdone.com/2012/08/more-on-exploring-correlations-in-r.html

## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

# correlation matrix with p-values
write.csv(data.frame(cor.prob(mydata)),
          file = FILE.CORR.table)

# "flatten" that table
flattenSquareMatrix(cor.prob(mydata))

# plot the data
png(FILE.CORR, width=2000, height=1600)
chart.Correlation(mydata)
dev.off()


