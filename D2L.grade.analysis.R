library(ggplot2)

file.grades = 'grades.csv'
data = read.csv(file.grades)

# remove all NA only columns
data[,  colSums(is.na(data)) != nrow(data)] -> data

titles = names(data)

grep('.Subtotal.Numer',titles) -> q2
mydata <- data[,q2]
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
cor.prob(mydata)

# "flatten" that table
flattenSquareMatrix(cor.prob(mydata))

# plot the data
library(PerformanceAnalytics)
chart.Correlation(mydata)

strsplit(titles,'.Points.Grade.') -> q
which(unlist(lapply(q, length))==2) -> p

unlist(lapply(q,'[[',1)) -> title.short
gsub('\\.',' ',title.short) -> title.short    # replace .

p[1]
title.short[p[1]]
hist(data[,p[1]])

data[,  colSums(is.na(data)) != nrow(data)] -> t


for (i in 1:length(p)) {
  #hist(data[,p[i]], main = title.short[p[i]])
  df = data.frame(lastname = data[2],
                  firstname = data[3],
                  x = data[,p[i]])
  df = na.omit(df)
  ggplot(df, aes(x)) + geom_histogram(binwidth=max(df$x)/10, col='red', fill='orange') +
    ggtitle(paste(title.short[p[i]],', N=',
                  nrow(df),', mean=',
                  signif(mean(df$x),digits=3),', sd=',
                  signif(sd(df$x), digits=3)))
}

