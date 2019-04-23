## convert Desire2Learn headers
## (c) 2015-2019 Thomas Gredig

library(corrplot)
source('config.R')

fname = file.latest
data = read.csv(fname)
q=names(data)

strsplit(q,'.Points.Grade') -> a
sapply(a, "[", 1) -> titles
names(data) <- titles

sapply(a, "[", 2) -> pts
numCols = !is.na(pts)

# find points
strsplit(pts,'.Weight.') -> a
gsub('..Numeric.MaxPoints.','',sapply(a, "[", 1)) -> pts

# graph correlations
q = data[,numCols]
# remove unpopulated columns
q[is.na(q)] <- 0
q[,as.vector(which(apply(q, 2, sum)==0))] <- NULL

M <- cor(q)
corrplot(M, method = "ellipse") #, order="hclust")

# save file:
print(paste("Saving: ",gsub('.csv$','.png', fname)))
png(
  gsub('.csv$','.png', fname),
  width     = 20,
  height    = 16,
  units     = "cm",
  res       = 300,
  pointsize = 4
)

corrplot(M, method = "ellipse")
dev.off()
