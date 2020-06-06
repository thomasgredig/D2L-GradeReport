###################################
#
# plot timeline of grade from grades of Beachboard
#
###################################

library(ggplot2)
source('config.R')
file.list = dir(path.source)
result = data.frame()

for(filename in file.list) {
  print(filename)
  data <- read.csv(file.path(path.source, filename))
  
  d <- data.frame(StudentID = data$OrgDefinedId,
             Lastname = data$Last.Name,
             Firstname = data$First.Name,
             Grade = data$Calculated.Final.Grade.Numerator)
  unlist(strsplit(filename,'_')) -> m
  m[length(m)] -> str.date
  unlist(strsplit(str.date,'[.-]')) -> q
  csv.date = as.Date(paste(q[1],q[2],q[3],sep='-'))
  d$date = rep(csv.date, nrow(d))
  d$last2 = substr(d$StudentID,nchar(as.character(d$StudentID))-3,nchar(as.character(d$StudentID)))
  
  result = rbind(result, d)
}

ggplot(result, aes(y=Grade, x=date, group=Firstname, label=Firstname)) + 
  geom_line(aes(col=Firstname), size=1, alpha=0.4) + theme_bw()
