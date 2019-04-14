###################################
#
# plot timeline of grade from grades of Beachboard
#
###################################

library(ggplot2)

path = '/Volumes/class/Phys152 2016 Summer/Grades'
dir(path, pattern='csv$') -> file.list
result = data.frame()

for(i in 1:length(file.list)) {
  filename = file.list[i]
  data <- read.csv(file.path(path, filename))
  
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
