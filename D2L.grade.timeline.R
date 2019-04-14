#########################################################
#
# Author: Thomas Gredig
# Date: 2016-08-22
#
# evaluate change in grades 
# over time on Beachboard
#
#########################################################

library(ggplot2)
source('config.R')

result = data.frame()
for(fname in file.list) {
  read.csv(fname) -> data
  names(data)
  which(names(data)=='Calculated.Final.Grade.Numerator' |
        names(data)=='Calculated.Final.Grade.Denominator') -> q
  d <- data[,c(1,2,3,q)]
  names(d)[4] = 'points'
  names(d)[5] = 'total'
  d$grade = d$points / max(d$points)
  
  # get date
  regexpr('xport(.*)\\.',fname) -> m
  regmatches(fname, m) -> m2
  substr(m2, 7, nchar(m2)-1) -> full.date
  substr(full.date, 6,10) -> short.date
  d$date = short.date
  d$full.date = full.date
  print(full.date)
  rbind(result,d[,c(1,2,3,7,6)]) -> result
}

result$data = factor(result$date)
result$name = paste(result$First.Name,result$Last.Name)
# ggplot(result, aes(name, grade, color=date)) + geom_point()

# creating title
as.Date(result$date,"%m-%d") -> result$day
result$lname = paste0(substr(result$First.Name,1,4),substr(result$Last.Name,1,4))
result$lname2 = paste0(substr(result$First.Name,1,5),substr(result$Last.Name,1,1))


ggplot(result, aes(day, grade)) + geom_line() + facet_wrap( ~ lname, ncol=10) +
  #scale_y_continuous(limits = c(0.5,1.0), breaks=c(0.6,0.8,1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(FILE.grade.timeline, width=12, height=8)


result$grade.letter = cut(result$grade, breaks=c(0,0.6,0.7,0.8,0.9,1.0),labels=c('F','D','C','B','A'))

r = subset(result, day==max(result$day))
top20 = ceiling(nrow(r)*0.2)
middle60 = ceiling(nrow(r)*0.8)
r1 = r[order(r$grade, decreasing=TRUE)[1:top20],'OrgDefinedId']
r2 = r[order(r$grade, decreasing=TRUE)[(top20+1):middle60],'OrgDefinedId']
r3 = r[order(r$grade, decreasing=TRUE)[(middle60+1):nrow(r)],'OrgDefinedId']


# Top 20%
ggplot(subset(result, OrgDefinedId %in% r1), aes(day, grade)) + 
  geom_rect(aes(ymin=0.8,
                ymax = 1,
                xmin = min(result$day),
                xmax = max(result$day), fill = grade.letter, alpha = 0.5)) +
  geom_line() + facet_wrap( ~ lname, ncol=6) +
  scale_y_continuous(limits = c(0.5,1.0), breaks=c(0.6,0.8,1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none')
ggsave(FILE.grade.timeline.top20, width=12, height=8)

# Middle 60%
ggplot(subset(result, OrgDefinedId %in% r2), aes(day, grade)) + 
  geom_rect(aes(ymin=0.7,
                ymax = 0.9,
                xmin = min(result$day),
                xmax = max(result$day), fill = grade.letter, alpha = 0.5)) +
  geom_line() + facet_wrap( ~ lname, ncol=6) +
  scale_y_continuous(limits = c(0.3,1.0), breaks=c(0.6,0.8,1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none')
ggsave(FILE.grade.timeline.middle, width=12, height=8)

# Bottom 20%
ggplot(subset(result, OrgDefinedId %in% r3), aes(day, grade)) +  
  geom_rect(aes(ymin=0,
              ymax = 0.6,
              xmin = min(result$day),
              xmax = max(result$day), fill = 'orange', alpha = 0.5)) +
  geom_line() + facet_wrap( ~ lname, ncol=6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none' )

ggsave(FILE.grade.timeline.bottom20, width=12, height=8)

