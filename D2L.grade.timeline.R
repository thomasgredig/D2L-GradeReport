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

result = read.grades(file.list)
tail(result)

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
ggsave(FILE.grade.timeline.top20, width=6, height=4)

# Middle 60%
ggplot(subset(result, OrgDefinedId %in% r2), aes(day, grade)) +
  geom_rect(aes(ymin=0.7,
                ymax = 0.9,
                xmin = min(result$day),
                xmax = max(result$day), fill = grade.letter, alpha = 0.5)) +
  geom_line() + facet_wrap( ~ lname, ncol=6) +
  scale_y_continuous(limits = c(0.3,1.0), breaks=c(0.6,0.8,1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none', strip.text.x = element_text(size = 6))
ggsave(FILE.grade.timeline.middle, width=6, height=12, dpi=220)

# Bottom 20%
ggplot(subset(result, OrgDefinedId %in% r3), aes(day, grade)) +
  geom_rect(aes(ymin=0,
              ymax = 0.6,
              xmin = min(result$day),
              xmax = max(result$day), fill = grade.letter, alpha = 0.5)) +
  geom_line() + facet_wrap( ~ lname, ncol=6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none' )

ggsave(FILE.grade.timeline.bottom20, width=6, height=4)

# make graphs for each student
# convert all factors to strings
i <- sapply(result, is.factor)
result[i] <- lapply(result[i], as.character)

result$OrgDefinedId = factor(result$OrgDefinedId)
result$data = as.Date(result$date, format='%m-%d')

#studentID = levels(result$OrgDefinedId)[result$OrgDefinedId[13]]
result$filename = paste0(substr(result$Last.Name, 1,3), substr(result$OrgDefinedId,8,10),'.png')
for(studentID in levels(result$OrgDefinedId)) {
  r1 = subset(result, result$OrgDefinedId == studentID)
  ggplot(r1, aes(data, grade*100)) +
    scale_y_continuous(limits=c(0,100)) +
    ylab('Grade (%)') + xlab('date') +
    geom_rect( xmin = -Inf, xmax = +Inf, ymin =  0, ymax = 60, col='transparent', fill='red',
             alpha = 0.1) +
    geom_rect( xmin = -Inf, xmax = +Inf, ymin =  60, ymax = 70, col='transparent', fill='orange',
               alpha = 0.1) +
    geom_rect( xmin = -Inf, xmax = +Inf, ymin =  70, ymax = 80, col='transparent', fill='yellow',
               alpha = 0.1) +
    geom_rect( xmin = -Inf, xmax = +Inf, ymin =  80, ymax = 90, col='transparent', fill='lightgreen',
               alpha = 0.1) +
    geom_rect( xmin = -Inf, xmax = +Inf, ymin =  90, ymax = 100, col='transparent', fill='green',
               alpha = 0.1) +
    geom_point(col='black', size=6) +
    geom_point(col='darkgreen', size=5) + geom_line() +
    geom_point(data = subset(r1, grade<0.9), col='lightgreen', size=5) +
    geom_point(data = subset(r1, grade<0.8), col='yellow', size=5) +
    geom_point(data = subset(r1, grade<0.7), col='orange', size=5) +
    geom_point(data = subset(r1, grade<0.6), col='red', size=5) +
    ggtitle(paste('Sem: ',r1$First.Name[1],r1$Last.Name[1])) +
    theme_bw(base_size = 18)
  
  ggsave(file.path(path.results,r1$filename[1]), width=6, height=4, dpi=220)
}



## make a points plot
str(result)
ggplot(data = result, aes(as.numeric(days), points, color=OrgDefinedId)) + 
  geom_line() +
  geom_line(data = subset(result, OrgDefinedId == studentID), col='red', size=3) + 
  theme_bw() +
  xlab('days') + 
  theme(legend.position = 'none')

d = data.frame(
  OrgDefinedId = result$OrgDefinedId,
  `Grade Summary Image Text Grade` = paste0(pth, result$filename),
  `End-of-Line Indicator` = '#'
)
write.csv(d, file=file.path(path.source,'output-GRADEimage.csv'), row.names = FALSE)
pth
