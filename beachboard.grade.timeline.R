########################
#
# evaluate change in grades 
# over time on Beachboard
#
#######################
library(ggplot2)


# ENTER PATH WITH CSV files from Beachboard (Desire2Learn)
path = '/Volumes/class/COURSES/Phys152 2016 Summer/Grades'
path = '/Volumes/class/Phys152 2016 Fall/Grades'

dir(path)
dir(path, pattern='^PHYS.*\\.csv$') -> file.list

result = data.frame()
for(file.list.name in file.list) {
  fname <- file.path(path,file.list.name)
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
ggplot(result, aes(name, grade, color=date)) + geom_point()



as.Date(result$date,"%m-%d") -> result$day
result$lname = paste0(substr(result$First.Name,1,1),substr(result$Last.Name,1,4))
result$lname2 = paste0(substr(result$First.Name,1,4),substr(result$Last.Name,3,5))


#ggplot(result, aes(day, grade)) + geom_line() + facet_grid(. ~ lname)
ggplot(result, aes(day, grade)) + geom_line() + facet_wrap( ~ lname2, ncol=10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('student-progress_Fall206b.png', width=12, height=8)
