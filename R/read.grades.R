# reads all the grades from CSV files

read.grades <- function(file.list) {
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
  str(result)

  # creating title
  as.Date(result$date,"%m-%d") -> result$day
  result$lname = paste0(substr(result$First.Name,1,1),substr(result$Last.Name,1,5))
  result$lname2 = paste0(substr(result$First.Name,1,5),substr(result$Last.Name,1,1))
  
  result
}
