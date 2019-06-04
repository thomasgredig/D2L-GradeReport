# make a list of gradebook files, including the latest file
find.fileList <- function(path.source) {
  file.list = file.path(path.source,dir(path.source,pattern='-\\d{2}\\.csv$'))
  if (length(file.list)==0) {
    print("ERROR: Grade book file list is empty.")
  }
  file.list
}

get.latestFile <- function(file.list) {
  file.list[which.max(file.info(file.list)$mtime)]
}
