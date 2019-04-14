# Configuration File for D2L Grade Analysis
# -----------------------------------------

# PATHS
# -----

# path with D2L Grades - can be overwritten in myConfig.R
path.source = '.'
# path for output graphs
path.results = '.'
# include subcategories only, or also main grade items
only.SUBCATs = FALSE
source('myConfig.R')  # to overwrite certain files


# this part does not need configuration

# FILENAMES
# ---------
FILE.CORR = file.path(path.results,'D2L.grade.analysis.CORRELATION.png')
FILE.CORR.table = file.path(path.results,'D2L.grade.analysis.CORRELATION.csv')
FILE.grade.timeline = file.path(path.results,'D2L.grade.timeline.FACETS.png')
FILE.grade.timeline.top20 = file.path(path.results,'D2L.grade.timeline.FACETS.top20.png')
FILE.grade.timeline.middle = file.path(path.results,'D2L.grade.timeline.FACETS.middle.png')
FILE.grade.timeline.bottom20 = file.path(path.results,'D2L.grade.timeline.FACETS.bottom20.png')

# make a list of gradebook files, including the latest file
file.list = file.path(path.source,dir(path.source,pattern='-\\d{2}\\.csv$'))
if (length(file.list)==0) {
  print("ERROR: Grade book file list is empty.")
}
file.latest = file.list[which.max(file.info(file.list)$mtime)]

