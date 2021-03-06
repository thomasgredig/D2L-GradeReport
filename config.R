# Configuration File for D2L Grade Analysis
# -----------------------------------------

# PATHS
# -----
for(q1 in dir('R', pattern='[^(main)].*\\.R$')) { source(file.path('R',q1)) }
# path with D2L Grades - can be overwritten in myConfig.R
path.source = '.'
# path for output graphs
path.results = '.'
# include subcategories only, or also main grade items
only.SUBCATs = FALSE
if(file.exists('myConfig.R')) {
  source('myConfig.R')  # to overwrite certain files
}

# this part does not need configuration

# FILENAMES
# ---------
FILE.CORR = file.path(path.results,'D2L.grade.analysis.CORRELATION.png')
FILE.CORR.table = file.path(path.results,'D2L.grade.analysis.CORRELATION.csv')
FILE.grade.timeline = file.path(path.results,'D2L.grade.timeline.FACETS.png')
FILE.grade.timeline.top20 = file.path(path.results,'D2L.grade.timeline.FACETS.top20.png')
FILE.grade.timeline.middle = file.path(path.results,'D2L.grade.timeline.FACETS.middle.png')
FILE.grade.timeline.bottom20 = file.path(path.results,'D2L.grade.timeline.FACETS.bottom20.png')


file.list = find.fileList(path.source)
file.latest = get.latestFile(file.list)
