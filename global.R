# Handle packages
source('Packages.R')

# Source data files
data_files <- list.files(path = 'data', full.names = TRUE)
lapply(X = data_files, FUN = function(x) {
  writeLines(paste0('...\t', x))
  source(x)
})
