## Function to check package dependencies and throw an informative error if need be
check_dependencies <- function() {
  requiredPackages <- c('rFIA', 'stars', 'yaImpute', 'here')
  missing <- !c(requiredPackages %in% installed.packages())
  missing <- requiredPackages[missing]
  if (length(missing) > 0) {
    stop(paste0('Missing packages: ', paste(missing, collapse = ', ')), '. Please install using "install.packages" and try again.')
  } else {
    cat('Good to go!\n')
  }
}
