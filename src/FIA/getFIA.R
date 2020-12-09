##=====================================================
##=====================================================
##
## Function to download FIA data for Oregon and 
## Washington if they are not already present in the 
## .data/FIA/ directory. Simply a wrapper around 
##
## Created:       15 October 2020 - Hunter Stanke
## Last modified: 27 October 2020 - Hunter Stanke
##
##====================================================
##====================================================

## Wrapper around getFIA, only downloads data when necessary 
## -----------------------------------------------------------------------------
## states (character): vector of state abbreviations (e.g., c('OR', 'WA'))
## dir (character):    data directory (e.g., './data/FIA/)
## force (logical):    if TRUE, download data for states regardless. Useful if
##                     we want to update our datasets
download_FIA <- function(states, dirFIA, force = FALSE) {
  
  ## Force uppercase
  states <- stringr::str_to_upper(states)
  
  ## Do we already have the data?  
  if (force == FALSE) {
    
    ## Check for the plot tables
    getThese <- !c(paste0(states, '_PLOT.csv') %in% list.files(dirFIA))
    
    ## Subset the states we need to download
    states <- states[getThese]
  }
  
  ## Download FIA data w/ getFIA
  if (length(states) > 0) {
    rFIA::getFIA(states, dirFIA, load = FALSE)
  }
  
  
  ## Return message in run_this
  cat('FIA data download complete ...\n')
  
}

