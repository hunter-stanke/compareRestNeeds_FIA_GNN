##=====================================================
##=====================================================
##
## Function to estimate area forested for all FIA plots 
## associated with current area inventories. Save
## spatial info and design info associated w/ each plot.
## Wrapper around rFIA's area function
##
##
## Created:       15 October 2020 - Hunter Stanke
## Last modified: 7 December 2020 - Hunter Stanke
##
##====================================================
##====================================================

## Function to subset FIA plots associated with 'current area' inventories
## -----------------------------------------------------------------------------
## dirFIA (character):    directory where FIA data is saved
## dirSave (character):   directory where results will be saved
## nCores (integer):      How many cores to use?
select_FIA_area_plots <- function(dirFIA = here::here('data/FIA/'), 
                                  dirResults = here::here('results/FIA/'),
                                  cores = 1) {
  
  ## Set up a remote database
  db <- rFIA::readFIA(dirFIA, 
                      states = c('OR', 'WA'),
                      inMemory = FALSE, 
                      nCores = cores)
  
  ## All plots associated w/ current area inventories
  plt <- rFIA::area(db, grpBy = c(INVYR, STATECD, LAT, LON),
                    byPlot = TRUE, nCores = cores)
  
  
  ## Save results
  write.csv(plt, paste0(dirResults, 'fiaPlts.csv'), row.names = FALSE)
  
  ## Return message in run_this
  cat('FIA data subset complete ...\n')

}


