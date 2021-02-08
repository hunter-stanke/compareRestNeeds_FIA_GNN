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
                      nCores = cores)
  
  ## Current area plots
  aPlts <- rFIA::area(db, grpBy = PREV_PLT_CN, byPlot = TRUE, nCores = cores)
  
  ## Area Change plots
  acPlts <- rFIA::areaChange(db, byPlot = TRUE, nCores = cores) %>%
    dplyr::left_join(dplyr::select(db$PLOT, PREV_PLT_CN, CN), by= c('PLT_CN' = 'CN'))
  
  ## All unique plot IDs
  allPlts <- unique(c(aPlts$PLT_CN, aPlts$PREV_PLT_CN, acPlts$PLT_CN, acPlts$PREV_PLT_CN))
  
  ## Convert back to dataframe and add some info
  allPlts  <- data.frame(PLT_CN = allPlts) %>%
    dplyr::left_join(dplyr::select(db$PLOT, PLT_CN = CN, INVYR, STATECD, 
                            LAT, LON, UNITCD, COUNTYCD, PLOT,
                            PLOT_STATUS_CD), by = 'PLT_CN') %>%
    dplyr::mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
    dplyr::select(PLT_CN, pltID, INVYR, STATECD, LAT, LON, PLOT_STATUS_CD)
  
  
  ## Save results
  write.csv(allPlts, paste0(dirResults, 'prep/fiaPlts.csv'), row.names = FALSE)
  
  ## Return message in run_this
  cat('FIA data subset complete ...\n')

}


