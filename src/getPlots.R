##=====================================================
##=====================================================
##
## Estimate area forested for all FIA plots associated
## with current area inventories in OR and WA. Save
## spatial info and design info associated w/ each plot
##
##
## Created:       15 October 2020 - Hunter Stanke
## Last modified: 27 October 2020 - Hunter Stanke
##
##====================================================
##====================================================


#===============================================================================
##  Set up your working directory and number of cores to use --------------------
##===============================================================================

library(rFIA)

## Set working directory
setwd('/home/hunter/Dropbox/departureR6/')

## How many cores do you want to use?
## Default to one
cores <- 4


##===============================================================================
##  Download FIA data -----------------------------------------------------------
##===============================================================================

###   ONLY RUN THIS ONCE ---
## Download data for all states
# getFIA(c('WA', 'OR'), dir = './FIA/',
#        load = FALSE)


##===============================================================================
##  Table modifications ---------------------------------------------------------
##===============================================================================

## None at this time, may have to work w/ canopy cover issues though



##===============================================================================
##  Set up 'remote' database ----------------------------------------------------
##===============================================================================
#db <- readFIA('./FIA/', inMemory = FALSE, nCores = cores)

## Hunter only - don't want to duplicate data
db <- readFIA('/home/hunter/FIA/', states = c('OR', 'WA'),
              inMemory = FALSE, nCores = cores)



##===============================================================================
##  Compute plot-level estimates ------------------------------------------------
##===============================================================================


## All live trees >= 1" DBH
plt <- area(db, grpBy = c(INVYR, STATECD, LAT, LON),
            byPlot = TRUE, nCores = cores)



##===============================================================================
##  Save results ----------------------------------------------------------------
##===============================================================================
write.csv(plt, './results/fiaPlts.csv', row.names = FALSE)
