##==============================================================================
##==============================================================================
##
## Main script that will run FIA-based estimation routines for forest structural
## restoration need in OR and WA. The purpose of this script is to call 'helper'
## functions that handle individual components of the estimation process. That
## is, this script loads and runs the code in the 'src/FIA/' directory, saving
## results along the way. There is no need to run any code in the 'src/FIA/' 
## directory directly.
##
##
## Created:       8 December 2020 - Hunter Stanke
## Last modified: 8 December 2020 - Hunter Stanke
##
##==============================================================================
##==============================================================================




## SETUP -----------------------------------------------------------------------

## Load a handful of packages 
library(here)
library(dplyr)

## Check that we're in the right location, this should be the location
## where the FIA_GNN_RNA folder is unzipped on your machine
cat('Working directory:', here())

## Loading our functions that will be called below 
sapply(list.files(here('src/FIA/'), full.names = TRUE), source)

## Check that we have all packages that we need
check_dependencies()




## PRE-PROCESS FIA DATA --------------------------------------------------------

## Download FIA data for OR and WA from the FIA Datamart
download_FIA(states = c('OR', 'WA'),
             dirFIA = here('data/FIA/'),
             force = FALSE)

## Select the FIA plots associated with all 'current area' inventories
select_FIA_area_plots(dirFIA = here('data/FIA/'),
                      dirResults = here('results/'))



## ESTIMATE LAND AREA ----------------------------------------------------------




## ESTIMATE RESTORATION NEED ---------------------------------------------------
