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
## Last modified: 9 December 2020 - Hunter Stanke
##
##==============================================================================
##==============================================================================


## SETUP -----------------------------------------------------------------------

## Load a handful of packages 
library(here)
library(dplyr)

## Loading our functions that will be called below 
invisible({
  sapply(list.files(here('src/FIA'), full.names = TRUE), source)
})

## Check that we have all packages that we need
check_dependencies()

## How many cores do you want to use? 
## Check how many you have with parallel::detectCores(logical = TRUE) 
## DO NOT USE everything you have, e.g., if you have 4, use 3
## If you run into memory issues (R crashes), try cores = 1
cores = 10




## PRE-PROCESS FIA DATA --------------------------------------------------------

## Download FIA data for OR and WA from the FIA Datamart
download_FIA(states = c('OR', 'WA'),
             dirFIA = here('data/FIA/'),
             force = FALSE)

## Select the FIA plots associated with all 'current area' inventories
select_FIA_area_plots(dirFIA = here('data/FIA/'),
                      dirResults = here('results/FIA/'),
                      cores = cores)

## Do a 'fuzzy' spatial intersection of plot locations with auxiliary rasters
fuzzy_plot_intersection(dirGIS = here('data/GIS/'),
                        dirResults = here('results/FIA/'),
                        cores = cores)

## Assign FIA plots to structural classes
classify_plot_structure(dirFIA = here::here('data/FIA/'),
                        dirFVS = here::here('data/FVS/'),
                        dirRefCon = here::here('data/refCon/'),
                        dirResults = here::here('results/FIA/'), 
                        mapStems = FALSE,
                        cores = cores)




## ESTIMATE LAND AREA ----------------------------------------------------------

## Estimate total and % forest area by S-class within classified domains (strata)
estimate_sclass_area(dirFIA = here::here('data/FIA/'),
                     dirResults = here::here('results/FIA/'),
                     cores = cores)




## ESTIMATE RESTORATION NEED ---------------------------------------------------

## Estimate total and % forest area by S-class within classified domains (strata)
estimate_rest_need(dirResults = here::here('results/FIA/'),
                   dirRefCon = here::here('data/refCon/'),
                   cores = cores)
