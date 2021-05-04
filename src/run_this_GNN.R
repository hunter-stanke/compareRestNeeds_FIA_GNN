##==============================================================================
##==============================================================================
##
## Main script that will run GNN-based estimation routines for forest structural
## restoration need in OR & WA. The purpose of this script is to call 'helper'
## functions that handle individual components of the estimation process. That
## is, this script loads and runs the code in the 'src/GNN/' directory, saving
## results along the way. There is no need to run any code in the 'src/GNN/' 
## directory directly.
##
##
## Created:       8 December 2020 - Hunter Stanke
## Last modified: 4 May 2021 - Hunter Stanke
##
##==============================================================================
##==============================================================================


## SETUP -----------------------------------------------------------------------

## Load a handful of packages 
library(here)
library(dplyr)

## Loading our functions that will be called below 
invisible({
  sapply(list.files(here('src/GNN/'), full.names = TRUE), source)
})

## Check that we have all packages that we need
check_dependencies()

## How many cores do you want to use? 
## Check how many you have with parallel::detectCores(logical = TRUE) 
## DO NOT USE everything you have, e.g., if you have 4, use 3
## If you run into memory issues (R crashes), try cores = 1
parallel::detectCores(logical = TRUE) 
cores = 10




## ESTIMATE LAND AREA ----------------------------------------------------------

## All years that we're going to run
years <- as.character(2002:2017)

for (year in years) {
  ## Estimate total and % forest area by S-class within classified domains (strata)
  estimate_sclass_area(dirResults = here::here('results/GNN/'),
                       dirGNN = here::here('data/GNN/'),
                       year = year,
                       cores = cores)
}





## ESTIMATE RESTORATION NEED ---------------------------------------------------
for (year in years) {
  ## Estimate total and % land area in need of structural restoration
  estimate_rest_needs(dirResults = here::here('results/GNN/'),
                      dirRefCon = here::here('data/refCon/'),
                      year = year,
                      cores = cores)
}



## CLEAN UP FILES --------------------------------------------------------------
merge_annual_files(dirResults = here::here('results/GNN/'))

