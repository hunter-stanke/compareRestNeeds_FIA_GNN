##=====================================================
##=====================================================
##
## Since we are using fuzzed 
## and swapped FIA plot locations, we are going 
## to take the mode of all other variables of interest
## within a 1 km radius of the fuzzed/swapped location. 
## These are then the most likely values of each variable
## since the fuzzing generally keeps plots within 1 km 
## of their true location. Nothing we can do about
## swapping unfortunately. If a plot falls beyond 1km
## of GNN's forested mask (181 plots in total), then we 
## bump up the buffer width to 3km (grabs 122 plots), 
## 7km, and 15km 
##
## Created:       6 November 2020 - Hunter Stanke
## Last modified: 8 December 2020 - Hunter Stanke
##
##====================================================
##====================================================

## Fast spatial intersection of plots buffers w/ auxiliary rasters
## -----------------------------------------------------------------------------
## dirGIS (character):     directory where project GIS files are stored
## dirResults (character): directory where project results are stored
## cores (numeric) :       number of physical cores to use
fuzzy_plot_intersection <- function(dirGIS = here::here('data/GIS/'),
                                    dirResults = here::here('results/FIA/'),
                                    cores = 1) {
  
  ## Read our plot data
  plt <- read.csv(paste0(dirResults, 'prep/fiaPlts.csv')) %>%
    dplyr::filter(PLOT_STATUS_CD == 1) %>% # Forested conditions only
    dplyr::select(pltID, LAT, LON) %>% # slim down the width
    dplyr::distinct() %>% # drop remeasurements, as spatial info is constant
    dplyr::mutate(OBJECTID = 1:nrow(.))
  
  ## Pointers to our rasters of interest
  strata <- stars::read_stars(paste0(dirGIS, '/BPS_LLID/BPS_LLID.tif'))
  mz <- stars::read_stars(paste0(dirGIS, '/mapzones/mapzones.tif'))
  
  ## Make plots spatial
  pltSF <- sf::st_as_sf(plt, coords = c('LON', 'LAT'))
  sf::st_crs(pltSF) <- 4326
  
  ## Transform to same projection as reference raster
  pltSF <- sf::st_transform(pltSF, crs = sf::st_crs(strata))
  
  ## Make the buffers
  pltBuffer <- sf::st_buffer(pltSF, dist = 1000)

  ## Iterate over the plots - spatial intersection happens here
  if (Sys.info()['sysname'] == 'Windows'){
    cl <- makeCluster(nCores)
    clusterEvalQ(cl, {
      library(dplyr)
      library(stars)})
    out <- parLapply(cl, X = 1:nrow(pltBuffer), fun = extract_raster_values, 
                     pltBuffer, strata, mz)
    stopCluster(cl) # Kill the cluster
    
  } else { # Multicore systems
    out <- parallel::mclapply(X = 1:nrow(pltBuffer), FUN = extract_raster_values, 
                              pltBuffer, strata, mz, mc.cores = cores)
  }
  
  ## Back to dataframe
  plts1 <- dplyr::bind_rows(out)
  
  ## Which pltIDs are we missing? 183 are more than 1km from GNN-defined forest
  missPlts <- pltBuffer$pltID[!c(pltBuffer$pltID %in% plts1$pltID)]
  
  
  
  ## For those that don't intersect w/ GNN at 1km, bump up the buffer size to 3km
  ## Make the buffers
  missBuffer <- pltSF %>%
    dplyr::filter(pltID %in% missPlts) %>%
    sf::st_buffer(dist = 3000)
  
  ## Iterate over the plots - spatial intersection happens here
  if (Sys.info()['sysname'] == 'Windows'){
    cl <- parallel::makeCluster(nCores)
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(stars)})
    out <- parallel::parLapply(cl, X = 1:nrow(missBuffer), fun = extract_raster_values, 
                     missBuffer, strata, mz)
    stopCluster(cl) # Kill the cluster
    
  } else { # Multicore systems
    out <- parallel::mclapply(X = 1:nrow(missBuffer), FUN = extract_raster_values, 
                              missBuffer, strata, mz, mc.cores = cores)
  }

  ## Back to dataframe 
  plts3 <- dplyr::bind_rows(out)
  
  
  
  ## How many still missing?  61
  missPlts <- missPlts[!c(missPlts %in% plts3$pltID)]
  
  ## For those that don't intersect w/ GNN at 3km, bump up the buffer size to 7km
  ## Make the buffers
  missBuffer <- pltSF %>%
    dplyr::filter(pltID %in% missPlts) %>%
    sf::st_buffer(dist = 7000)
  
  ## Iterate over the plots - spatial intersection happens here
  if (Sys.info()['sysname'] == 'Windows'){
    cl <- parallel::makeCluster(nCores)
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(stars)})
    out <- parallel::parLapply(cl, X = 1:nrow(missBuffer), fun = extract_raster_values, 
                               missBuffer, strata, mz)
    stopCluster(cl) # Kill the cluster
    
  } else { # Multicore systems
    out <- parallel::mclapply(X = 1:nrow(missBuffer), FUN = extract_raster_values, 
                              missBuffer, strata, mz, mc.cores = cores)
  }
  
  ## Back to dataframe
  plts7 <- dplyr::bind_rows(out)
  
  
  
  ## How many still missing? 10. All in SE Oregon, not terribly surprising
  missPlts <- missPlts[!c(missPlts %in% plts7$pltID)]
  
  ## For those that don't intersect w/ GNN at 7km, bump up the buffer size to 15km
  ## Make the buffers
  missBuffer <- pltSF %>%
    dplyr::filter(pltID %in% missPlts) %>%
    sf::st_buffer(dist = 15000)
  
  ## Iterate over the plots - spatial intersection happens here
  if (Sys.info()['sysname'] == 'Windows'){
    cl <- parallel::makeCluster(nCores)
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(stars)})
    out <- parallel::parLapply(cl, X = 1:nrow(missBuffer), fun = extract_raster_values, 
                               missBuffer, strata, mz)
    stopCluster(cl) # Kill the cluster
    
  } else { # Multicore systems
    out <- parallel::mclapply(X = 1:nrow(missBuffer), FUN = extract_raster_values, 
                              missBuffer, strata, mz, mc.cores = cores)
  }
  plts15 <- dplyr::bind_rows(out)
  
  
  ## Pull all our plots back together
  plt <- bind_rows(plts1, plts3, plts7, plts15)
  
  ## Add on attributes from other tables
  plt <- joinAttributes(plt, dirGIS)
  
  ## Save results
  write.csv(plt, paste0(dirResults, 'prep/fiaPlts_attributes.csv'), row.names = F)
  
  
  cat('Fuzzy spatial intersection complete ...\n')
}




## Function to pull raster values from an overlapping polygon mask and compute 
## mode of resulting values
extract_raster_values <- function(x, pltBuffer, strata, mz) {
  
  ## Pull out one plot buffer
  buff <- pltBuffer[x,]
  
  ## Extract raster values from the strata and map zone rasters
  buff.strata <- strata[buff] %>% as.data.frame()
  buff.mz <- mz[buff] %>% as.data.frame()
  
  out <- buff.strata %>%
    ## Join resulting data.frames
    dplyr::rename(strat = BPS_LLID.tif) %>%
    dplyr::left_join(dplyr::select(buff.mz, x, y, mapzone = mapzones.tif), by = c('x', 'y')) %>%
    ## Add the pltID
    dplyr::mutate(pltID = buff$pltID) %>%
    ## These are the non-forest
    dplyr::filter(!is.na(strat)) %>%
    ## Counts for each variable
    dplyr::group_by(pltID, mapzone, strat) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    ## Select the most abundant group by plot
    dplyr::group_by(pltID) %>%
    dplyr::filter(n == max(n)) %>%
    ## If a tie, take the first
    dplyr::distinct(pltID, n, .keep_all = TRUE) %>%
    dplyr::ungroup()
  
  return(out)
}


## Append attributes of auxiliary rasters onto plot info table
joinAttributes <- function(plt, dirGIS) {
  
  ## Read raster attribute tables and select columns of interest
  strataAtt <- read.csv(paste0(dirGIS, 'attributes/BPS_LLID.csv'),
                        stringsAsFactors = FALSE) %>%
    dplyr::select(OBJECTID, BPS_LLID)
  
  bpsAtt <- read.csv(paste0(dirGIS, 'attributes/bps.txt'),
                     stringsAsFactors = FALSE) %>%
    dplyr::select(-c(Value, Count))
  
  mzAtt <- read.csv(paste0(dirGIS, 'attributes/mapzones.txt'),
                    stringsAsFactors = FALSE) %>%
    dplyr::select(OBJECTID, MAP_ZONE = LLID)
  
  ## Join attributes
  plt <- plt %>%
    dplyr::left_join(mzAtt, by = c('mapzone' = 'OBJECTID')) %>%
    dplyr::left_join(strataAtt, by = c('strat' = 'OBJECTID')) %>%
    dplyr::mutate(BPS = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,1],
                  PVT = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,2],
                  BpS_Code = paste(BPS, PVT, sep = "_")) %>%
    dplyr::select(-c('PVT')) %>%
    dplyr::left_join(bpsAtt, by = c('BpS_Code')) %>%
    dplyr::select(pltID, dplyr::everything())
  
  return(plt)
}


