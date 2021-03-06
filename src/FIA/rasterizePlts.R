##=====================================================
##=====================================================
##
## First portion of spatial intersection of input rasters
## with FIA plot locations. Since we are using fuzzed 
## and swapped FIA plot locations, we are going 
## to take the mode of all other variables of interest
## within a 1 km radius of the fuzzed/swapped location. 
## Here we produce buffers and rasterize them. In 
## intersectPlts.R, we intersect the buffers with the 
## landfire products and compute a mode by plot. 
## These are then the most likely values of each variable
## since the fuzzing generally keeps plots within 1 km 
## of their true location. Nothing we can do about
## swapping unfortunately.
##
##
## Created:       6 November 2020 - Hunter Stanke
## Last modified: 8 December 2020 - Hunter Stanke
##
##====================================================
##====================================================



## Function to generate circular buffers around FIA plots and rasterize output
## -----------------------------------------------------------------------------
## dirGIS (character):     directory where project GIS files are stored
## dirResults (character): directory where project results are stored
## bufferRadius (numeric): size of buffer to be generated, i.e, radius in meters
rasterize_plot_buffers <- function(dirGIS = here::here('data/GIS/'),
                                   dirResults = here::here('results/FIA/'),
                                   bufferRadius = 1000) {
  
  ## Read our plot data
  plt <- read.csv(paste0(dirResults, 'prep/fiaPlts.csv')) %>%
    dplyr::filter(PLOT_STATUS_CD == 1) %>% # Forested conditions only
    dplyr::select(pltID, LAT, LON) %>% # slim down the width
    dplyr::distinct() %>% # drop remeasurements, as spatial info is constant
    dplyr::mutate(OBJECTID = 1:nrow(.))
  
  ## using the strata raster as a template for plot buffer rasters
  ref <- stars::read_stars(paste0(dirGIS, '/BPS_LLID/BPS_LLID.tif'), proxy = F)
  ref$BPS_LLID.tif <- NA # Don't carry attributes into plot buffer rasters
  
  ## Make plots spatial
  pltSF <- sf::st_as_sf(plt, coords = c('LON', 'LAT'))
  sf::st_crs(pltSF) <- 4326
  
  ## Transform to same projection as reference raster
  pltSF <- sf::st_transform(pltSF, crs = sf::st_crs(ref))
  
  ## Make the buffers
  pltBuffer <- sf::st_buffer(pltSF, dist = bufferRadius)

  ## Make it a home if it needs one
  outdir <- paste0(dirGIS, 'plts_', bufferRadius / 1000, 'km/')
  dir.create(file.path(outdir), showWarnings = FALSE) # make the directory
  
  ## If anything lives there already, delete it
  existing.files <- list.files(outdir, full.names = TRUE)
  noprint <- file.remove(existing.files) 
  
  ## rasterize and save the buffers
  buffR <- stars::st_rasterize(dplyr::select(pltBuffer, OBJECTID), 
                               template = ref,
                               file = paste0(outdir, 'plts_', bufferRadius / 1000, 'km.tif'),
                               driver = 'GTiff')
  
  ## Save the "attribute table"
  write.csv(dplyr::select(as.data.frame(pltBuffer), OBJECTID, pltID), # drop geometry
            paste0(dirGIS, 'attributes/plts_', bufferRadius / 1000, 'km.csv'),
            row.names = FALSE)
}

