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
## Last modified: 6 November 2020 - Hunter Stanke
##
##====================================================
##====================================================


##===============================================================================
##  Set up your working directory and number of cores to use --------------------
##===============================================================================

library(stars)
library(dplyr)
library(sf)
library(parallel)
library(rFIA)


## Set to local
setwd('/home/hunter/Dropbox/departureR6')


##===============================================================================
##  Data read and prep ----------------------------------------------------------
##===============================================================================

## Read our plot data and drop all but spatial info and pltID
## Only want distinct plot locations
## Only using plot locations with forested condition present
plt <- read.csv('./results/prepData/fiaPlts.csv') %>%
  filter(PLOT_STATUS_CD == 1) %>%
  dplyr::select(pltID, LAT, LON) %>%
  distinct()


## using the strata raster as a template
refRaster <- read_stars('./inputRasters/BPS_LLID_DeMeo/BPS_LLID.tif', proxy = FALSE)
refRaster$BPS_LLID.tif <- NA


##===============================================================================
##  Make 1 km buffer around FIA plots--------------------------------------------
##===============================================================================

## Convert to sf points
pltSF <- st_as_sf(plt, coords = c('LON', 'LAT'))
st_crs(pltSF) <- 4326

## Transform to same projection as reference
pltSF <- st_transform(pltSF, crs = st_crs(refRaster))


## Make the buffers
pltBuffer <- st_buffer(pltSF, dist = 1000)
pltBuffer3 <- st_buffer(pltSF, dist = 3000) ## 3km radius, sometimes 1km is too small


## Make an OBJECTID column so we can pull pltIDs after the intersection
pltBuffer$OBJECTID <- 1:nrow(pltBuffer)
pltBuffer3$OBJECTID <- 1:nrow(pltBuffer3)


##==============================================================================
##  Now we rasterize the polygons and write them out ---------------------------
##==============================================================================



## Now rasterize them on the same grid
buffR <- st_rasterize(select(pltBuffer, OBJECTID), template = refRaster)
buff3R <- st_rasterize(select(pltBuffer3, OBJECTID), template = refRaster)

## Save raster and its "attributes"
write_stars(buffR, './inputRasters/plts_1km/plts_1km.tif', driver = 'GTiff')
write_stars(buff3R, './inputRasters/plts_3km/plts_3km.tif', driver = 'GTiff')

write.csv(select(as.data.frame(pltBuffer), OBJECTID, pltID), 
          './inputRasters/attributes/plts_1km.csv',
          row.names = FALSE)
write.csv(select(as.data.frame(pltBuffer3), OBJECTID, pltID), 
          './inputRasters/attributes/plts_3km.csv',
          row.names = FALSE)
