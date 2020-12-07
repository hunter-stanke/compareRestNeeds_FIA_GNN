##=====================================================
##=====================================================
##
## Second portion of spatial intersection of FIA plot 
## locations. Since we are using fuzzed swapped FIA 
## plot locations, we are going to take the mode of each
## variable of interest within a 1 km radius of the
## fuzzed/swapped location. In rasterizePlts.R we
## produce buffers and rasterize them. Here, we
## intersect the buffers with our rasters and compute 
## a mode by plot. This is the most likely value of each
## variable, since the fuzzing generally keeps plots 
## within 1 km of their true location. Nothing we can 
## do about swapping unfortunately.
##
##
## Created:       6 November 2020 - Hunter Stanke
## Last modified: 8 November 2020 - Hunter Stanke
##
##====================================================
##====================================================



##==============================================================================
##  Set up your working directory and number of cores to use -------------------
##==============================================================================
library(stars)
library(dplyr)
library(tidyr)
library(parallel)
library(data.table)
library(stringr)

setwd('/home/hunter/Dropbox/departureR6')

## parallel::detectCores(logical = FALSE)
cores <- 10




##==============================================================================
##  Read the "attribute tables" of our raster data -----------------------------
##==============================================================================

## Strata attributes
strataAtt <- read.csv('./inputRasters/attributes/BPS_LLID_attributes.csv',
                      stringsAsFactors = FALSE)
## HUC10 attributes
huc10Att <- read.csv('./inputRasters/attributes/huc10.txt',
                     stringsAsFactors = FALSE)
## HUC8 attributes
huc8Att <- read.csv('./inputRasters/attributes/huc8.csv',
                    stringsAsFactors = FALSE)
## Mapzone attributes
mzAtt <- read.csv('./inputRasters/attributes/mapzones.txt',
                  stringsAsFactors = FALSE)
## BPS attributes
bpsAtt <- read.csv('./inputRasters/attributes/bps.txt',
                  stringsAsFactors = FALSE)
## reference conditions
ref <- read.csv('./refCon/R6RefCon.txt',
                stringsAsFactors = FALSE)
## pltIDs
pltAtt1 <- read.csv('./inputRasters/attributes/plts_1km.csv')
pltAtt3 <- read.csv('./inputRasters/attributes/plts_3km.csv')

## Combine a few
att <- strataAtt %>%
  select(BPS_LLID, Value) %>%
  mutate(BPS_MODEL = str_split(BPS_LLID, "_", simplify = TRUE)[,1],
         BPS_CODE = paste(BPS_MODEL, str_split(BPS_LLID, "_", simplify = TRUE)[,2], sep = '_'),
         LLID = str_split(BPS_LLID, "_", simplify = TRUE)[,3]) %>%
  left_join(select(ref, LF_BpS_Code, FRG, LL, SCLASS = Sclass, Minus_2_SD, Plus_2_SD), by = c('BPS_CODE' = 'LF_BpS_Code'))



##==============================================================================
##  Set up an iterator to "chunk up" the raster data ---------------------------
##==============================================================================

## Get the dimensions of the BPS raster (same as S-class)
refRaster <- read_stars('./inputRasters/bps/bps.tif', proxy = TRUE)
refDim <- st_dimensions(refRaster)
xmin <- refDim$x$from
xmax <- refDim$x$to
ymin <- refDim$y$from
ymax <- refDim$y$to

## Now we are going to loop over 'chunks' and process each individually
chunkSize <- 100 # 100 by 100 cell squares

## Set up starting indices for rasterIO
xstart <- seq(xmin, xmax, by = chunkSize)
ystart <- seq(ymin, ymax, by = chunkSize)

## Our iterator
index = tidyr::crossing(xstart, ystart) %>%
  mutate(id = paste(xstart, ystart)) %>%
  split(.$id)




##==============================================================================
##  Process each chunk individually --------------------------------------------
##==============================================================================

## Define a function to process chunks
processChunk <- function(iter, index, xstart, ystart,
                         xmax, ymax, chunkSize, dir){
  
  ## Pull the index info
  i = index[[iter]]
  
  ## If it's the last chunk in row/ column, probably won't have the full 100
  ## Handle that
  chunkSize_x <- ifelse(i$xstart == max(xstart), xmax - i$xstart, chunkSize)
  chunkSize_y <- ifelse(i$ystart == max(ystart), ymax - i$ystart, chunkSize)
  
  ## Define the chunk
  rasterio = list(nXOff = i$xstart, nYOff = i$ystart, nXSize = chunkSize_x, nYSize = chunkSize_y)
  
  ## Read the chunks
  strata <- read_stars(paste0(dir, '/inputRasters/BPS_LLID_DeMeo/BPS_LLID.tif'), RasterIO = rasterio, proxy = FALSE)
  huc10 <- read_stars(paste0(dir, '/inputRasters/huc10/huc10.tif'), RasterIO = rasterio, proxy = FALSE)
  huc8 <- read_stars(paste0(dir, '/inputRasters/huc8/huc8.tif'), RasterIO = rasterio, proxy = FALSE)
  mz <- read_stars(paste0(dir, '/inputRasters/mapzones/mapzones.tif'), RasterIO = rasterio, proxy = FALSE)
  plt1 <- read_stars(paste0(dir, '/inputRasters/plts_1km/plts_1km.tif'), RasterIO = rasterio, proxy = FALSE)
  plt3 <- read_stars(paste0(dir, '/inputRasters/plts_3km/plts_3km.tif'), RasterIO = rasterio, proxy = FALSE)
  
  ## Convert to data.frame and combine
  strata <- as.data.frame(strata)
  huc10 <- as.data.frame(huc10)
  huc8 <- as.data.frame(huc8)
  mz <- as.data.frame(mz)
  plt1 <- as.data.frame(plt1)
  plt3 <- as.data.frame(plt3)
  dat <- strata %>%
    rename(strat = BPS_LLID.tif) %>%
    mutate(huc10 = huc10$huc10.tif,
           huc8 = huc8$huc8.tif,
           mapzone = mz$mapzones.tif,
           plt1 = plt1$plts_1km.tif,
           plt3 = plt3$plts_3km.tif)
  
  suppressMessages({
    totals1 <- dat %>%
      filter(!is.na(plt1)) %>%
      group_by(plt1, mapzone, huc8, huc10, strat) %>%
      summarise(n = n()) %>%
      as.data.table()
    totals3 <- dat %>%
      filter(!is.na(plt3)) %>%
      group_by(plt3, mapzone, huc8, huc10, strat) %>%
      summarise(n = n()) %>%
      as.data.table()
  })
  
  
  return(list(t1 = totals1, t3 = totals3))
}

## Run the above for all chunks
out <- mclapply(X = names(index), FUN = processChunk, index,
                xstart, ystart, xmax, ymax, chunkSize,
                dir= getwd(), mc.cores = cores)


##==============================================================================
##  Process results and summarize ----------------------------------------------
##==============================================================================
cat('Merging results ...\n')
out <- unlist(out, recursive = FALSE)
## Pull out the results
chunks1 <- rbindlist(out[names(out) == 't1'])
chunks3 <- rbindlist(out[names(out) == 't3'])

cat('Summarizing results ...\n')


## Summarize everything, if anything is available at 1km, use it
plt1 <- chunks1 %>%
  ## These are the non-forest
  filter(!is.na(strat)) %>%
  ## Select the most abundant group by plot
  group_by(plt1) %>%
  filter(n == max(n)) %>%
  ## If a tie, take the first
  distinct(plt1, n, .keep_all = TRUE) %>%
  ungroup() %>%
  ## Add on pltID
  left_join(pltAtt1, by = c('plt1' = 'OBJECTID')) %>%
  select(-c(plt1))


## IF nothing shows up in 1km, try 3, otherwise out of sample
plt3 <- chunks3 %>%
  ## Add on pltID
  left_join(pltAtt3, by = c('plt3' = 'OBJECTID')) %>%
  ## Only those not in the 1km buffer
  filter(pltID %in% plt1$pltID == FALSE) %>%
  ## These are the non-forest
  filter(!is.na(strat)) %>%
  ## Select the most abundant group by plot
  group_by(plt3) %>%
  filter(n == max(n)) %>%
  ## If a tie, take the first
  distinct(plt3, n, .keep_all = TRUE) %>%
  ungroup() %>%
  select(-c(plt3))

## Combine and append attributes
plt <- bind_rows(plt1, plt3) %>%
  left_join(select(huc10Att, OBJECTID, HUC10 = LLID), by = c('huc10' = 'OBJECTID')) %>%
  left_join(select(huc8Att, OBJECTID, HUC8 = LLID), by = c('huc8' = 'OBJECTID')) %>%
  left_join(select(mzAtt, OBJECTID, MAP_ZONE = LLID), by = c('mapzone' = 'OBJECTID')) %>%
  left_join(select(strataAtt, OBJECTID, BPS_LLID), by = c('strat' = 'OBJECTID')) %>%
  mutate(BPS = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,1],
         PVT = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,2],
         BpS_Code = paste(BPS, PVT, sep = "_")) %>%
  left_join(select(bpsAtt, -c(Value, Count)), by = c('BpS_Code')) %>%
  select(pltID, everything())

## Save results
write.csv(plt, './results/prepData/fiaPlts_attributes.csv', row.names = FALSE)
