##=====================================================
##=====================================================
##
## Estimate total land area by S-classes within strata
## defined by biophysical settings and landscape units
## Landscape units are one of HUC10, HUC8, or HUC6,
## depending on the historic fire regime of the
## biophysical setting. In addition to the landscape
## units from DeMeo et al 2020, summarize to some
## larger units within the hierarchy (BPS, map zones).
##
## Raster data is pretty big, bigger than we'd be able
## to hold in RAM if we scale this up beyond R6. So,
## we will chunk the data up into small, manageable
## bits, process them individually, and combine
## results at the end. We use 'stars' to handle all
## raster data - open source, fast, and works well.
##
##
## Created:       15 October 2020 - Hunter Stanke
## Last modified: 2 November 2020 - Hunter Stanke
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

## Stand structural classification (sclass)
scAtt <- read.csv('./inputRasters/attributes/sClass.txt',
                  stringsAsFactors = FALSE)
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
## reference conditions
ref <- read.csv('./refCon/R6RefCon.txt',
                stringsAsFactors = FALSE)

## Combine a few
att <- strataAtt %>%
  select(BPS_LLID, Value) %>%
  mutate(BPS_MODEL = str_split(BPS_LLID, "_", simplify = TRUE)[,1],
         BPS_CODE = paste(BPS_MODEL, str_split(BPS_LLID, "_", simplify = TRUE)[,2], sep = '_'),
         LLID = str_split(BPS_LLID, "_", simplify = TRUE)[,3]) %>%
  left_join(select(ref, LF_BpS_Code, FRG, LL, SCLASS = Sclass, Minus_2_SD, Plus_2_SD), by = c('BPS_CODE' = 'LF_BpS_Code')) %>%
  left_join(select(scAtt, SCLASS = Sclass_Code, sclass = Value), by = 'SCLASS')



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
  sclass <- read_stars(paste0(dir, '/inputRasters/sClass/S_Class1.tif'), RasterIO = rasterio, proxy = FALSE)
  huc10 <- read_stars(paste0(dir, '/inputRasters/huc10/huc10.tif'), RasterIO = rasterio, proxy = FALSE)
  huc8 <- read_stars(paste0(dir, '/inputRasters/huc8/huc8.tif'), RasterIO = rasterio, proxy = FALSE)
  mz <- read_stars(paste0(dir, '/inputRasters/mapzones/mapzones.tif'), RasterIO = rasterio, proxy = FALSE)
  #lidar <- read_stars(paste0(dir, '/GNN_AA_lidarFootprints/footprint_raster/footprint.tif'), RasterIO = rasterio, proxy = FALSE)

  ## Convert to data.frame and combine
  strata <- as.data.frame(strata)
  sclass <- as.data.frame(sclass)
  huc10 <- as.data.frame(huc10)
  huc8 <- as.data.frame(huc8)
  mz <- as.data.frame(mz)
  #lidar <- as.data.frame(lidar)
  dat <- strata %>%
    rename(strat = BPS_LLID.tif) %>%
    mutate(sclass = sclass$S_Class1.tif,
           huc10 = huc10$huc10.tif,
           huc8 = huc8$huc8.tif,
           mapzone = mz$mapzones.tif,
           #lidar = lidar$footprint.tif
           )

  suppressMessages({
    ## Sum up total area in each S-class, this is additive and internally consistent
    ## Also get the number of FIA plot locations in each
    totals <- dat %>%
      #filter(lidar == 1) %>%
      group_by(mapzone, huc8, huc10, strat, sclass) %>%
      summarise(n = n()) %>%
      as.data.table()
  })


  return(totals)
}

## Run the above for all chunks
out <- mclapply(X = names(index), FUN = processChunk, index,
                xstart, ystart, xmax, ymax, chunkSize,
                dir= getwd(), mc.cores = cores)


##==============================================================================
##  Process results and summarize ----------------------------------------------
##==============================================================================
cat('Merging results ...\n')
## Pull out the results
totalSC_chunks <- rbindlist(out)

cat('Summarizing results ...\n')


## Finish summarizing strata totals - most will straddle multiple "chunks"
bpsLLID <- totalSC_chunks %>%
  ## Eastside only for now
  left_join(select(mzAtt, OBJECTID, MAP_ZONE = LLID), by = c('mapzone' = 'OBJECTID')) %>%
  filter(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') == FALSE) %>%
  as.data.frame() %>%
  group_by(strat, sclass) %>%
  summarize(n = sum(n, na.rm = TRUE)) %>%
  group_by(strat) %>%
  mutate(AREA = n * 0.222395, # Convert from sq meters to ACRES
         AREA_TOTAL = sum(AREA, na.rm = TRUE),
         AREA_PCT = AREA / AREA_TOTAL * 100) %>%
  ungroup() %>%
  mutate(SCLASS_NAME = case_when(sclass == 0 ~ 'NF',
                                 sclass == 1 ~ 'Early seral',
                                 sclass == 2 ~ 'Mid-seral closed',
                                 sclass == 3 ~ 'Mid-seral open',
                                 sclass == 4 ~ 'Late-seral open',
                                 sclass == 5 ~ 'Late-seral closed')) %>%
  ## Add attributes
  left_join(att, by  = c('strat' = 'Value', 'sclass')) %>%
  select(BpS_Code = BPS_CODE, BPS_LLID, SCLASS, AREA_PCT, AREA, AREA_TOTAL) %>%
  filter(!is.na(BPS_LLID))


## To BPS 
bps <- totalSC_chunks %>%
  as.data.frame() %>%
  ## Eastside only for now
  left_join(select(mzAtt, OBJECTID, MAP_ZONE = LLID), by = c('mapzone' = 'OBJECTID')) %>%
  filter(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') == FALSE) %>%
  ## Add attributes
  left_join(att, by  = c('strat' = 'Value', 'sclass')) %>%
  group_by(BPS_CODE, sclass, SCLASS) %>%
  summarize(n = sum(n, na.rm = TRUE)) %>%
  group_by(BPS_CODE) %>%
  mutate(AREA = n * 0.222395, # Convert from sq meters to ha
         AREA_TOTAL = sum(AREA, na.rm = TRUE),
         AREA_PCT = AREA / AREA_TOTAL * 100) %>%
  ungroup() %>%
  mutate(SCLASS_NAME = case_when(sclass == 0 ~ 'NF',
                                 sclass == 1 ~ 'Early seral',
                                 sclass == 2 ~ 'Mid-seral closed',
                                 sclass == 3 ~ 'Mid-seral open',
                                 sclass == 4 ~ 'Late-seral open',
                                 sclass == 5 ~ 'Late-seral closed')) %>%
  select(BpS_Code = BPS_CODE, SCLASS, AREA_PCT, AREA, AREA_TOTAL) %>%
  filter(!is.na(BpS_Code))


## To BPS + mapzone
bpsMZ <- totalSC_chunks %>%
  as.data.frame() %>%
  ## Eastside only for now
  left_join(select(mzAtt, OBJECTID, MAP_ZONE = LLID), by = c('mapzone' = 'OBJECTID')) %>%
  filter(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') == FALSE) %>%
  ## Add attributes
  left_join(att, by  = c('strat' = 'Value', 'sclass')) %>%
  ## Summarize
  group_by(BPS_CODE, MAP_ZONE, sclass, SCLASS) %>%
  summarize(n = sum(n, na.rm = TRUE)) %>%
  group_by(BPS_CODE, MAP_ZONE) %>%
  mutate(AREA = n * 0.222395, # Convert from sq meters to ha
         AREA_TOTAL = sum(AREA, na.rm = TRUE),
         AREA_PCT = AREA / AREA_TOTAL * 100) %>%
  ungroup() %>%
  mutate(SCLASS_NAME = case_when(sclass == 0 ~ 'NF',
                                 sclass == 1 ~ 'Early seral',
                                 sclass == 2 ~ 'Mid-seral closed',
                                 sclass == 3 ~ 'Mid-seral open',
                                 sclass == 4 ~ 'Late-seral open',
                                 sclass == 5 ~ 'Late-seral closed')) %>%
  select(BpS_Code = BPS_CODE, MAP_ZONE, SCLASS, AREA_PCT, AREA, AREA_TOTAL) %>%
  filter(!is.na(BpS_Code))


## Full region
full <- totalSC_chunks %>%
  as.data.frame() %>%
  ## Eastside only for now
  left_join(select(mzAtt, OBJECTID, MAP_ZONE = LLID), by = c('mapzone' = 'OBJECTID')) %>%
  filter(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') == FALSE) %>%
  ## Add attributes
  left_join(att, by  = c('strat' = 'Value', 'sclass')) %>%
  filter(!is.na(sclass) & !is.na(SCLASS)) %>%
  ## Summarize
  group_by(sclass, SCLASS) %>%
  summarize(n = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(AREA = n * 0.222395, # Convert from sq meters to ha
         AREA_TOTAL = sum(AREA, na.rm = TRUE),
         AREA_PCT = AREA / AREA_TOTAL * 100) %>%
  mutate(SCLASS_NAME = case_when(sclass == 0 ~ 'NF',
                                 sclass == 1 ~ 'Early seral',
                                 sclass == 2 ~ 'Mid-seral closed',
                                 sclass == 3 ~ 'Mid-seral open',
                                 sclass == 4 ~ 'Late-seral open',
                                 sclass == 5 ~ 'Late-seral closed')) %>%
  select(SCLASS, AREA_PCT, AREA, AREA_TOTAL) 


## Lidar footprint
lidar <- totalSC_chunks %>%
  as.data.frame() %>%
  ## Add attributes
  left_join(att, by  = c('strat' = 'Value', 'sclass')) %>%
  filter(!is.na(sclass) & !is.na(SCLASS)) %>%
  ## Summarize
  group_by(sclass, SCLASS) %>%
  summarize(n = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(AREA = n * 0.222395, # Convert from sq meters to ha
         AREA_TOTAL = sum(AREA, na.rm = TRUE),
         AREA_PCT = AREA / AREA_TOTAL * 100) %>%
  mutate(SCLASS_NAME = case_when(sclass == 0 ~ 'NF',
                                 sclass == 1 ~ 'Early seral',
                                 sclass == 2 ~ 'Mid-seral closed',
                                 sclass == 3 ~ 'Mid-seral open',
                                 sclass == 4 ~ 'Late-seral open',
                                 sclass == 5 ~ 'Late-seral closed')) %>%
  select(SCLASS, AREA_PCT, AREA, AREA_TOTAL)

## Save results
fwrite(bpsLLID, './results/areaGNN/sclass_BPSLLID.csv')
fwrite(bps, './results/areaGNN/sclass_BPS.csv')
fwrite(bpsMZ, './results/areaGNN/sclass_BPS_MAPZONE.csv')
fwrite(full, './results/areaGNN/sclass_ORWA.csv')
fwrite(lidar, './results/areaGNN/sclass_WA_lidarFP.csv')







