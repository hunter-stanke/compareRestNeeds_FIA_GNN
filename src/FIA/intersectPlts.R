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
## Last modified: 8 December 2020 - Hunter Stanke
##
##====================================================
##====================================================



## Fast spatial intersection of plots buffers w/ auxiliary rasters
## -----------------------------------------------------------------------------
## dirGIS (character):     directory where project GIS files are stored
## dirResults (character): directory where project results are stored
## cores (numeric) :       number of physical cores to use
intersect_plot_buffers <- function(dirGIS = here::here('data/GIS/'),
                                   dirResults = here::here('results/'),
                                   cores = 1) {
  
  ## Attributes of our plot buffers
  pltAtt1 <- read.csv(paste0(dirGIS, 'attributes/plts_1km.csv'))
  pltAtt3 <- read.csv(paste0(dirGIS, 'attributes/plts_3km.csv'))
  
  ## Get the dimensions of our auxiliary rasters (all must be identical)
  refRaster <- stars::read_stars(paste0(dirGIS, 'BPS_LLID/BPS_LLID.tif'))
  refDim <- stars::st_dimensions(refRaster)
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
    dplyr::mutate(id = paste(xstart, ystart)) %>%
    split(.$id)
  
  ## Process the chunks with function below - spatial intersection happens here
  out <- parallell::mclapply(X = names(index), FUN = processChunk, index,
                             xstart, ystart, xmax, ymax, chunkSize,
                             dir = dirGIS, mc.cores = cores)
  
  ## Merge the results of each chunk
  out <- unlist(out, recursive = FALSE)
  chunks1 <- rbindlist(out[names(out) == 't1'])
  chunks3 <- rbindlist(out[names(out) == 't3'])
  
  
  ## Summarize everything, if anything is available at 1km, use it
  plt1 <- chunks1 %>%
    ## These are the non-forest
    dplyr::filter(!is.na(strat)) %>%
    ## Select the most abundant group by plot, i.e., the mode
    dplyr::group_by(plt1) %>%
    dplyr::filter(n == max(n)) %>%
    ## If a tie, take the first
    dplyr::distinct(plt1, n, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    ## Add on pltID
    dplyr::left_join(pltAtt1, by = c('plt1' = 'OBJECTID')) %>%
    dplyr::select(-c(plt1))
  
  
  ## If nothing shows up in 1km, try 3, otherwise out of sample
  plt3 <- chunks3 %>%
    ## Add on pltID
    dplyr::left_join(pltAtt3, by = c('plt3' = 'OBJECTID')) %>%
    ## Only those not in the 1km buffer
    dplyr::filter(pltID %in% plt1$pltID == FALSE) %>%
    ## These are the non-forest
    dplyr::filter(!is.na(strat)) %>%
    ## Select the most abundant group by plot
    dplyr::group_by(plt3) %>%
    dplyr::filter(n == max(n)) %>%
    ## If a tie, take the first
    dplyr::distinct(plt3, n, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(plt3))
  
  ## Combine results from above
  plt <- dplyr::bind_rows(plt1, plt3) 
  
  
  ## Join on attributes for each auxiliary layer using helper function from below
  plt <- joinAttributes(plt, dirGIS)
  
  ## Save results
  write.csv(plt, paste0(dirResults, 'fiaPlts_attributes.csv'), row.names = F)
  
  
}


## Helper function to process chunks in 'intersect_plot_buffers' above
## Right now the function is quite inflexible in the interest of speed, i.e.,
## we don't want to re-chunk/read for each potential auxiliary variable and 
## I don't want to put the time into generalizing it right now.
processChunk <- function(iter, index, xstart, ystart,
                         xmax, ymax, chunkSize, dir){
  
  ## Pull the index info
  i = index[[iter]]
  
  ## If it's the last chunk in row/ column, probably won't have the full 100
  ## Handle that
  chunkSize_x <- ifelse(i$xstart == max(xstart), xmax - i$xstart, chunkSize)
  chunkSize_y <- ifelse(i$ystart == max(ystart), ymax - i$ystart, chunkSize)
  
  ## Define the chunk
  rasterio = list(nXOff = i$xstart, nYOff = i$ystart, 
                  nXSize = chunkSize_x, nYSize = chunkSize_y)
  
  ## Read the chunks
  strata <- stars::read_stars(paste0(dir, 'BPS_LLID/BPS_LLID.tif'), 
                              RasterIO = rasterio, proxy = FALSE)
  huc10 <- stars::read_stars(paste0(dir, 'huc10/huc10.tif'), 
                             RasterIO = rasterio, proxy = FALSE)
  huc8 <- stars::read_stars(paste0(dir, 'huc8/huc8.tif'), 
                            RasterIO = rasterio, proxy = FALSE)
  mz <- stars::read_stars(paste0(dir, 'mapzones/mapzones.tif'), 
                          RasterIO = rasterio, proxy = FALSE)
  plt1 <- stars::read_stars(paste0(dir, 'plts_1km/plts_1km.tif'), 
                            RasterIO = rasterio, proxy = FALSE)
  plt3 <- stars::read_stars(paste0(dir, 'plts_3km/plts_3km.tif'),
                            RasterIO = rasterio, proxy = FALSE)
  
  ## Convert to data.frame and combine
  strata <- as.data.frame(strata)
  huc10 <- as.data.frame(huc10)
  huc8 <- as.data.frame(huc8)
  mz <- as.data.frame(mz)
  plt1 <- as.data.frame(plt1)
  plt3 <- as.data.frame(plt3)
  dat <- strata %>%
    dplyr::rename(strat = BPS_LLID.tif) %>%
    dplyr::mutate(huc10 = huc10$huc10.tif,
                  huc8 = huc8$huc8.tif,
                  mapzone = mz$mapzones.tif,
                  plt1 = plt1$plts_1km.tif,
                  plt3 = plt3$plts_3km.tif)
  
  suppressMessages({
    totals1 <- dat %>%
      dplyr::filter(!is.na(plt1)) %>%
      dplyr::group_by(plt1, mapzone, huc8, huc10, strat) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      as.data.table()
    totals3 <- dat %>%
      dplry::filter(!is.na(plt3)) %>%
      dplyr::group_by(plt3, mapzone, huc8, huc10, strat) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      as.data.table()
  })
  
  
  return(list(t1 = totals1, t3 = totals3))
}



## Append attributes of auxiliary rasters onto plot info table
joinAttributes <- function(plt, dirGIS) {
  
  ## Read raster attribute tables and select columns of interest
  strataAtt <- read.csv(paste0(dirGIS, 'attributes/BPS_LLID_attributes.csv'),
                        stringsAsFactors = FALSE) %>%
    dplyr::select(OBJECTID, BPS_LLID)
  
  huc10Att <- read.csv(paste0(dirGIS, 'attributes/huc10.txt'),
                       stringsAsFactors = FALSE) %>%
    dplyr::select(OBJECTID, HUC10 = LLID)
  
  huc8Att <- read.csv(paste0(dirGIS, 'attributes/huc8.csv'),
                      stringsAsFactors = FALSE) %>%
    dplyr::select(OBJECTID, HUC8 = LLID)
  
  mzAtt <- read.csv(paste0(dirGIS, 'attributes/mapzones.txt'),
                    stringsAsFactors = FALSE) %>%
    dplyr::select(OBJECTID, MAP_ZONE = LLID)
  
  bpsAtt <- read.csv(paste0(dirGIS, 'attributes/bps.txt'),
                     stringsAsFactors = FALSE) %>%
    dplyr::select(-c(Value, Count))

  
  ## Join attributes
  plt <- plt %>%
    dplyr::left_join(huc10Att, by = c('huc10' = 'OBJECTID')) %>%
    dplyr::left_join(huc8Att,  by = c('huc8' = 'OBJECTID')) %>%
    dplyr::left_join(mzAtt, by = c('mapzone' = 'OBJECTID')) %>%
    dplyr::left_join(strataAtt, by = c('strat' = 'OBJECTID')) %>%
    dplyr::mutate(BPS = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,1],
                  PVT = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,2],
                  BpS_Code = paste(BPS, PVT, sep = "_")) %>%
    dplyr::left_join(bpsAtt, by = c('BpS_Code')) %>%
    dplyr::select(pltID, dplyr::everything())
  
  return(plt)
}



