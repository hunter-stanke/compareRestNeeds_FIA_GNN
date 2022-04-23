##=====================================================
##=====================================================
##
## Estimate total land area by S-classes within strata
## and other spatial zones. Strata and s-classes should
## be given as raster data. Optionally add groups 
## by unique spatial zones
##
## Raster data is pretty big, bigger than we'll be able
## to hold in RAM. So, we chunk the data up into small,
## manageable bits, process them individually, and 
## combine results at the end. We use 'stars' to handle
## all raster data - open source, fast, and works well.
##
##
## Created:       15 October 2020 - Hunter Stanke
## Last modified: 9 February 2021 - Hunter Stanke
##
##====================================================
##====================================================

## Wrapper around sum_sclass_by_strata -- see below for details
estimate_sclass_area <- function(dirResults = here::here('results/GNN/'),
                                 dirGNN = here::here('data/GNN/'),
                                 year,
                                 cores = 1) {
  
  ## Identify the folder/file containing the sclass raster for the given year
  sclassDir <- paste0(dirGNN, 'sclass/', year, '/')
  sclassFile <- list.files(sclassDir, full.names = TRUE)[stringr::str_sub(list.files(sclassDir), -4, -1) == '.tif']
  sclassFile <- stringr::str_replace(sclassFile, '//', '/')
  
  ## BPS x LLID
  bpsllid <- sum_sclass_by_strata(strata = here::here('data/GIS/BPS_LLID/BPS_LLID.tif'),
                                  strataRef = here::here('data/GIS/attributes/BPS_LLID.csv'),
                                  sclass = sclassFile,
                                  sclassRef = here::here('data/GIS/attributes/sClass.txt'),
                                  mz = here::here('data/GIS/mapzones/mapzones.tif'),
                                  mzRef = here::here('data/GIS/attributes/mapzones.txt'),
                                  dirResults = dirResults,
                                  cores = cores)
  suppressMessages({
    ## Clean up the names of BPS x LLID summary
    bpsllid <- bpsllid %>% 
      dplyr::mutate(YEAR = year,
                    BpS = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,1],
                    PVT = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,2],
                    LLID = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,3],
                    BpS_Code = paste(BpS, PVT, sep = '_')) %>%
      dplyr::select(YEAR, ew, BpS, PVT, BpS_Code, BPS_LLID, sclass, AREA_TOTAL) %>%
      ungroup()
    
    
    ## Now rather than re-run with BPS as strata, just sum up the BPS x LLID
    ## estimates within BPS and up to region-wide totals
    
    ## BPS x EW  totals
    bps.ew <- bpsllid %>%
      dplyr::group_by(YEAR, ew, BpS, PVT, BpS_Code, sclass) %>%
      dplyr::summarise(AREA_TOTAL = sum(AREA_TOTAL, na.rm = TRUE)) %>%
      ungroup()
    
    ## Eastside / westside totals
    ew <- bpsllid %>%
      dplyr::group_by(YEAR, ew, sclass) %>%
      dplyr::summarise(AREA_TOTAL = sum(AREA_TOTAL, na.rm = TRUE))%>%
      ungroup()
  
    ## Regional totals
    orwa <- ew %>%
      dplyr::group_by(YEAR, sclass) %>%
      dplyr::summarise(AREA_TOTAL = sum(AREA_TOTAL, na.rm = TRUE))%>%
      ungroup()
    
  })
  
  
  ## save results
  write.csv(bps.ew, paste0(dirResults, 'sclass/annual/ORWA_EW_BPS_', year, '.csv'), row.names = FALSE)
  write.csv(ew, paste0(dirResults, 'sclass/annual/ORWA_EW_', year, '.csv'), row.names = FALSE)
  write.csv(orwa, paste0(dirResults, 'sclass/annual/ORWA_', year, '.csv'), row.names = FALSE)
  
}




## Fast spatial summary of sclass abundance within strata
## -----------------------------------------------------------------------------
## strata (character):     path to "strata" raster (including extension)
## strataRef (character):  path to "strata" attribute table
## sclass (character):     path to "sclass" raster
## sclassRef (character):  path to "sclass" attribute table
## dirResults (character): directory where project results are stored
## cores (numeric) :       number of physical cores to use
sum_sclass_by_strata <- function(strata,
                                 strataRef,
                                 sclass,
                                 sclassRef,
                                 mz,
                                 mzRef,
                                 dirResults,
                                 cores) {
  
  
  ##  Read the "attribute tables" of our raster data -----------------------------

  ## Strata attributes
  strataAtt <- read.csv(strataRef,
                        stringsAsFactors = FALSE)
  ## Map zone attributes --> eastside and westside
  mzAtt <- read.csv(mzRef, stringsAsFactors = FALSE) %>%
    rename(MAP_ZONE = LLID) %>%
    mutate(ew = case_when(is.na(MAP_ZONE) ~ NA_character_,
                          MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') ~ 'westside',
                          TRUE ~ 'eastside'))
  
  


  ##  Set up an iterator to "chunk up" the raster data ---------------------------

  ## A reference raster - dimensions of sclass and strata must be identical
  refRaster <- stars::read_stars(strata, proxy = FALSE)
  
  ## Get dimensions of resulting raster data
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
  
  

  
  
  
  ## Sum up sclass counts w/in each chunk in parallel --------------------------
  if (Sys.info()['sysname'] == 'Windows'){
    cl <- makeCluster(cores)
    clusterEvalQ(cl, {
      library(dplyr)
      library(stars)})
    out <- parLapply(cl, X = names(index), fun = sum_chunk, index,
                     start, ystart, xmax, ymax, chunkSize, strata, 
                     sclass, mz)
    stopCluster(cl) # Kill the cluster
    
  } else { # Multicore systems
    out <- parallel::mclapply(X = names(index), FUN = sum_chunk, index,
                              xstart, ystart, xmax, ymax, chunkSize,
                              strata, sclass, mz, mc.cores = cores)
  }
  
  suppressMessages({
    ## Merge the results & summarize over the chunk-level sums----------------------
    totals <- data.table::rbindlist(out) %>%
      as.data.frame() %>%
      dplyr::group_by(mz, strat, sc) %>%
      dplyr::summarise(n = sum(n, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      ## Convert cell count to acres
      dplyr::mutate(AREA_TOTAL = n * 0.222395) %>%
      ## Join on attribute tables and convert to format consistent w/ rFIA
      dplyr::left_join(strataAtt, by = c('strat' = 'OBJECTID')) %>%
      dplyr::left_join(select(mzAtt, OBJECTID, MAP_ZONE, ew), by = c('mz' = 'OBJECTID')) %>%
      ## Hard coding sclasses here
      # dplyr::mutate(sclass = case_when(is.na(sc) ~ NA_character_,
      #                              sc == 0 ~ NA_character_,
      #                              sc == 1 ~ 'E',
      #                              sc == 2 ~ 'C',
      #                              sc == 3 ~ 'A',
      #                              sc == 4 ~ 'B',
      #                              sc == 5 ~ 'D')) %>%
      dplyr::mutate(sclass = case_when(is.na(sc) ~ NA_character_,
                                       sc == 0 ~ NA_character_,
                                       sc == 1 ~ 'A',
                                       sc == 2 ~ 'B',
                                       sc == 3 ~ 'C',
                                       sc == 4 ~ 'D',
                                       sc == 5 ~ 'E')) %>%
      dplyr::select(-c(any_of(c('n', 'sc', 'strat', 'mz')))) ## Still ugly, clean up on the top end
  })

    
  
 return(totals)
  
}


## Define a function to process chunks
sum_chunk <- function(iter, index, xstart, ystart,
                      xmax, ymax, chunkSize,
                      strata, sclass, mz){
  
  ## Pull the index info
  i = index[[iter]]
  
  ## If it's the last chunk in row/ column, probably won't have the full 100
  ## Handle that
  chunkSize_x <- ifelse(i$xstart == max(xstart), xmax - i$xstart, chunkSize)
  chunkSize_y <- ifelse(i$ystart == max(ystart), ymax - i$ystart, chunkSize)
  
  ## Define the chunk
  rasterio = list(nXOff = i$xstart, nYOff = i$ystart, nXSize = chunkSize_x, nYSize = chunkSize_y)
  
  ## Read the chunks
  strataR <- stars::read_stars(strata, RasterIO = rasterio, proxy = FALSE)
  sclassR <- stars::read_stars(sclass, RasterIO = rasterio, proxy = FALSE)
  mzR <- stars::read_stars(mz, RasterIO = rasterio, proxy = FALSE)
  
  ## Makes naming easier
  stratName <- dplyr::sym(names(strataR))

  ## Convert to data.frame and combine
  dat <- as.data.frame(strataR) %>%
    dplyr::rename(strat = !! stratName) %>%
    dplyr::mutate(sc = as.data.frame(sclassR)[,3],
                  mz = as.data.frame(mzR)[,3])
  
  suppressMessages({
    ## Sum up total area in each S-class, this is additive and internally consistent
    totals <- dat %>%
      dplyr::group_by(mz, strat, sc) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::filter(!is.na(strat) & !is.na(sc)) %>%
      dplyr::ungroup()
  })
  
  
  return(totals)
}


