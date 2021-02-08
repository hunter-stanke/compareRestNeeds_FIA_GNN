##=====================================================
##=====================================================
##
## Estimate annual change in forest land area by
## S-class within various domains using the FIADB alone.
## Using rFIA's areaChange function to compute totals,
## ratios, and associated variances. 
##
## Created:       30 October 2020 - Hunter Stanke
## Last modified: 7 January 2020 - Hunter Stanke
##
##====================================================
##====================================================




## Wrapper around rFIA to estimate land area by S-class within unique domains
## -----------------------------------------------------------------------------
## dirFIA (character):     directory where project FIA data are stored
## dirResults (character): directory where project results are stored
## cores (numeric):        number of physical cores to use 
estimate_sclass_area_change <- function(dirFIA = here::here('data/FIA/'),
                                        dirResults = here::here('results/FIA/'),
                                        cores = 1) {
  
  ## Prep FIA data -------------------------------------------------------------
  
  ## Read our FIA Data
  pnw <- rFIA::readFIA(dirFIA, states = c('OR', 'WA'), nCores = cores)
  
  # plot-level structure classifications
  sclass <- read.csv(paste0(dirResults, '/prep/plt_sclass.csv'))
  
  # Landscape unit attributes
  bpsAtt <- read.csv(paste0(dirResults, '/prep/fiaPlts_attributes.csv')) %>%
    dplyr::select(pltID, BPS_LLID, BpS, BpS_Code, 
                  BpS_Name, FRG, HUC10, HUC8, MAP_ZONE) %>%
    distinct()
  
  ## Need to use S-class codes (i.e, 'A', 'B', etc.), appending to PLOT table
  pnw$COND <- pnw$COND %>%  
    dplyr::left_join(sclass, by = c('PLT_CN', 'CONDID'))
  pnw$PLOT <- pnw$PLOT %>%
    dplyr::mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
    ## Add other plot attributes
    dplyr::left_join(bpsAtt, by = c('pltID'))
  
  
  
  
  ## Net change ------------------------------------------------
  
  ## Match OR and WA 2010 & 2011 inventories
  pnw$POP_EVAL <- pnw$POP_EVAL %>%
    dplyr::mutate(END_INVYR = dplyr::case_when(
      END_INVYR == 2010 ~ 2011,
      TRUE ~ as.double(END_INVYR)
    ))
  
  # Across entire region
  full <- rFIA::areaChange(pnw, 
                           grpBy = sclass,
                           nCores = cores,
                           areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')) & !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'ti',
                           chngType = 'net') 
  
  # Within BpS
  bps <- rFIA::areaChange(pnw, 
                          grpBy = c(BpS_Code, sclass),
                          nCores = cores,
                          areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')) & !is.na(BpS_Code),
                          variance = TRUE,
                          method = 'ti',
                          chngType = 'net')
  
  
  # Within BPS_LLID
  bpsLLID <- rFIA::areaChange(pnw, 
                              grpBy = c(BpS_Code, BPS_LLID, sclass),
                              nCores = cores,
                              areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')) & !is.na(BpS_Code),
                              variance = TRUE,
                              method = 'ti',
                              chngType = 'net')
  
  # Within BPS & map zone
  bpsMZ <- rFIA::areaChange(pnw, 
                            grpBy = c(BpS_Code, MAP_ZONE, sclass),
                            nCores = cores,
                            areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')) & !is.na(BpS_Code),
                            variance = TRUE,
                            method = 'ti',
                            chngType = 'net')
  
  ## Save results --------------------------------------------------------------
  write.csv(full, paste0(dirResults, '/sclassChng/ORWA_net.csv'), row.names = FALSE)
  write.csv(bps, paste0(dirResults, '/sclassChng/BPS_net.csv'), row.names = FALSE)
  write.csv(bpsLLID, paste0(dirResults, '/sclassChng/BPS_LLID_net.csv'), row.names = FALSE)
  write.csv(bpsMZ, paste0(dirResults, '/sclassChng/BPS_MAPZONE_net.csv'), row.names = FALSE)
  
  
  
  ## Component change ------------------------------------------------
  
  # Across entire region
  full <- rFIA::areaChange(pnw, 
                           grpBy = sclass,
                           nCores = cores,
                           areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')) & !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'ti',
                           chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  # Within BpS
  bps <- rFIA::areaChange(pnw, 
                          grpBy = c(BpS_Code, sclass),
                          nCores = cores,
                          areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')) & !is.na(BpS_Code),
                          variance = TRUE,
                          method = 'ti',
                          chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  
  
  # Within BPS_LLID
  bpsLLID <- rFIA::areaChange(pnw, 
                              grpBy = c(BpS_Code, BPS_LLID, sclass),
                              nCores = cores,
                              areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')) & !is.na(BpS_Code),
                              variance = TRUE,
                              method = 'ti',
                              chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  
  # Within BPS & map zone
  bpsMZ <- rFIA::areaChange(pnw, 
                            grpBy = c(BpS_Code, MAP_ZONE, sclass),
                            nCores = cores,
                            areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')) & !is.na(BpS_Code),
                            variance = TRUE,
                            method = 'ti',
                            chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  
  ## Save results --------------------------------------------------------------
  write.csv(full, paste0(dirResults, '/sclassChng/ORWA_comp.csv'), row.names = FALSE)
  write.csv(bps, paste0(dirResults, '/sclassChng/BPS_comp.csv'), row.names = FALSE)
  write.csv(bpsLLID, paste0(dirResults, '/sclassChng/BPS_LLID_comp.csv'), row.names = FALSE)
  write.csv(bpsMZ, paste0(dirResults, '/sclassChng/BPS_MAPZONE_comp.csv'), row.names = FALSE)
  
  cat('Area estimates complete ...\n')
}

