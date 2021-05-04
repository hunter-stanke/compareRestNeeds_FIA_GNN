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
                  BpS_Name, FRG, MAP_ZONE) %>%
    distinct() %>%
    mutate(ew = case_when(is.na(MAP_ZONE) ~ NA_character_,
                          MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') ~ 'westside',
                          TRUE ~ 'eastside'))
  
  ## Need to use S-class codes (i.e, 'A', 'B', etc.), appending to PLOT table
  pnw$COND <- pnw$COND %>%  
    dplyr::left_join(sclass, by = c('PLT_CN', 'CONDID'))
  pnw$PLOT <- pnw$PLOT %>%
    dplyr::mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
    ## Add other plot attributes
    dplyr::left_join(bpsAtt, by = c('pltID'))  %>%
    dplyr::mutate(STATE = case_when(STATECD == 53 ~ 'Washington',
                                    STATECD == 41 ~ 'Oregon',
                                    TRUE ~ NA_character_))
  
  
  
  
  ## Net change ------------------------------------------------
  
  ## Match OR and WA 2010 & 2011 inventories
  pnw$POP_EVAL <- pnw$POP_EVAL %>%
    dplyr::mutate(END_INVYR = dplyr::case_when(
      END_INVYR == 2010 ~ 2011,
      TRUE ~ as.double(END_INVYR)
    ))
  
  # Across entire region
  full <- rFIA::areaChange(pnw, 
                           grpBy = c(sclass),
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'ti',
                           chngType = 'net') 
  
  # By state
  state <- rFIA::areaChange(pnw, 
                           grpBy = c(STATE, sclass),
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'ti',
                           chngType = 'net') 
  # Eastside/ westside
  ew <- rFIA::areaChange(pnw, 
                           grpBy = c(ew, sclass),
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'ti',
                           chngType = 'net') 
  
  # Eastside/ westside + state
  ewState <- rFIA::areaChange(pnw, 
                           grpBy = c(STATE, ew, sclass),
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'ti',
                           chngType = 'net') 
  
  # Within BpS
  bps <- rFIA::areaChange(pnw, 
                          grpBy = c(ew, BpS_Code, sclass),
                          nCores = cores,
                          areaDomain = !is.na(BpS_Code),
                          variance = TRUE,
                          method = 'ti',
                          chngType = 'net')
  
  # Within BpS + state
  bpsState <- rFIA::areaChange(pnw, 
                          grpBy = c(STATE, ew, BpS_Code, sclass),
                          nCores = cores,
                          areaDomain = !is.na(BpS_Code),
                          variance = TRUE,
                          method = 'ti',
                          chngType = 'net')
  
  # Within BPS_LLID
  bpsLLID <- rFIA::areaChange(pnw, 
                              grpBy = c(ew, BpS_Code, BPS_LLID, sclass),
                              nCores = cores,
                              areaDomain = !is.na(BpS_Code),
                              variance = TRUE,
                              method = 'ti',
                              chngType = 'net')
  
  # Within BPS_LLID + STATE
  bpsLLIDState <- rFIA::areaChange(pnw, 
                              grpBy = c(STATE, ew, BpS_Code, BPS_LLID, sclass),
                              nCores = cores,
                              areaDomain = !is.na(BpS_Code),
                              variance = TRUE,
                              method = 'ti',
                              chngType = 'net')
  
  ## Save results --------------------------------------------------------------
  write.csv(full, paste0(dirResults, '/sclassChng/ORWA_net.csv'), row.names = FALSE)
  write.csv(state, paste0(dirResults, '/sclassChng/STATE_net.csv'), row.names = FALSE)
  write.csv(ew, paste0(dirResults, '/sclassChng/ORWA_EW_net.csv'), row.names = FALSE)
  write.csv(ewState, paste0(dirResults, '/sclassChng/STATE_EW_net.csv'), row.names = FALSE)
  write.csv(bps, paste0(dirResults, '/sclassChng/ORWA_BPS_net.csv'), row.names = FALSE)
  write.csv(bpsState, paste0(dirResults, '/sclassChng/STATE_BPS_net.csv'), row.names = FALSE)
  write.csv(bpsLLID, paste0(dirResults, '/sclassChng/ORWA_BPS_LLID_net.csv'), row.names = FALSE)
  write.csv(bpsLLIDState, paste0(dirResults, '/sclassChng/STATE_BPS_LLID_net.csv'), row.names = FALSE)
  
  
  ## Component change ------------------------------------------------
  
  # Across entire region
  full <- rFIA::areaChange(pnw, 
                           grpBy = c(sclass),
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'ti',
                           chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  # By state
  state <- rFIA::areaChange(pnw, 
                           grpBy = c(STATE, sclass),
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'ti',
                           chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  # Eastside/ westside
  ew <- rFIA::areaChange(pnw, 
                           grpBy = c(ew, sclass),
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'ti',
                           chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  # Eastside/ westside + state
  ewState <- rFIA::areaChange(pnw, 
                           grpBy = c(STATE, ew, sclass),
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'ti',
                           chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  # Within BpS
  bps <- rFIA::areaChange(pnw, 
                          grpBy = c(ew, BpS_Code, sclass),
                          nCores = cores,
                          areaDomain = !is.na(BpS_Code),
                          variance = TRUE,
                          method = 'ti',
                          chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  # Within BpS + state
  bpsState <- rFIA::areaChange(pnw, 
                          grpBy = c(STATE, ew, BpS_Code, sclass),
                          nCores = cores,
                          areaDomain = !is.na(BpS_Code),
                          variance = TRUE,
                          method = 'ti',
                          chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  # Within BPS_LLID
  bpsLLID <- rFIA::areaChange(pnw, 
                              grpBy = c(ew, BpS_Code, BPS_LLID, sclass),
                              nCores = cores,
                              areaDomain = !is.na(BpS_Code),
                              variance = TRUE,
                              method = 'ti',
                              chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  # Within BPS_LLID + state
  bpsLLIDState <- rFIA::areaChange(pnw, 
                              grpBy = c(STATE, ew, BpS_Code, BPS_LLID, sclass),
                              nCores = cores,
                              areaDomain = !is.na(BpS_Code),
                              variance = TRUE,
                              method = 'ti',
                              chngType = 'component') %>%
    ## Dropping components that were outside domain of interest at both measurements
    dplyr::filter(AREA_DOMAIN1 + AREA_DOMAIN2 > 0) %>%
    dplyr::filter(!is.na(AREA_DOMAIN1))
  
  
  
  ## Save results --------------------------------------------------------------
  write.csv(full, paste0(dirResults, '/sclassChng/ORWA_comp.csv'), row.names = FALSE)
  write.csv(state, paste0(dirResults, '/sclassChng/STATE_comp.csv'), row.names = FALSE)
  write.csv(ew, paste0(dirResults, '/sclassChng/ORWA_EW_comp.csv'), row.names = FALSE)
  write.csv(ewState, paste0(dirResults, '/sclassChng/STATE_EW_comp.csv'), row.names = FALSE)
  write.csv(bps, paste0(dirResults, '/sclassChng/ORWA_BPS_comp.csv'), row.names = FALSE)
  write.csv(bpsState, paste0(dirResults, '/sclassChng/STATE_BPS_comp.csv'), row.names = FALSE)
  write.csv(bpsLLID, paste0(dirResults, '/sclassChng/ORWA_BPS_LLID_comp.csv'), row.names = FALSE)
  write.csv(bpsLLIDState, paste0(dirResults, '/sclassChng/STATE_BPS_LLID_comp.csv'), row.names = FALSE)
  
  cat('Area estimates complete ...\n')
}

                                                                                                                                                     
