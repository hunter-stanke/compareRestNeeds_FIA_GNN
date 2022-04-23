##=====================================================
##=====================================================
##
## Estimate total forest land area by S-class within
## various domains using the FIADB alone. Using rFIA's
## area function to compute totals, ratios, and 
## associated variances. 
##
## Created:       30 October 2020 - Hunter Stanke
## Last modified: 8 June 2021 - Hunter Stanke
##
##====================================================
##====================================================



## Wrapper around rFIA to estimate land area by S-class within unique domains
## -----------------------------------------------------------------------------
## dirFIA (character):     directory where project FIA data are stored
## dirResults (character): directory where project results are stored
## cores (numeric):        number of physical cores to use 
estimate_sclass_area <- function(dirFIA = here::here('data/FIA/'),
                                 dirResults = here::here('results/FIA/'),
                                 cores = 1) {
  
  ## Prep FIA data -------------------------------------------------------------
  
  ## Read our FIA Data
  pnw <- rFIA::readFIA(dirFIA, states = c('OR', 'WA'), nCores = cores)
  
  # plot-level structure classifications
  sclass <- read.csv(paste0(dirResults, '/prep/plt_sclass.csv'))
  
  # Landscape unit attributes
  bpsAtt <- read.csv(paste0(dirResults, '/prep/fiaPlts_attributes.csv')) %>%
    dplyr::left_join(read.csv(paste0(dirResults, '/prep/fiaPlts.csv')), by = 'pltID') %>%
    dplyr::select(PLT_CN, pltID, BPS_LLID, BpS, BpS_Code, 
                  BpS_Name, FRG, MAP_ZONE) %>%
    mutate(ew = case_when(is.na(MAP_ZONE) ~ NA_character_,
                          MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') ~ 'westside',
                          TRUE ~ 'eastside'))
  
  ## Need to use S-class codes (i.e, 'A', 'B', etc.), appending to PLOT table
  pnw$COND <- pnw$COND %>%  
    dplyr::left_join(sclass, by = c('PLT_CN', 'CONDID'))
  pnw$PLOT <- pnw$PLOT %>%
    ## Add other plot attributes
    dplyr::left_join(bpsAtt, by = c('CN' = 'PLT_CN')) %>%
    dplyr::mutate(STATE = case_when(STATECD == 53 ~ 'Washington',
                                    STATECD == 41 ~ 'Oregon',
                                    TRUE ~ NA_character_))
  
  
  
  ## Use annual estimator first ------------------------------------------------
  # Across entire region
  full <- rFIA::area(pnw,
                     grpBy = c(sclass),
                     nCores = cores,
                     areaDomain = !is.na(BpS_Code),
                     variance = TRUE,
                     method = 'annual') %>%
    dplyr::filter(YEAR > 2001) # Only in OR

  # Eastside/ westside
  ew <- rFIA::area(pnw,
                     grpBy = c(ew, sclass),
                     nCores = cores,
                     areaDomain = !is.na(BpS_Code),
                     variance = TRUE,
                     method = 'annual') %>%
    dplyr::filter(YEAR > 2001) # Only in OR


  # Within EW + BpS
  bpsEw <- rFIA::area(pnw,
                           grpBy = c(ew, BpS_Code, sclass),
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'annual') %>%
    dplyr::filter(YEAR > 2001) # Only in OR

  
  ## Save results --------------------------------------------------------------
  write.csv(full, paste0(dirResults, '/sclass/ORWA_annual.csv'), row.names = FALSE)
  write.csv(ew, paste0(dirResults, '/sclass/ORWA_EW_annual.csv'), row.names = FALSE)
  write.csv(bpsEw, paste0(dirResults, '/sclass/ORWA_EW_BPS_annual.csv'), row.names = FALSE)

  
  
  
  ## Total forestland, i.e., not by sclass
  # Across entire region
  full.total <- rFIA::area(pnw,
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'annual') %>%
    dplyr::filter(YEAR > 2001) # Only in OR
  
  
  # Eastside/ westside
  ew.total <- rFIA::area(pnw,
                         grpBy = c(ew),
                         nCores = cores,
                         areaDomain = !is.na(BpS_Code),
                         variance = TRUE,
                         method = 'annual') %>%
    dplyr::filter(YEAR > 2001) # Only in OR
  
  # Within EW + BpS
  bpsEw.total <- rFIA::area(pnw,
                            grpBy = c(ew, BpS_Code),
                            nCores = cores,
                            areaDomain = !is.na(BpS_Code),
                            variance = TRUE,
                            method = 'annual') %>%
    dplyr::filter(YEAR > 2001) # Only in OR
  
  write.csv(full.total, paste0(dirResults, '/totalForest/ORWA_annual.csv'), row.names = FALSE)
  write.csv(ew.total, paste0(dirResults, '/totalForest/ORWA_EW_annual.csv'), row.names = FALSE)
  write.csv(bpsEw.total, paste0(dirResults, '/totalForest/ORWA_EW_BPS_annual.csv'), row.names = FALSE)
  
  
  
  
  ## Now use temporally indifferent ------------------------------------------------
  
  ## Match OR and WA 2010 & 2011 inventories
  pnw$POP_EVAL <- pnw$POP_EVAL %>%
    dplyr::mutate(END_INVYR = dplyr::case_when(
      END_INVYR == 2010 ~ 2011,
      TRUE ~ as.double(END_INVYR)
      ))
  
  # Across entire region
  full <- rFIA::area(pnw, 
                     grpBy = c(sclass),
                     nCores = cores,
                     areaDomain = !is.na(BpS_Code),
                     variance = TRUE,
                     method = 'ti') 
  
  # Eastside/ westside
  ew <- rFIA::area(pnw, 
                   grpBy = c(ew, sclass),
                   nCores = cores,
                   areaDomain = !is.na(BpS_Code),
                   variance = TRUE,
                   method = 'ti') 
  
  # Within EW + BpS
  bpsEw <- rFIA::area(pnw, 
                      grpBy = c(ew, BpS_Code, sclass),
                      nCores = cores,
                      areaDomain = !is.na(BpS_Code),
                      variance = TRUE,
                      method = 'ti') %>%
    dplyr::filter(YEAR > 2001) # Only in OR
  
  
  
  ## Save results --------------------------------------------------------------
  write.csv(full, paste0(dirResults, '/sclass/ORWA_ti.csv'), row.names = FALSE)
  write.csv(ew, paste0(dirResults, '/sclass/ORWA_EW_ti.csv'), row.names = FALSE)
  write.csv(bpsEw, paste0(dirResults, '/sclass/ORWA_EW_BPS_ti.csv'), row.names = FALSE)
  
  
  ## Total forestland, i.e., not by sclass
  # Across entire region
  full.total <- rFIA::area(pnw,
                           nCores = cores,
                           areaDomain = !is.na(BpS_Code),
                           variance = TRUE,
                           method = 'TI')
  
  
  # Eastside/ westside
  ew.total <- rFIA::area(pnw,
                         grpBy = c(ew),
                         nCores = cores,
                         areaDomain = !is.na(BpS_Code),
                         variance = TRUE,
                         method = 'TI') 
  
  # Within EW + BpS
  bpsEw.total <- rFIA::area(pnw,
                            grpBy = c(ew, BpS_Code),
                            nCores = cores,
                            areaDomain = !is.na(BpS_Code),
                            variance = TRUE,
                            method = 'TI')
  
  write.csv(full.total, paste0(dirResults, '/totalForest/ORWA_ti.csv'), row.names = FALSE)
  write.csv(ew.total, paste0(dirResults, '/totalForest/ORWA_EW_ti.csv'), row.names = FALSE)
  write.csv(bpsEw.total, paste0(dirResults, '/totalForest/ORWA_EW_BPS_ti.csv'), row.names = FALSE)
  
  
  cat('Area estimates complete ...\n')
}

