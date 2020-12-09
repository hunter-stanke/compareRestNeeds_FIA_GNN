##=====================================================
##=====================================================
##
## Estimate total forest land area by S-class within
## various domains using the FIADB alone. Using rFIA's
## area function to compute totals, ratios, and 
## associated variances. 
##
## Created:       30 October 2020 - Hunter Stanke
## Last modified: 9 December 2020 - Hunter Stanke
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
  sclass <- read.csv(paste0(dirResults, 'prep/plt_sclass.csv'))
  
  # Landscape unit attributes
  bpsAtt <- read.csv(paste0(dirResults, 'prep/fiaPlts_attributes.csv')) %>%
    dplyr::left_join(read.csv(paste0(dirResults, 'prep/fiaPlts.csv')), by = 'pltID') %>%
    dplyr::select(PLT_CN, pltID, BPS_LLID, BpS, BpS_Code, 
                  BpS_Name, FRG, HUC10, HUC8, MAP_ZONE)
  
  ## Need to use S-class codes (i.e, 'A', 'B', etc.), appending to PLOT table
  pnw$PLOT <- pnw$PLOT %>%
    dplyr::left_join(sclass, by = c('CN' = 'PLT_CN')) %>%
    ## Add other plot attributes
    dplyr::left_join(bpsAtt, by = c('CN' = 'PLT_CN'))
  
  
  
  ## Use annual estimator first ------------------------------------------------
  # Across entire region
  full <- rFIA::area(pnw, 
                     grpBy = sclass,
                     nCores = cores,
                     areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')),
                     variance = TRUE,
                     method = 'annual') %>%
    dplyr::filter(YEAR > 2001) # Only in OR
  
  # Within BpS
  bps <- rFIA::area(pnw, 
                    grpBy = c(BpS_Code, sclass),
                    nCores = cores,
                    areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')),
                    variance = TRUE,
                    method = 'annual') %>%
    dplyr::filter(YEAR > 2001) # Only in OR

  
  # Within BPS_LLID
  bpsLLID <- rFIA::area(pnw, 
                        grpBy = c(BpS_Code, BPS_LLID, sclass),
                        nCores = cores,
                        areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')),
                        variance = TRUE,
                        method = 'annual') %>%
    dplyr::filter(YEAR > 2001) # Only in OR
  
  # Within BPS & map zone
  bpsMZ <- rFIA::area(pnw, 
                      grpBy = c(BpS_Code, MAP_ZONE, sclass),
                      nCores = cores,
                      areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')),
                      variance = TRUE,
                      method = 'annual') %>%
    dplyr::filter(YEAR > 2001) # Only in OR
  
  ## Save results --------------------------------------------------------------
  write.csv(full, paste0(dirResults, 'sclass/ORWA_annual.csv'), row.names = FALSE)
  write.csv(bps, paste0(dirResults, 'sclass/BPS_annual.csv'), row.names = FALSE)
  write.csv(bpsLLID, paste0(dirResults, 'sclass/BPS_LLID_annual.csv'), row.names = FALSE)
  write.csv(bpsMZ, paste0(dirResults, 'sclass/BPS_MAPZONE_annual.csv'), row.names = FALSE)
  
  
  
  
  ## Now use temporally indifferent ------------------------------------------------
  
  ## Match OR and WA 2010 & 2011 inventories
  pnw$POP_EVAL <- pnw$POP_EVAL %>%
    dplyr::mutate(END_INVYR = dplyr::case_when(
      END_INVYR == 2010 ~ 2011,
      TRUE ~ as.double(END_INVYR)
      ))
  
  # Across entire region
  full <- rFIA::area(pnw, 
                     grpBy = sclass,
                     nCores = cores,
                     areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')),
                     variance = TRUE,
                     method = 'ti') 
  
  # Within BpS
  bps <- rFIA::area(pnw, 
                    grpBy = c(BpS_Code, sclass),
                    nCores = cores,
                    areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')),
                    variance = TRUE,
                    method = 'ti')
  
  
  # Within BPS_LLID
  bpsLLID <- rFIA::area(pnw, 
                        grpBy = c(BpS_Code, BPS_LLID, sclass),
                        nCores = cores,
                        areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')),
                        variance = TRUE,
                        method = 'ti')
  
  # Within BPS & map zone
  bpsMZ <- rFIA::area(pnw, 
                      grpBy = c(BpS_Code, MAP_ZONE, sclass),
                      nCores = cores,
                      areaDomain = !c(MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC')),
                      variance = TRUE,
                      method = 'ti')
  
  ## Save results --------------------------------------------------------------
  write.csv(full, paste0(dirResults, 'sclass/ORWA_ti.csv'), row.names = FALSE)
  write.csv(bps, paste0(dirResults, 'sclass/BPS_ti.csv'), row.names = FALSE)
  write.csv(bpsLLID, paste0(dirResults, 'sclass/BPS_LLID_ti.csv'), row.names = FALSE)
  write.csv(bpsMZ, paste0(dirResults, 'sclass/BPS_MAPZONE_ti.csv'), row.names = FALSE)
  
  cat('Area estimates complete ...\n')
}

