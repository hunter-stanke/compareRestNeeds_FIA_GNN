
##=====================================================
##=====================================================
##
## Determine "forest structural class" following the Haugo et
## al classification scheme. We first predict crown
## width from allometric equations implemented in FVS.
## We then determine percent crown cover and live tree
## density by DBH classes on each FIA plot. Once we
## have the same variables as GNN, we can classify
## FIA plots into one of 7 size classes. Structural
## classifications then follow from size class and 
## total plot canopy cover (predicted).
##
##
## Created:       27 October 2020 - Hunter Stanke
## Last modified: 8 December 2020 - Hunter Stanke
##
##====================================================
##====================================================






## Function to predict stand structural classifications on FIA plots
## -----------------------------------------------------------------------------
classify_plot_structure <- function(dirFIA = here::here('data/FIA/'),
                                    dirFVS = here::here('data/FVS/'),
                                    dirRefCon = here::here('data/refCon/'),
                                    dirResults = here::here('results/FIA/'), 
                                    mapStems = FALSE,
                                    cores = 1){
  
  ## Read some FIA tables
  pnw <- rFIA::readFIA(dirFIA,
                       states = c('OR', 'WA'),
                       common = FALSE,
                       tables = c('PLOT', 'TREE', 'PLOTGEOM'),
                       nCores = cores)
  
  ## Which plots are associated with a current area inventory?
  keepPlts <- read.csv(paste0(dirResults, 'prep/fiaPlts.csv'))
  
  ## Predict crown area of live trees from FVS allometrics
  tree <- predictCrownWidth(pnw, dirFVS, keepPlts, mapStems)
  
  ## Summarize into GNN variables
  gnnVars <- makeGNNvars(tree)
  
  ## Classify plots into S-classes, if we can predict canopy cover
  pltSC <- classifyPlots(gnnVars, dirRefCon, dirResults, cores)
  
  ## If canopy cover is missing, but the plot is in sample, 
  ## predict its sclass
  suppressMessage ({
    preds <- predictMissing(dirFIA, dirResults, pltSC, cores)
  })
  
  
  ## Join it all up on PLOT
  sclassPlt <- pnw$PLOT %>%
    dplyr::mutate(PLT_CN = CN) %>%
    dplyr::filter(PLT_CN %in% unique(keepPlts$PLT_CN)) %>%
    dplyr::select(PLT_CN) %>%
    # Join "observed" s-class
    dplyr::left_join(dplyr::distinct(dplyr::select(pltSC, PLT_CN, sclass)),
                     by = 'PLT_CN') %>%
    dplyr::left_join(dplyr::distinct(preds),
                     by = 'PLT_CN') %>%
    # Update s-class with predictions when we couldn't "observe" it
    dplyr::mutate(sclass.pred = dplyr::case_when(
      is.na(sclass.pred) ~ NA_character_,
      sclass.pred == 1 ~ 'A',
      sclass.pred == 2 ~ 'B',
      sclass.pred == 3 ~ 'C',
      sclass.pred == 4 ~ 'D',
      sclass.pred == 5 ~ 'E')) %>%
    dplyr::mutate(sclass = dplyr::case_when(
      is.na(sclass) ~ as.character(sclass.pred),
      TRUE ~ as.character(sclass))
      ) %>%
    dplyr::select(PLT_CN, sclass)
  
  
  ## Save the results
  write.csv(sclassPlt, 
            paste0(dirResults,'prep/plt_sclass.csv'),
            row.names = FALSE)
  
  cat('S-class assignments complete ...\n')
}






## Predict crown area of live trees from FVS allometrics
predictCrownWidth <- function(db, dirFVS, keepPlts, mapStems) {
  
  ## Read FVS coefficients
  coefFVS <- read.csv(paste0(dirFVS, 'coef.csv')) %>%
    dplyr::distinct(SPCD, variant, .keep_all = TRUE)
  boundsFVS <- read.csv(paste0(dirFVS, 'bounds.csv')) %>%
    dplyr::distinct(SPCD, variant, minD) %>%
    dplyr::filter(minD > 1) # All else assumed 1 inch
  bfFVS <- read.csv(paste0(dirFVS, 'bf.csv')) %>%
    dplyr::distinct(SPCD, variant, location, BF) %>%
    dplyr::filter(BF != 1)
  mergeCoefs <- read.csv(paste0(dirFVS, 'mergeTheseSpecies.csv'))
  
  
  ## Estimate crown width/area for every live tree that we can
  tree <- db$TREE %>%
    ## Select plots in the current area inventory
    dplyr::filter(PLT_CN %in% keepPlts$PLT_CN) %>%
    ## Drop all dead trees
    dplyr::filter(STATUSCD == 1) %>%
    ## Basal area per acre (BAA) of each tree 
    dplyr::mutate(BAA = rFIA:::basalArea(DIA) * TPA_UNADJ) %>% 
    ## Stand-level BAA
    dplyr::group_by(PLT_CN) %>%
    dplyr::mutate(BAA_STAND = sum(BAA, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>%
    ## Here PLT_CN is a unique plot visit ID, TRE_CN is a unique tree visit ID,
    ## DIA is dbh, SPCD is a species code, HT is total tree height,
    ## CR is compacted crown ratio, and TPA_UNAJD is TPA each tree represents
    ## UNADJ refers to non-response bias, which is handled later. Think of it 
    ## as just standard TPA
    dplyr::select(PLT_CN, TRE_CN=CN, SPCD, DIA, HT, CR, BAA_STAND, TPA_UNADJ) %>%
    ## Join on plot attributes
    dplyr::left_join(dplyr::select(db$PLOT, CN, LAT, LON, ELEV), 
                     by = c('PLT_CN' = 'CN')) %>%
    dplyr::left_join(dplyr::select(db$PLOTGEOM, CN, FVS_LOC_CD, FVS_VARIANT), 
                     by = c('PLT_CN' = 'CN')) %>%
    ## We want to drop entire plots where any of these variables are NA
    ## i.e., don't want to compute plot-level canopy cover if individual trees
    ## are ommitted due to lack of allometrics. We will predict S-class of these
    ## plots based on a range of other variables later on.
    dplyr::mutate(cut = ifelse(is.na(SPCD) | SPCD %in% 998:999 |
                                 is.na(DIA) | is.na(HT) | is.na(CR) | is.na(BAA_STAND) |
                                 is.na(LAT) | is.na(LON) | is.na(ELEV) |
                                 is.na(FVS_LOC_CD) | is.na(FVS_VARIANT), 1, 0)) %>%
    dplyr::group_by(PLT_CN) %>%
    dplyr::mutate(cut = ifelse(sum(cut, na.rm = TRUE) > 0, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(cut < 1) %>%
    tidyr::drop_na() %>%
    ## Recode AK to Pacific coast
    dplyr::mutate(FVS_VARIANT = dplyr::case_when(FVS_VARIANT == 'AK' ~ 'PN',
                                                 FVS_VARIANT == 'CI' ~ 'IE',
                                                 TRUE ~ FVS_VARIANT)) %>%
    # Handle merged species -- most species share allometric equations
    dplyr::left_join(dplyr::select(mergeCoefs, -c(Note)),
                     by = c('SPCD', 'FVS_VARIANT')) %>%
    dplyr::mutate(SPCD = dplyr::case_when(is.na(NEW_SPCD) ~ SPCD, 
                                          TRUE ~ NEW_SPCD),
                  FVS_VARIANT = dplyr::case_when(is.na(NEW_FVS_VARIANT) ~ as.character(FVS_VARIANT),
                                                 TRUE ~ as.character(NEW_FVS_VARIANT))) %>%
    # Add coefficients
    dplyr::left_join(coefFVS, by = c('SPCD', 'FVS_VARIANT' = 'variant')) %>%
    dplyr::left_join(boundsFVS, by = c('SPCD', 'FVS_VARIANT' = 'variant')) %>%
    dplyr::left_join(bfFVS, by = c('SPCD', 'FVS_LOC_CD' = 'location', 'FVS_VARIANT' = 'variant')) %>%
    ## If not listed, set equal to one
    dplyr::mutate(BF = dplyr::case_when(is.na(BF) ~ 1.0,
                                        TRUE ~ BF),
                  minD = dplyr::case_when(is.na(minD) ~ 1.0,
                                          TRUE ~ minD))  %>%
    ## Prep our variables for allometric equations
    dplyr::mutate(CL = HT * CR / 100, # Crown length
                  EL100 = ELEV / 100, # Wierd ass parameterization, but that's what FIA does
                  HI = ((ELEV - 5449) / 100) + 4*(LAT - 42.16) + 1.25*(-116.39 - LON), # Hopkins index - I think it's called hopkins
                  BA = BAA_STAND,
                  a1 = dplyr::case_when(is.na(a1) ~ 0, TRUE ~ a1),
                  a2 = dplyr::case_when(is.na(a2) ~ 0, TRUE ~ a2),
                  a3 = dplyr::case_when(is.na(a3) ~ 0, TRUE ~ a3),
                  a4 = dplyr::case_when(is.na(a4) ~ 0, TRUE ~ a4),
                  a5 = dplyr::case_when(is.na(a5) ~ 0, TRUE ~ a5),
                  a6 = dplyr::case_when(is.na(a6) ~ 0, TRUE ~ a6)) %>%
    ## Predict crown width
    dplyr::mutate(cw = dplyr::case_when(
      eq == 1 & DIA >= minD ~ a1 + (a2*DIA) + (a3*(DIA^2)),
      eq == 1 & DIA < minD ~ (a1 + (a2*minD) * (a3 * (minD^2))) * (DIA / minD),
      eq == 2 & DIA >= minD ~ a1 + (a2 * DIA) + (a3 * (DIA^2)) + (a4 * CR) + (a5 * BA) + (a6 *HI),
      eq == 2 & DIA < minD ~ (a1 + (a2 * minD) + (a3 * (minD^2)) + (a4 * CR) + (a5 * BA) + (a6 *HI)) * (DIA / minD),
      eq == 3 & HT >= 15 & SPCD == 264 ~ a1 * (DIA^a2) * (HT^a3) * (CL^a4),
      eq == 3 & HT < 15 & HT >=5 & SPCD == 264 ~ 0.8 * HT * ifelse(CR * .01 > .5, .5, CR*.01),
      eq == 3 & HT < 5 & SPCD == 264 ~ (0.8 * HT * ifelse(CR * .01 > .5, .5, CR*.01)) * (1 - ((HT - 5) * .1)) * a1 * (DIA^a2) * (HT^a3) * (CL^a4) * (HT - 5) * .1,
      eq == 3 & DIA >= minD ~ a1 * exp(a2 + (a3 * log(CL)) + (a4 * log(DIA)) + (a5 * log(HT)) + (a6 * log(BA))),
      eq == 3 & DIA < minD ~ (a1 * exp(a2 + (a3 * log(CL)) + (a4 * log(minD)) + (a5 * log(HT)) + (a6 * log(BA)))) * (DIA / minD),
      eq %in% c(4,6) & DIA >= minD ~ a1 * (DIA^a2),
      eq %in% c(4,6) & DIA < minD ~ a1 * (minD^a2) * (DIA / minD),
      eq == 5 & DIA >= minD ~ (a1 * BF) * (DIA ^ a2) * (HT^a3) * (CL^a4) * ((BA + 1)^a5) * (exp(EL100)^a6),
      eq == 5 & DIA < minD ~ (a1 * BF) * (minD ^ a2) * (HT^a3) * (CL^a4) * ((BA + 1)^a5) * (exp(EL100)^a6) * (DIA / minD))) %>%
    ## Convert to crown area, assuming circular crowns
    dplyr::mutate(crownArea = pi * (cw/2)^2) %>%
    dplyr::select(PLT_CN, TRE_CN, BAA_STAND, TPA_UNADJ, DIA, crownWidth = cw, crownArea)
  
  return(tree)
}


## Estimate tree canopy cover and density by diameter classes at each plot,
## adjusting canopy cover for overlap assuming random tree placement. Completely
## bogus, but again, that's what FVS does.
makeGNNvars <- function(tree) {
  
  gnnVars <- tree %>%
    dplyr::group_by(PLT_CN, BAA_STAND) %>%
    dplyr::mutate(DIA_PERC = dplyr::percent_rank(DIA),
                  n = dplyr::n(),
                  DIA_PERC = dplyr::case_when(n > 1 ~ DIA_PERC,
                                              TRUE ~ 1.0)) %>%
    ungroup() %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(PLT_CN, BAA_STAND) %>%
    ## All trees listed are live, see above
    dplyr::summarize(
      TPA_0_5 = sum(TPA_UNADJ[DIA < 5], na.rm = TRUE),
      TPA_5_10 = sum(TPA_UNADJ[DIA >= 5 & DIA < 10], na.rm = TRUE),
      TPA_10_15 = sum(TPA_UNADJ[DIA >= 10 & DIA < 15], na.rm = TRUE),
      TPA_15_20 = sum(TPA_UNADJ[DIA >= 15 & DIA < 20], na.rm = TRUE),
      TPA_20_30 = sum(TPA_UNADJ[DIA >= 20 & DIA < 29], na.rm = TRUE),
      TPA_GE_5 = sum(TPA_UNADJ[DIA >= 5], na.rm = TRUE),
      TPA_GE_10 = sum(TPA_UNADJ[DIA >= 10], na.rm = TRUE),
      TPA_GE_15 = sum(TPA_UNADJ[DIA >= 15], na.rm = TRUE),
      TPA_GE_30 = sum(TPA_UNADJ[DIA >= 30], na.rm = TRUE),
      TPA_ALL = sum(TPA_UNADJ, na.rm = TRUE),
      CC_0_5 = sum(TPA_UNADJ[DIA < 5] * crownArea[DIA < 5], na.rm = TRUE) * 100 / 43560,
      CC_5_10 = sum(TPA_UNADJ[DIA >= 5 & DIA < 10] * crownArea[DIA >= 5 & DIA < 10], na.rm = TRUE) * 100 / 43560,
      CC_10_15 = sum(TPA_UNADJ[DIA >= 10 & DIA < 15] * crownArea[DIA >= 10 & DIA < 15], na.rm = TRUE) * 100 / 43560,
      CC_15_20 = sum(TPA_UNADJ[DIA >= 15 & DIA < 20] * crownArea[DIA >= 15 & DIA < 20], na.rm = TRUE) * 100 / 43560,
      CC_20_30 = sum(TPA_UNADJ[DIA >= 20 & DIA < 30] * crownArea[DIA >= 20 & DIA < 30], na.rm = TRUE) * 100 / 43560,
      CC_GE_5 = sum(TPA_UNADJ[DIA >= 5] * crownArea[DIA >= 5], na.rm = TRUE) * 100 / 43560,
      CC_GE_10 = sum(TPA_UNADJ[DIA >= 10] * crownArea[DIA >= 10], na.rm = TRUE) * 100 / 43560,
      CC_GE_15 = sum(TPA_UNADJ[DIA >= 15] * crownArea[DIA >= 15], na.rm = TRUE) * 100 / 43560,
      CC_GE_30 = sum(TPA_UNADJ[DIA >= 30] * crownArea[DIA >= 30], na.rm = TRUE) * 100 / 43560,
      CC_ALL = sum(TPA_UNADJ * crownArea, na.rm = TRUE) * 100 / 43560,
      QMD_20 = 4/pi * sum(rFIA:::basalArea(DIA[DIA_PERC >=.8]) * TPA_UNADJ[DIA_PERC >=.8], na.rm = TRUE) / sum(TPA_UNADJ[DIA_PERC >=.8], na.rm = TRUE)
    ) %>%
    ## Correct for canopy overlap, bogus, but how FVS does it
    dplyr::mutate_at(.vars = dplyr::vars(CC_0_5:CC_ALL), function(x) {100 * (1- exp(-.01 * x))}) %>%
    ## If QMD is NA, then all DIA were the same and PERC_RANK flagged all as 0
    ## So, we say they are all in the top 20
    dplyr::mutate(QMD_20 = dplyr::case_when(
      is.na(QMD_20) ~ sqrt(4/ pi * BAA_STAND / TPA_ALL) * 12,
      TRUE ~ sqrt(QMD_20) * 12)
    ) %>%
    as.data.frame() %>% # Force data.table eval via dtplyr
    dplyr::rowwise() %>%
    dplyr::mutate(MAX_CC = max(CC_0_5:CC_20_30, na.rm = TRUE),
                  MAX_CC2 = max(CC_0_5:CC_15_20, na.rm = TRUE))
  

  return(gnnVars)
}


## Classify plots into one of 7 size classes, then one of 5 structure classes
classifyPlots <- function(gnnVars, dirRefCon, dirResults, cores) {
  
  ## Plot attributes for strata assignments and PVT
  pltAtt <- read.csv(paste0(dirResults, 'prep/fiaPlts.csv')) %>%
    select(PLT_CN, pltID, PLOT_STATUS_CD) %>%
    left_join(read.csv(paste0(dirResults, 'prep/fiaPlts_attributes.csv')),
              by = 'pltID')
  
  ## Get TPH thresholds for large size classes, thresholds vary by pvt
  th_size <- read.csv(paste0(dirRefCon, 'TH_Summary.csv'))
  
  ## Get S-class thresholds, varies by BPS
  sclassRules <- read.csv(paste0(dirRefCon, 'sclass_thresholds.txt'))
  
  
  ## Join thresholds and plot attributes onto our GNN variables table
  gnnVars <- gnnVars %>%
    dplyr::left_join(pltAtt, by = 'PLT_CN') %>%
    dplyr::mutate(PVTCode = paste(MAP_ZONE, PVT, sep = '_')) %>%
    dplyr::left_join(dplyr::select(th_size, PVTCode, Size_TH), by = 'PVTCode') %>%
    dplyr::mutate(Size_TH = dplyr::case_when(is.na(Size_TH) ~  integer(1), 
                                             TRUE ~ Size_TH)) %>%
    ## Dropping forested locations when they fall in "non-forest" strata
    ## Update once we have real plot locations
    dplyr::filter(PLOT_STATUS_CD == 1 & !is.na(BPS_LLID))
  
  
  ## Run the size-class classifier
  pltSC <- gnnVars %>%
    dplyr::mutate(sizeClass = dplyr::case_when(
      CC_ALL < 10 ~ 1.0,
      TPA_GE_30 > Size_TH & CC_GE_30 > 10 ~ 7.0,
      TPA_20_30 > Size_TH & CC_20_30 > 10 ~ 6.0,
      TPA_20_30 + TPA_GE_30 > Size_TH & CC_20_30 + CC_GE_30 > 10 ~ 6.0,
      CC_15_20 == MAX_CC ~ 5.0,
      CC_10_15 == MAX_CC ~ 4.0,
      CC_5_10 == MAX_CC & CC_GE_10 > CC_5_10 & CC_10_15 > CC_GE_15 ~ 4.0,
      CC_5_10 == MAX_CC & CC_GE_10 > CC_5_10 & CC_10_15 <= CC_GE_15 ~ 5.0,
      CC_5_10 == MAX_CC ~ 3.0,
      CC_GE_5 > MAX_CC2 & CC_GE_10 >= CC_5_10 & CC_10_15 >= CC_15_20 ~ 4.0,
      CC_GE_5 > MAX_CC2 & CC_GE_10 >= CC_5_10 & CC_10_15 < CC_15_20 ~ 5.0,
      CC_GE_5 > MAX_CC2 & CC_GE_10 < CC_5_10 ~ 3.0,
      CC_0_5 == MAX_CC & CC_GE_5 >= CC_0_5 ~ 3.0,
      CC_0_5 == MAX_CC & CC_GE_5 < CC_0_5 ~ 2.0,
      CC_0_5 == MAX_CC ~ 2.0,
      CC_5_10 == MAX_CC ~ 3.0,
      CC_10_15 == MAX_CC ~ 4.0,
      CC_15_20 == MAX_CC ~ 5.0,
      TRUE ~ NA_real_)
      )
  
  ## Iterate over plots so we can parellize the classification
  pltList <- split(pltSC, pltSC$PLT_CN)
  
  
  ## Run in parallel - sorry for confusing if else here. Parallel is weird on Windows
  if (Sys.info()['sysname'] == 'Windows'){
    cl <- makeCluster(nCores)
    clusterEvalQ(cl, {
      library(dplyr)
      })
    out <- parLapply(cl, X = names(pltList), fun = assignSClass, pltList,
                    sclassRules)
    stopCluster(cl) # Kill the cluster
    
  } else { # Multicore systems
    out <- parallel::mclapply(X = names(pltList), FUN = assignSClass, pltList,
                              sclassRules, mc.cores = cores)
  }
  

  pltSC$sclass <- dplyr::bind_rows(out)$SClass
  
  return(pltSC)
}

## Clunky as hell, but it works, so whatever
assignSClass <- function(x, pltList, sclassRules) {
  
  ## Literally just subset until we're down to one
  dat <- sclassRules %>%
    dplyr::filter(as.character(BPS_ID) == as.character(pltList[[x]]$BpS)) %>%
    dplyr::filter(SizeClass_max >= pltList[[x]]$sizeClass) %>%
    dplyr::filter(SizeClass_min <= pltList[[x]]$sizeClass) %>%
    dplyr::filter(Cover_max >= round(pltList[[x]]$CC_ALL, 1)) %>%
    dplyr::filter(Cover_min <= round(pltList[[x]]$CC_ALL, 1))
  
  return(dat)
}


## When we can't predict canopy cover at a plot, due to missing allometrics, 
## or something else, predict S-class as a function of TPA, QMD of largest 20% 
## of trees, and diameter class diversity (biomass)
## Using knn as the predictive method, very efficient and excellent accuracy
## in this case
predictMissing <- function(dirFIA, dirResults, pltSC, cores) {
  
  ## Set up remote FIA database to compute predictors
  db <- rFIA::readFIA(dirFIA, states = c('OR', 'WA'), 
                      nCores = cores)
  
  ## Setting up some predictors -------------
  
  ## TPA and BAA for all FIA plots
  pltTPA <- rFIA::tpa(db, byPlot = TRUE, nCores = cores)
  
  ## TPA and BAA of top 20% of trees by diameter class
  db$TREE <- db$TREE %>%
    dplyr::group_by(PLT_CN) %>%
    dplyr::mutate(DIA_PERC = dplyr::percent_rank(DIA),
                  n = dplyr::n(),
                  DIA_PERC = dplyr::case_when(n > 1 ~ DIA_PERC,
                                              TRUE ~ 1.0)) %>%
    dplyr::ungroup()
  
  ## TPA of largest 20% of trees
  pltTPA20 <- rFIA::tpa(db, byPlot = TRUE,
                        treeDomain = DIA_PERC >= .8,
                        nCores = cores)
  
  
  ## Diversity of biomass across size strata for all FIA plots
  db$TREE$sc5 <- rFIA::makeClasses(db$TREE$DIA, interval = 5)
  pltDiv <- rFIA::diversity(db, byPlot = TRUE,
                            stateVar = DRYBIO_AG*TPA_UNADJ, 
                            grpVar = sc5,
                            nCores = cores)
  
  ## Drop all NA BPS_LLID, estimate QMD, join sclass when available
  pltTPA <- pltTPA %>%
    dplyr::filter(PLOT_STATUS_CD == 1) %>%
    dplyr::left_join(read.csv(paste0(dirResults, 'prep/fiaPlts_attributes.csv')), by = 'pltID') %>%
    dplyr::select(PLT_CN, BpS_Code, BPS_LLID, TPA, BAA) %>%
    ## Average Tree size,  QMD
    dplyr::mutate(QMD = sqrt(BAA / TPA * 4 / pi) * 12,
                  QMD = dplyr::case_when(is.na(QMD) ~ 0, TRUE ~ QMD),
                  BPS = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,1]) %>%
    ## Join sclass
    dplyr::left_join(dplyr::select(pltSC, PLT_CN, sclass), by = 'PLT_CN') %>%
    dplyr::left_join(dplyr::select(pltDiv, PLT_CN, H, S, Eh), by = 'PLT_CN') %>%
    dplyr::left_join(dplyr::select(pltTPA20, PLT_CN, TPA, BAA), by = 'PLT_CN', suffix = c('', '20')) %>%
    dplyr::mutate(sclass = as.factor(sclass),
                  QMD20 = sqrt(BAA20 / TPA20 * 4 / pi) * 12,
                  QMD20 = dplyr::case_when(is.na(QMD20) ~ 0, TRUE ~ QMD20),
                  Eh = tidyr::replace_na(Eh, 0)) %>%
    dplyr::select(-c(BAA, BAA20)) %>%
    dplyr::distinct()
  
  
  
  
  ## Impute missing sclass by BPS -----------------------------------

  ## Which BPSs have missing values?
  miss <- pltTPA  %>%
    dplyr::ungroup() %>%
    dplyr::group_by(BpS_Code) %>%
    dplyr::summarize(n = dplyr::n(),
                     nna = length(which(is.na(sclass)))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(nna > 0) %>%
    tidyr::drop_na()
  
  predList <- list()
  ## Building a seperate "model" for each forest type group
  for (i in 1:nrow(miss)){
    
    ## Break into reference and target set and predict
    ## sclass with knn, distance defined by trees in random forest algorithm
    x <- pltTPA %>%
      dplyr::filter(BpS_Code == miss$BpS_Code[i]) %>%
      ## yaImpute references observations based on row names.
      ## tibble hates that idea, so we have to watch really closely when
      ## we use yaImpute and dplyr/tibble together
      tibble::column_to_rownames('PLT_CN') %>%
      dplyr::select(TPA, QMD, TPA20, QMD20, H, Eh)
    y <- pltTPA %>%
      dplyr::filter(BpS_Code == miss$BpS_Code[i]) %>%
      dplyr::filter(!is.na(sclass)) %>%
      ## Resetting levels
      dplyr::mutate(sclass = factor(sclass)) %>%
      tibble::column_to_rownames('PLT_CN') %>%
      dplyr::select(sclass)
    
    ## If we've got nothing, skip it
    if (nrow(y) < 0) next
    
    ## Need at least two groups to do classification
    if (length(unique(y$sclass)) > 2){
      
      ## Find nearest neighbors
      rf <- yaImpute::yai(x = x, y = y,
                          method = 'randomForest',
                          k =  ifelse(miss$n[i] < 15, miss$n[i]-1, 15))
      
      newdata = pltTPA %>%
        dplyr::filter(BpS_Code == miss$BpS_Code[i] & is.na(sclass)) %>%
        tibble::column_to_rownames('PLT_CN')
      
      ## Predict s-class at forested plots where we couldn't calculate it
      preds <- yaImpute::predict.yai(rf, newdata,
                                     method.factor = 'median')
      preds <- preds %>%
        dplyr::mutate(PLT_CN = as.numeric(row.names(.))) %>%
        tibble::as_tibble() %>%
        ## IF TPA is 0, automatically sclass 1 (recently disturbed is what I assume)
        dplyr::mutate(sclass = as.numeric(as.character(sclass)),
                      sclass = dplyr::case_when(TPA == 0 ~ 1, TRUE ~ sclass)) %>%
        dplyr::select(PLT_CN, sclass.pred = sclass)
    } else {
      
      ## if only one class observed in biophysical setting, predict that class everywhere
      preds <- pltTPA %>%
        dplyr::filter(BpS_Code == miss$BpS_Code[i] & is.na(sclass)) %>%
      preds$sclass.pred <- unique(y$sclass)
      preds <- preds[,c('PLT_CN', 'sclass.pred')]
    }
    
    
    ## Handle factor/ double/ levels issues
    preds$sclass.pred <- as.numeric(as.character(preds$sclass.pred))
    
    predList[[i]] <- preds
    
  }
  
  ## Combine and handle factors
  preds <- dplyr::bind_rows(predList) %>%
    dplyr::select(PLT_CN, sclass.pred) %>%
    dplyr::mutate(sclass.pred = factor(sclass.pred))
  
  
  return(preds)
  
}





