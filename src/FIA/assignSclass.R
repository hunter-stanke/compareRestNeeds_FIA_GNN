
##=====================================================
##=====================================================
##
## Determine "forest structural class" following the Haugo et
## al classification scheme. We first predict crown
## width from allometric equations implemented by GNN.
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
                                    dirCW = here::here('data/CW/'),
                                    dirRefCon = here::here('data/refCon/'),
                                    dirResults = here::here('results/FIA/'), 
                                    mapStems = FALSE,
                                    cores = 1){
  
  ## Read some FIA tables
  pnw <- rFIA::readFIA(dirFIA,
                       states = c('OR', 'WA'),
                       common = FALSE,
                       tables = c('PLOT', 'TREE', 'PLOTGEOM', 'COND',
                                  'POP_PLOT_STRATUM_ASSGN', 'POP_ESTN_UNIT',
                                  'POP_EVAL', 'POP_STRATUM', 'POP_EVAL_TYP',
                                  'POP_EVAL_GRP'),
                       nCores = cores)
  
  ## Which plots are associated with a current area inventory?
  keepPlts <- read.csv(paste0(dirResults, '/prep/fiaPlts.csv')) 
  
  ## Predict crown area of live trees from FVS allometrics
  tree <- predictCrownWidth(pnw, dirCW, keepPlts, mapStems)
  
  ## Summarize into GNN variables
  gnnVars <- makeGNNvars(tree)
  
  ## Classify plots into S-classes, if we can predict canopy cover
  pltSC <- classifyPlots(gnnVars, dirRefCon, dirResults, cores)
  
  ## Get a plot/condition list for all plots in the current area inventory, 
  ## and join on sclass, and live tree TPA/BAA
  a.plts <- rFIA::area(pnw,
                 grpBy = CONDID,
                 byPlot = TRUE,
                 nCores = 10) 
  t.plts <- rFIA::tpa(pnw, 
                grpBy = CONDID,
                byPlot = TRUE,
                nCores = 10)
  
  ## Now adjust s-class assignments for slivers, and for non-treed forestland
  ## 3 plots remaining that we can't make an assignment on, as crown ratio is 
  ## missing for a few trees. 
  sclassPlt  <- a.plts %>%
    ## Cuts non-forest
    dplyr::filter(PERC_AREA > 0) %>%
    ## Join sclass from above
    dplyr::left_join(select(pltSC, PLT_CN, CONDID, sclass), by = c('PLT_CN', 'CONDID')) %>%
    dplyr::left_join(select(t.plts, PLT_CN, CONDID, BAA), by = c('PLT_CN', 'CONDID')) %>%
    ## For conditions that are slivers (i.e., occupy less than 15% of plot area),
    ## we won't be able to accurately assign an sclass. In these cases, assign the
    ## dominant sclass on the plot, if there is another available
    mutate(sliver = case_when(PERC_AREA < .15 ~ 1,
                               TRUE ~ 0)) %>%
    group_by(PLT_CN) %>%
    mutate(n.cond = n()) %>%
    # If no live trees, then early seral
    mutate(sclass = case_when(is.na(BAA) ~ 'A', 
                              BAA == 0 ~ 'A',
                              TRUE ~ sclass)) %>%
    # If sliver assign dominant alternative
    mutate(sclass = case_when(sliver == 1 & n.cond > 1 ~ sclass[which.max(PERC_AREA)],
                              TRUE ~ sclass)) %>%
    ungroup() %>%
    select(PLT_CN, CONDID, sclass)
  
  
  ## Save the results
  write.csv(sclassPlt, 
            paste0(dirResults,'/prep/plt_sclass.csv'),
            row.names = FALSE)
  
  cat('S-class assignments complete ...\n')
}






## Predict crown area of live trees from FVS allometrics
predictCrownWidth <- function(db, dirCW, keepPlts, mapStems) {
  
  ## Read coefficients used in allometric equations below, from Hann 1997
  ## This method is extremely simplistic, but is what is used by GNN, so 
  ## use it here to match. See bottom of this script for a more contemporary
  ## approach, using FVS allometrics
  
  ## Read FVS coefficients
  coefCW <- read.csv(paste0(dirCW, '/canopy_cover_coeff.csv')) 
  
  ## Check if we have FIA's reference table already so we can link NRCS SYMBOL
  ## to FIA SPCD
  if (!any(stringr::str_detect('REF_SPECIES.csv', list.files(dirCW)))) {
    ## Download the reference table if we don't have it already
    rFIA::getFIA('REF', dir = dirCW, tables = 'SPECIES', load = FALSE)
  }
  
  ## Get NRCS PLANT Dictionary symbols and join onto coefficient table
  plantDict <- read.csv(paste0(dirCW, '/REF_SPECIES.csv')) %>%
    dplyr::mutate(SPECIES_SYMBOL = stringr::str_trim (SPECIES_SYMBOL)) %>% # Remove whitespace
    dplyr::select(SPCD, SPP_SYMBOL = SPECIES_SYMBOL)
  coefCW <- coefCW %>%
    mutate(SPP_SYMBOL = stringr::str_trim (SPP_SYMBOL)) %>% # Remove whitespace
    dplyr::left_join(plantDict, by = c('SPP_SYMBOL'))
  
  ## Norway maple and giant chinkapin are present in the FIA Database, but
  ## not in the coefficient list. In FVS norway is treated the same as bigleaf
  ## and chinkapin the same as tanoak. Doing the same here - only affects (0.5% of trees)
  nm <- dplyr::filter(coefCW, SPCD == 312) %>% dplyr::mutate(SPCD = 320)
  gc <- dplyr::filter(coefCW, SPCD == 631) %>% dplyr::mutate(SPCD = 431)
  coefCW <- coefCW %>%
    dplyr::bind_rows(nm) %>%
    dplyr::bind_rows(gc)
  
  ## Estimate crown width/area for every live tree that we can
  tree <- db$COND %>%
    dplyr::filter(PLT_CN %in% keepPlts$PLT_CN) %>%
    dplyr::filter(COND_STATUS_CD == 1) %>%
    dplyr::select(PLT_CN, CONDID) %>%
    dplyr::left_join(db$TREE, by = c('PLT_CN', 'CONDID')) %>%
    ## Drop all dead trees
    dplyr::filter(STATUSCD == 1) %>%
    ## Basal area per acre (BAA) of each tree 
    dplyr::mutate(BAA = rFIA:::basalArea(DIA) * TPA_UNADJ) %>% 
    ## Stand-level BAA
    dplyr::group_by(PLT_CN, CONDID) %>%
    dplyr::mutate(BAA_STAND = sum(BAA, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>%
    ## Is the tree dominant, codominant or open grown?
    dplyr::mutate(DOM = case_when(CCLCD %in% 1:3 ~ 1,
                                  TRUE ~ 0)) %>%
    ## Here PLT_CN is a unique plot visit ID, TRE_CN is a unique tree visit ID,
    ## DIA is dbh, SPCD is a species code, HT is total tree height,
    ## CR is compacted crown ratio, and TPA_UNADJ is TPA each tree represents
    ## UNADJ refers to non-response bias, which is handled later. Think of it 
    ## as just standard TPA
    dplyr::select(PLT_CN, TRE_CN=CN, CONDID, SPCD, DIA, HT, CR, DOM, BAA_STAND, TPA_UNADJ) %>%
    ## Join on plot attributes
    dplyr::left_join(dplyr::select(db$PLOT, CN, LAT, LON, ELEV, STATECD, UNITCD, COUNTYCD, PLOT), 
                     by = c('PLT_CN' = 'CN')) %>%
    dplyr::left_join(dplyr::select(db$COND, PLT_CN, CONDID, COND_STATUS_CD),
                     by = c('PLT_CN', 'CONDID')) %>%
    ## Join on coefficients
    dplyr::left_join(dplyr::select(coefCW, CC_B0:SPCD), by = 'SPCD') %>%
    dplyr::filter(COND_STATUS_CD == 1) %>%
    dplyr::mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
    ## We want to drop entire plots where any of these variables are NA
    ## i.e., don't want to compute plot-level canopy cover if individual trees
    ## are ommitted due to lack of allometrics. We will predict S-class of these
    ## plots based on a range of other variables later on.
    dplyr::mutate(cut = ifelse(is.na(SPCD) | SPCD %in% 998:999 |
                                 is.na(DIA) | is.na(HT) | is.na(CR) | is.na(BAA_STAND), 
                               1, 0)) %>%
    dplyr::group_by(PLT_CN, CONDID) %>%
    dplyr::mutate(cut = ifelse(sum(cut, na.rm = TRUE) > 0, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(cut < 1) %>%
    tidyr::drop_na() %>%
    ## Predict maximum crown width per Hann 1997 Eq 3
    mutate(mcw = CC_C0 + (CC_C1 * DIA) + (CC_C2 * DIA * DIA)) %>%
    ## Observed compacted crown length
    mutate(crown_length = (CR/100) * HT) %>%
    # Calculate largest crown width for tree (Hann - equation 2)
    mutate(exponent = CC_B0 + (CC_B1 * crown_length) + (CC_B2 * (DIA / HT)),
           cr = (CR/100) ** exponent,
           cw = mcw * cr) %>%
    ## Convert to crown area, assuming circular crowns
    dplyr::mutate(crownArea = pi * (cw/2)^2) %>%
    dplyr::select(PLT_CN, pltID, CONDID, TRE_CN, DOM, BAA_STAND, TPA_UNADJ, DIA, crownWidth = cw, crownArea)
  
  return(tree)
}


## Estimate tree canopy cover and density by diameter classes at each plot,
## adjusting canopy cover for overlap assuming random tree placement. Completely
## bogus, but again, that's what FVS does.
makeGNNvars <- function(tree) {
  
  gnnVars <- tree %>%
    dplyr::group_by(PLT_CN, pltID, CONDID, BAA_STAND) %>%
    dplyr::mutate(DIA_PERC = dplyr::percent_rank(DIA),
                  n = dplyr::n(),
                  DIA_PERC = dplyr::case_when(n > 1 ~ DIA_PERC,
                                              TRUE ~ 1.0)) %>%
    ungroup() %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(PLT_CN, pltID, CONDID, BAA_STAND) %>%
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
      QMD_20 = sqrt(sum(rFIA:::basalArea(DIA[DIA_PERC >=.8]) * TPA_UNADJ[DIA_PERC >=.8], na.rm = TRUE) / (0.005454 * sum(TPA_UNADJ[DIA_PERC >=.8], na.rm = TRUE))),
      QMD_DOM = sqrt(sum(rFIA:::basalArea(DIA[DOM == 1]) * TPA_UNADJ[DOM == 1], na.rm = TRUE) / (0.005454 * sum(TPA_UNADJ[DOM == 1], na.rm = TRUE))),
    ) %>%
    ## Correct for canopy overlap, bogus, but how FVS does it
    dplyr::mutate_at(.vars = dplyr::vars(CC_0_5:CC_ALL), function(x) {100 * (1- exp(-.01 * x))}) %>%
    ## If QMD is NA, then all DIA were the same and PERC_RANK flagged all as 0
    ## So, we say they are all in the top 20
    dplyr::mutate(QMD_20 = dplyr::case_when(
      is.na(QMD_20) ~ sqrt(BAA_STAND / (0.005454 * TPA_ALL)),
      TRUE ~ QMD_20)
    ) %>%
    as.data.frame() #%>% # Force data.table eval via dtplyr
    
  ## Previously used rowwise -- gives bogus results for max across columns
  ## Reverting to tried and tru
  # #g <- gnnVars %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(MAX_CC = max(CC_0_5:CC_20_30, na.rm = TRUE),
  #                 MAX_CC2 = max(CC_0_5:CC_15_20, na.rm = TRUE))
  gnnVars[, "MAX_CC"] <- apply(gnnVars[, c(15:19, 23)], 1, max)
  gnnVars[, "MAX_CC2"] <- apply(gnnVars[, 15:18], 1, max)
  

  return(gnnVars)
}



# Classify plots into one of 7 size classes, then one of 5 structure classes
classifyPlots <- function(gnnVars, dirRefCon, dirResults, cores) {

  ## Plot attributes for strata assignments and PVT
  pltAtt <- read.csv(paste0(dirResults, '/prep/fiaPlts.csv')) %>%
    select(pltID, PLOT_STATUS_CD) %>%
    distinct() %>%
    left_join(read.csv(paste0(dirResults, '/prep/fiaPlts_attributes.csv')),
              by = 'pltID')

  ## Get TPH thresholds for large size classes, thresholds vary by pvt
  th_size <- read.csv(paste0(dirRefCon, '/TH_Summary.csv'))

  ## Get S-class thresholds, varies by BPS
  sclassRules <- read.csv(paste0(dirRefCon, '/sclass_thresholds.txt'))


  ## Join thresholds and plot attributes onto our GNN variables table
  gnnVars <- gnnVars %>%
    dplyr::left_join(pltAtt, by = 'pltID') %>%
    dplyr::mutate(PVTCode = paste(MAP_ZONE, PVT, sep = '_')) %>%
    dplyr::left_join(dplyr::select(th_size, PVTCode, Size_TH, QMD_TH), by = 'PVTCode') %>%
    dplyr::mutate(Size_TH = dplyr::case_when(is.na(Size_TH) ~  integer(1),
                                             TRUE ~ Size_TH)) %>%
    dplyr::mutate(QMD_TH = dplyr::case_when(is.na(QMD_TH) ~  integer(1),
                                            TRUE ~ QMD_TH)) %>%
    ## Dropping forested locations when they fall in "non-forest" strata
    ## Update once we have real plot locations
    dplyr::filter(PLOT_STATUS_CD == 1 & !is.na(BPS_LLID))


  ## Run the size-class classifier
  pltSC <- gnnVars %>%
    dplyr::mutate(sizeClass = dplyr::case_when(
      CC_ALL < 10 ~ 1.0,
      Size_TH == 0 & QMD_DOM > QMD_TH & CC_GE_30 > 10 ~ 7.0,
      Size_TH == 0 & QMD_DOM > QMD_TH & CC_20_30 > 10 ~ 6.0,
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
  ruleList <- split(sclassRules, sclassRules$ID)


  ## Run in parallel - sorry for confusing if else here. Parallel is weird on Windows
  if (Sys.info()['sysname'] == 'Windows'){
    cl <- parallel::makeCluster(cores)
    parallel::clusterEvalQ(cl, {
      library(dplyr)
    })
    out <- parallel::parLapply(cl, X = names(ruleList), fun = assignSClass, ruleList,
                               pltSC)
    parallel::stopCluster(cl) # Kill the cluster

  } else { # Multicore systems
    out <- parallel::mclapply(X = names(ruleList), FUN = assignSClass, ruleList,
                              pltSC, mc.cores = cores)
  }
  ## Back to data.frame
  pltSC <- bind_rows(out)

  return(pltSC)
}

## Clunky as hell, but it works, so whatever
assignSClass <- function(x, ruleList, pltSC) {

  ## Literally just subset until we're down to one
  dat <- pltSC %>%
    dplyr::filter(as.character(ruleList[[x]]$BPS_ID) == as.character(BpS)) %>%
    dplyr::filter(ruleList[[x]]$SizeClass_max >= sizeClass) %>%
    dplyr::filter(ruleList[[x]]$SizeClass_min <= sizeClass) %>%
    dplyr::filter(ruleList[[x]]$Cover_max >= round(CC_ALL, 1)) %>%
    dplyr::filter(ruleList[[x]]$Cover_min <= round(CC_ALL, 1)) %>%
    dplyr::mutate(sclass = ruleList[[x]]$SClass)


  return(dat)
}







# 
# ## Predict crown area of live trees from FVS allometrics
# predictCrownWidth_fvs <- function(db, dirFVS, keepPlts, mapStems) {
#   
#   ## Read FVS coefficients
#   coefFVS <- read.csv(paste0(dirFVS, '/coef.csv')) %>%
#     dplyr::distinct(SPCD, variant, .keep_all = TRUE)
#   boundsFVS <- read.csv(paste0(dirFVS, '/bounds.csv')) %>%
#     dplyr::distinct(SPCD, variant, minD) %>%
#     dplyr::filter(minD > 1) # All else assumed 1 inch
#   bfFVS <- read.csv(paste0(dirFVS, '/bf.csv')) %>%
#     dplyr::distinct(SPCD, variant, location, BF) %>%
#     dplyr::filter(BF != 1)
#   mergeCoefs <- read.csv(paste0(dirFVS, '/mergeTheseSpecies.csv'))
#   
#   
#   ## Estimate crown width/area for every live tree that we can
#   tree <- db$COND %>%
#     filter(PLT_CN %in% keepPlts$PLT_CN) %>%
#     dplyr::filter(COND_STATUS_CD == 1) %>%
#     dplyr::select(PLT_CN, CONDID) %>%
#     dplyr::left_join(db$TREE, by = c('PLT_CN', 'CONDID')) %>%
#     ## Drop all dead trees
#     dplyr::filter(STATUSCD == 1) %>%
#     ## Basal area per acre (BAA) of each tree 
#     dplyr::mutate(BAA = rFIA:::basalArea(DIA) * TPA_UNADJ) %>% 
#     ## Stand-level BAA
#     dplyr::group_by(PLT_CN, CONDID) %>%
#     dplyr::mutate(BAA_STAND = sum(BAA, na.rm = TRUE)) %>% 
#     dplyr::ungroup() %>%
#     ## Here PLT_CN is a unique plot visit ID, TRE_CN is a unique tree visit ID,
#     ## DIA is dbh, SPCD is a species code, HT is total tree height,
#     ## CR is compacted crown ratio, and TPA_UNAJD is TPA each tree represents
#     ## UNADJ refers to non-response bias, which is handled later. Think of it 
#     ## as just standard TPA
#     dplyr::select(PLT_CN, TRE_CN=CN, CONDID, SPCD, DIA, HT, CR, BAA_STAND, TPA_UNADJ) %>%
#     ## Join on plot attributes
#     dplyr::left_join(dplyr::select(db$PLOT, CN, LAT, LON, ELEV, STATECD, UNITCD, COUNTYCD, PLOT), 
#                      by = c('PLT_CN' = 'CN')) %>%
#     dplyr::left_join(dplyr::select(db$PLOTGEOM, CN, FVS_LOC_CD, FVS_VARIANT), 
#                      by = c('PLT_CN' = 'CN')) %>%
#     dplyr::left_join(dplyr::select(db$COND, PLT_CN, CONDID, COND_STATUS_CD),
#                      by = c('PLT_CN', 'CONDID')) %>%
#     filter(COND_STATUS_CD == 1) %>%
#     mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
#     ## We want to drop entire plots where any of these variables are NA
#     ## i.e., don't want to compute plot-level canopy cover if individual trees
#     ## are ommitted due to lack of allometrics. We will predict S-class of these
#     ## plots based on a range of other variables later on.
#     dplyr::mutate(cut = ifelse(is.na(SPCD) | SPCD %in% 998:999 |
#                                  is.na(DIA) | is.na(HT) | is.na(CR) | is.na(BAA_STAND) |
#                                  is.na(LAT) | is.na(LON) | is.na(ELEV) |
#                                  is.na(FVS_LOC_CD) | is.na(FVS_VARIANT), 1, 0)) %>%
#     dplyr::group_by(PLT_CN, CONDID) %>%
#     dplyr::mutate(cut = ifelse(sum(cut, na.rm = TRUE) > 0, 1, 0)) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(cut < 1) %>%
#     tidyr::drop_na() %>%
#     ## Recode AK to Pacific coast
#     dplyr::mutate(FVS_VARIANT = dplyr::case_when(FVS_VARIANT == 'AK' ~ 'PN',
#                                                  FVS_VARIANT == 'CI' ~ 'IE',
#                                                  TRUE ~ FVS_VARIANT)) %>%
#     # Handle merged species -- most species share allometric equations
#     dplyr::left_join(dplyr::select(mergeCoefs, -c(Note)),
#                      by = c('SPCD', 'FVS_VARIANT')) %>%
#     dplyr::mutate(SPCD = dplyr::case_when(is.na(NEW_SPCD) ~ SPCD, 
#                                           TRUE ~ NEW_SPCD),
#                   FVS_VARIANT = dplyr::case_when(is.na(NEW_FVS_VARIANT) ~ as.character(FVS_VARIANT),
#                                                  TRUE ~ as.character(NEW_FVS_VARIANT))) %>%
#     # Add coefficients
#     dplyr::left_join(coefFVS, by = c('SPCD', 'FVS_VARIANT' = 'variant')) %>%
#     dplyr::left_join(boundsFVS, by = c('SPCD', 'FVS_VARIANT' = 'variant')) %>%
#     dplyr::left_join(bfFVS, by = c('SPCD', 'FVS_LOC_CD' = 'location', 'FVS_VARIANT' = 'variant')) %>%
#     ## If not listed, set equal to one
#     dplyr::mutate(BF = dplyr::case_when(is.na(BF) ~ 1.0,
#                                         TRUE ~ BF),
#                   minD = dplyr::case_when(is.na(minD) ~ 1.0,
#                                           TRUE ~ minD))  %>%
#     ## Prep our variables for allometric equations
#     dplyr::mutate(CL = HT * CR / 100, # Crown length
#                   EL100 = ELEV / 100, # Wierd ass parameterization, but that's what FIA does
#                   HI = ((ELEV - 5449) / 100) + 4*(LAT - 42.16) + 1.25*(-116.39 - LON), # Hopkins index - I think it's called hopkins
#                   BA = BAA_STAND,
#                   a1 = dplyr::case_when(is.na(a1) ~ 0, TRUE ~ a1),
#                   a2 = dplyr::case_when(is.na(a2) ~ 0, TRUE ~ a2),
#                   a3 = dplyr::case_when(is.na(a3) ~ 0, TRUE ~ a3),
#                   a4 = dplyr::case_when(is.na(a4) ~ 0, TRUE ~ a4),
#                   a5 = dplyr::case_when(is.na(a5) ~ 0, TRUE ~ a5),
#                   a6 = dplyr::case_when(is.na(a6) ~ 0, TRUE ~ a6)) %>%
#     ## Predict crown width
#     dplyr::mutate(cw = dplyr::case_when(
#       eq == 1 & DIA >= minD ~ a1 + (a2*DIA) + (a3*(DIA^2)),
#       eq == 1 & DIA < minD ~ (a1 + (a2*minD) * (a3 * (minD^2))) * (DIA / minD),
#       eq == 2 & DIA >= minD ~ a1 + (a2 * DIA) + (a3 * (DIA^2)) + (a4 * CR) + (a5 * BA) + (a6 *HI),
#       eq == 2 & DIA < minD ~ (a1 + (a2 * minD) + (a3 * (minD^2)) + (a4 * CR) + (a5 * BA) + (a6 *HI)) * (DIA / minD),
#       eq == 3 & HT >= 15 & SPCD == 264 ~ a1 * (DIA^a2) * (HT^a3) * (CL^a4),
#       eq == 3 & HT < 15 & HT >=5 & SPCD == 264 ~ 0.8 * HT * ifelse(CR * .01 > .5, .5, CR*.01),
#       eq == 3 & HT < 5 & SPCD == 264 ~ (0.8 * HT * ifelse(CR * .01 > .5, .5, CR*.01)) * (1 - ((HT - 5) * .1)) * a1 * (DIA^a2) * (HT^a3) * (CL^a4) * (HT - 5) * .1,
#       eq == 3 & DIA >= minD ~ a1 * exp(a2 + (a3 * log(CL)) + (a4 * log(DIA)) + (a5 * log(HT)) + (a6 * log(BA))),
#       eq == 3 & DIA < minD ~ (a1 * exp(a2 + (a3 * log(CL)) + (a4 * log(minD)) + (a5 * log(HT)) + (a6 * log(BA)))) * (DIA / minD),
#       eq %in% c(4,6) & DIA >= minD ~ a1 * (DIA^a2),
#       eq %in% c(4,6) & DIA < minD ~ a1 * (minD^a2) * (DIA / minD),
#       eq == 5 & DIA >= minD ~ (a1 * BF) * (DIA ^ a2) * (HT^a3) * (CL^a4) * ((BA + 1)^a5) * (exp(EL100)^a6),
#       eq == 5 & DIA < minD ~ (a1 * BF) * (minD ^ a2) * (HT^a3) * (CL^a4) * ((BA + 1)^a5) * (exp(EL100)^a6) * (DIA / minD))) %>%
#     ## Convert to crown area, assuming circular crowns
#     dplyr::mutate(crownArea = pi * (cw/2)^2) %>%
#     dplyr::select(PLT_CN, pltID, CONDID, TRE_CN, BAA_STAND, TPA_UNADJ, DIA, crownWidth = cw, crownArea)
#   
#   return(tree)
# }
# 
