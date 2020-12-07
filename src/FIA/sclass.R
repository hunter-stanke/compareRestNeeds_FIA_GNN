##=====================================================
##=====================================================
##
## Determine "forest size class" following the Haugo et
## al classification scheme. We first predict crown
## width from allometric equations implemented in FVS.
## We then determine percent crown cover and live tree
## density by DBH classes on each FIA plot. Once we
## have the same variables as GNN, we can classify
## FIA plots into one of 7 size classes.
##
##
## Created:       27 October 2020 - Hunter Stanke
## Last modified: 8 November 2020 - Hunter Stanke
##
##====================================================
##====================================================


library(rFIA)
library(dplyr)
library(tidyr)

## Set working directory
setwd('/home/hunter/Dropbox/departureR6/')



#================================================================================
##  Produce same variables reported by GNN for each FIA plot --------------------
##===============================================================================

## Read FIA tables - really big, sorry
pnw <- readFIA('/home/hunter/FIA',
               states = c('OR', 'WA'),
               common = FALSE)

## List of potential plots to include (i.e., drop pre annual plots)
plts <- read.csv('./results/prepData/fiaPlts.csv') %>%
  left_join(read.csv('./results/prepData/fiaPlts_attributes.csv'), by = 'pltID')

# Join attributes
pnw$PLOT <- pnw$PLOT %>%
  left_join(select(plts, PLT_CN, pltID, BPS_LLID), by = c('CN' = 'PLT_CN'))

## Read FVS coefficients
coefFVS <- read.csv('./fvs/crownWidth/coef.csv') %>%
  distinct(SPCD, variant, .keep_all = TRUE)
boundsFVS <- read.csv('./fvs/crownWidth/bounds.csv') %>%
  distinct(SPCD, variant, minD) %>%
  filter(minD > 1) # All else assumed 1 inch
bfFVS <- read.csv('./fvs/crownWidth/bf.csv') %>%
  distinct(SPCD, variant, location, BF) %>%
  filter(BF != 1)

## Read our merge table - most models are re-used for a handful of species
mergeCoefs <- read.csv('./fvs/crownWidth/mergeCoefs.csv')


## Estimate crown width from dbh and other things, following FVS equations
tree <- pnw$TREE %>%
  filter(PLT_CN %in% plts$PLT_CN) %>%
  mutate(BAA = rFIA:::basalArea(DIA) * TPA_UNADJ) %>%
  filter(STATUSCD == 1) %>%
  group_by(PLT_CN) %>%
  mutate(BAA_STAND = sum(BAA, na.rm = TRUE)) %>%
  ungroup() %>%
  select(PLT_CN, TRE_CN = CN, SPCD, DIA, HT, CR, BAA_STAND, TPA_UNADJ) %>%
  left_join(select(pnw$PLOT, CN, LAT, LON, ELEV), by = c('PLT_CN' = 'CN')) %>%
  left_join(select(pnw$PLOTGEOM, CN, FVS_LOC_CD, FVS_VARIANT), by = c('PLT_CN' = 'CN')) %>%
  ## We want to drop PLOTS where any of these variables are NA
  mutate(cut = if_else(is.na(SPCD) | SPCD %in% 998:999 |
                       is.na(DIA) | is.na(HT) | is.na(CR) | is.na(BAA_STAND) |
                       is.na(LAT) | is.na(LON) | is.na(ELEV) |
                       is.na(FVS_LOC_CD) | is.na(FVS_VARIANT), 1, 0)) %>%
  group_by(PLT_CN) %>%
  mutate(cut = if_else(sum(cut, na.rm = TRUE) > 0, 1, 0)) %>%
  ungroup() %>%
  filter(cut < 1) %>%
  tidyr::drop_na() %>%
  ## Recode AK to Pacific coast
  mutate(FVS_VARIANT = case_when(FVS_VARIANT == 'AK' ~ 'PN',
                                 FVS_VARIANT == 'CI' ~ 'IE',
                                 TRUE ~ FVS_VARIANT)) %>%
  # Handle merges
  left_join(select(mergeCoefs, -c(Note)), by = c('SPCD', 'FVS_VARIANT')) %>%
  mutate(SPCD = case_when(is.na(NEW_SPCD) ~ SPCD,
                          TRUE ~ NEW_SPCD),
         FVS_VARIANT = case_when(is.na(NEW_FVS_VARIANT) ~ as.character(FVS_VARIANT),
                                 TRUE ~ as.character(NEW_FVS_VARIANT))) %>%
  # Add coefficients
  left_join(coefFVS, by = c('SPCD', 'FVS_VARIANT' = 'variant')) %>%
  left_join(boundsFVS, by = c('SPCD', 'FVS_VARIANT' = 'variant')) %>%
  left_join(bfFVS, by = c('SPCD', 'FVS_LOC_CD' = 'location', 'FVS_VARIANT' = 'variant')) %>%
  ## If not listed, values are equal to one
  mutate(BF = case_when(is.na(BF) ~ 1.0,
                        TRUE ~ BF),
         minD = case_when(is.na(minD) ~ 1.0,
                          TRUE ~ minD))  %>%
  ## Prep our variables for allometric equations
  mutate(CL = HT * CR / 100,
         EL100 = ELEV / 100,
         HI = ((ELEV - 5449) / 100) + 4*(LAT - 42.16) + 1.25*(-116.39 - LON),
         BA = BAA_STAND,
         a1 = case_when(is.na(a1) ~ 0, TRUE ~ a1),
         a2 = case_when(is.na(a2) ~ 0, TRUE ~ a2),
         a3 = case_when(is.na(a3) ~ 0, TRUE ~ a3),
         a4 = case_when(is.na(a4) ~ 0, TRUE ~ a4),
         a5 = case_when(is.na(a5) ~ 0, TRUE ~ a5),
         a6 = case_when(is.na(a6) ~ 0, TRUE ~ a6)) %>%
  ## Predict crown width
  mutate(cw =   case_when(eq == 1 & DIA >= minD ~ a1 + (a2*DIA) + (a3*(DIA^2)),
                          eq == 1 & DIA < minD ~ (a1 + (a2*minD) * (a3 * (minD^2))) * (DIA / minD),
                          eq == 2 & DIA >= minD ~ a1 + (a2 * DIA) + (a3 * (DIA^2)) + (a4 * CR) + (a5 * BA) + (a6 *HI),
                          eq == 2 & DIA < minD ~ (a1 + (a2 * minD) + (a3 * (minD^2)) + (a4 * CR) + (a5 * BA) + (a6 *HI)) * (DIA / minD),
                          eq == 3 & HT >= 15 & SPCD == 264 ~ a1 * (DIA^a2) * (HT^a3) * (CL^a4),
                          eq == 3 & HT < 15 & HT >=5 & SPCD == 264 ~ 0.8 * HT * if_else(CR * .01 > .5, .5, CR*.01),
                          eq == 3 & HT < 5 & SPCD == 264 ~ (0.8 * HT * if_else(CR * .01 > .5, .5, CR*.01)) * (1 - ((HT - 5) * .1)) * a1 * (DIA^a2) * (HT^a3) * (CL^a4) * (HT - 5) * .1,
                          eq == 3 & DIA >= minD ~ a1 * exp(a2 + (a3 * log(CL)) + (a4 * log(DIA)) + (a5 * log(HT)) + (a6 * log(BA))),
                          eq == 3 & DIA < minD ~ (a1 * exp(a2 + (a3 * log(CL)) + (a4 * log(minD)) + (a5 * log(HT)) + (a6 * log(BA)))) * (DIA / minD),
                          eq %in% c(4,6) & DIA >= minD ~ a1 * (DIA^a2),
                          eq %in% c(4,6) & DIA < minD ~ a1 * (minD^a2) * (DIA / minD),
                          eq == 5 & DIA >= minD ~ (a1 * BF) * (DIA ^ a2) * (HT^a3) * (CL^a4) * ((BA + 1)^a5) * (exp(EL100)^a6),
                          eq == 5 & DIA < minD ~ (a1 * BF) * (minD ^ a2) * (HT^a3) * (CL^a4) * ((BA + 1)^a5) * (exp(EL100)^a6) * (DIA / minD))) %>%
  mutate(crownArea = pi * (cw/2)^2) %>%
  select(PLT_CN, TRE_CN, BAA_STAND, TPA_UNADJ, DIA, crownWidth = cw, crownArea)



## Now we want to make a series of variables to match GNNs predicted  variables
## i.e., TPA and canopy cover by size classes
gnnVars <- tree %>%
  group_by(PLT_CN, BAA_STAND) %>%
  mutate(DIA_PERC = percent_rank(DIA),
         n = n(),
         DIA_PERC = case_when(n > 1 ~ DIA_PERC,
                              TRUE ~ 1.0)) %>%
  ## All trees listed are live, see above
  summarize(TPA_0_5 = sum(TPA_UNADJ[DIA < 5], na.rm = TRUE),
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
            QMD_20 = 4/pi * sum(rFIA:::basalArea(DIA[DIA_PERC >=.8]) * TPA_UNADJ[DIA_PERC >=.8], na.rm = TRUE) / sum(TPA_UNADJ[DIA_PERC >=.8], na.rm = TRUE)) %>%
  ## Correct for canopy overlap, bogus, but how FVS does it
  mutate_at(.vars = dplyr::vars(CC_0_5:CC_ALL), function(x) {100 * (1- exp(-.01 * x))}) %>%
  ## If QMD is NA, then all DIA were the same and PERC_RANK flagged all as 0
  ## So, we say they are all in the top 20
  mutate(QMD_20 = case_when(is.na(QMD_20) ~ sqrt(4/ pi * BAA_STAND / TPA_ALL) * 12,
                            TRUE ~ sqrt(QMD_20) * 12)) %>%
  rowwise() %>%
  mutate(MAX_CC = max(CC_0_5:CC_20_30, na.rm = TRUE),
         MAX_CC2 = max(CC_0_5:CC_15_20, na.rm = TRUE))





#================================================================================
##  Classify plots into size classes and structure classes ----------------------
##===============================================================================

## Get TPH thresholds for large size classes, thresholds vary by pvt
th_size <- read.csv('./refCon/TH_Summary.csv')

## Get S-class thresholds, varies by BPS
sclassRules <- read.csv('./refCon/sclass_thresholds.txt')


## Join thresholds and plot attributes onto our GNN variables table
gnnVars <- gnnVars %>%
  left_join(plts, by = 'PLT_CN') %>%
  mutate(PVTCode = paste(MAP_ZONE, PVT, sep = '_')) %>%
  left_join(select(th_size, PVTCode, Size_TH)) %>%
  mutate(Size_TH = case_when(is.na(Size_TH) ~  integer(1), TRUE ~ Size_TH)) %>%
  ## Dropping forested locations when they fall in "non-forest" strata
  ## Update once we have real plot locations
  filter(PLOT_STATUS_CD == 1 & !is.na(BPS_LLID))


## Run the size-class classifier
pltSC <- gnnVars %>%
  mutate(sizeClass = case_when(CC_ALL < 10 ~ 1.0,
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
                               TRUE ~ NA_real_))

## Now the structure class classifier
## Really clunky but it works fine
sclass <- c()
for (i in 1:nrow(pltSC)){

  dat <- sclassRules %>%
    filter(as.character(BPS_ID) == as.character(pltSC$BpS[i])) %>%
    filter(SizeClass_max >= pltSC$sizeClass[i]) %>%
    filter(SizeClass_min <= pltSC$sizeClass[i]) %>%
    filter(Cover_max >= round(pltSC$CC_ALL[i], 1)) %>%
    filter(Cover_min <= round(pltSC$CC_ALL[i], 1))

  if(nrow(dat) > 1) {
    stop('More than 1 size class possible')
  } else if (nrow(dat) < 1) {
    stop("Can't classify, doesn't meet criteria")
  } else {
    sclass <- c(sclass, dat$SClass)
  }
}
## Add it back on
pltSC$sclass <- sclass




#================================================================================
##  Predict S-class at forested plots where we can't estimate canopy cover ------
##===============================================================================


## Setting up some predictors ---------------------------------------------------
## TPA and BAA for all FIA plots
pltTPA <- tpa(pnw, grpBy = c(BPS_LLID), byPlot = TRUE)

## TPA and BAA of top 20% of trees by diameter class
pnw$TREE <- pnw$TREE %>%
  group_by(PLT_CN) %>%
  mutate(DIA_PERC = percent_rank(DIA),
         n = n(),
         DIA_PERC = case_when(n > 1 ~ DIA_PERC,
                              TRUE ~ 1.0)) %>%
  ungroup()
pltTPA20 <- tpa(pnw, byPlot = TRUE,
                treeDomain = DIA_PERC >= .8)


## Diversity of biomass across size strata for all FIA plots
pnw$TREE$sc5 <- makeClasses(pnw$TREE$DIA, interval = 5)
pltDiv <- diversity(pnw, byPlot = TRUE, stateVar = DRYBIO_AG*TPA_UNADJ, grpVar = sc5)

## Drop all NA BPS_LLID, estimate QMD, join sclass when available
pltTPA <- pltTPA %>%
  filter(PLOT_STATUS_CD == 1) %>%
  select(PLT_CN, BPS_LLID, TPA, BAA) %>%
  ## Average Tree size,  QMD
  mutate(QMD = sqrt(BAA / TPA * 4 / pi) * 12,
         QMD = case_when(is.na(QMD) ~ 0, TRUE ~ QMD),
         BPS = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,1]) %>%
  ## Join sclass
  left_join(select(pltSC, PLT_CN, sclass), by = 'PLT_CN') %>%
  left_join(select(pltDiv, PLT_CN, H, S, Eh), by = 'PLT_CN') %>%
  left_join(select(pltTPA20, PLT_CN, TPA, BAA), by = 'PLT_CN', suffix = c('', '20')) %>%
  mutate(sclass = as.factor(sclass),
         #id = row.names(.),
         QMD20 = sqrt(BAA20 / TPA20 * 4 / pi) * 12,
         QMD20 = case_when(is.na(QMD20) ~ 0, TRUE ~ QMD20),
         Eh = replace_na(Eh, 0)) %>%
    select(-c(BAA, BAA20)) %>%
  distinct()





## Impute missing sclass by BPS -----------------------------------

library(yaImpute)

## Which BPSs have missing values?
miss <- pltTPA  %>%
  group_by(BPS) %>%
  summarize(n = n(),
            nna = length(which(is.na(sclass)))) %>%
  filter(nna > 0)

predList <- list()
## Building a seperate "model" for each forest type group
for (i in 1:nrow(miss)){

  ## Break into reference and target set and predict
  ## sclass with knn, distance defined by trees in random forest algorithm
  x <- pltTPA %>%
    filter(BPS == miss$BPS[i]) %>%
    ## yaImpute references observations based on row names.
    ## tibble hates that idea, so we have to watch really closely when
    ## we use yaImpute and dplyr/tibble together
    tibble::column_to_rownames('PLT_CN') %>%
    select(TPA, QMD, TPA20, QMD20, H, Eh)
  y <- pltTPA %>%
    filter(BPS == miss$BPS[i]) %>%
    filter(!is.na(sclass)) %>%
    ## Resetting levels
    mutate(sclass = factor(sclass)) %>%
    tibble::column_to_rownames('PLT_CN') %>%
    select(sclass)

  ## Need at least two groups to do classification
  if (length(unique(y$sclass)) > 2){
    ## Find nearest neighbors
    rf <- yai(x = x, y = y,
              method = 'randomForest',
              k =  if_else(miss$n[i] < 15, miss$n[i]-1, 15))

    newdata = pltTPA %>%
      filter(BPS == miss$BPS[i] & is.na(sclass)) %>%
      tibble::column_to_rownames('PLT_CN')

    ## Predict s-class at forested plots where we couldn't calculate it
    preds <- predict.yai(rf, newdata,
                         method.factor = 'median')
    preds <- preds %>%
      mutate(PLT_CN = as.numeric(row.names(.))) %>%
      tibble::as_tibble() %>%
      ## IF TPA is 0, automatically sclass 1 (recently disturbed is what I assume)
      mutate(sclass = as.numeric(as.character(sclass)),
             sclass = case_when(TPA == 0 ~ 1, TRUE ~ sclass)) %>%
      select(PLT_CN, sclass.pred = sclass)
  } else {
    ## if only one class observed in forest type, predict that class everywhere
    preds <- pltTPA %>%
      filter(FORTYPCD == miss$FORTYPCD[i] & is.na(sclass))
    preds$sclass.pred <- unique(y$sclass)
    preds <- preds[,c('PLT_CN', 'sclass.pred')]
  }


  ## Handle factor/ double/ levels issues
  preds$sclass.pred <- as.numeric(as.character(preds$sclass.pred))

  predList[[i]] <- preds

  print(i)

}

## Combine and handle factors
preds <- bind_rows(predList) %>%
  select(PLT_CN, sclass.pred) %>%
  mutate(sclass.pred = factor(sclass.pred))





#================================================================================
##  Merge observed/predicted s-classes and save ---------------------------------
##===============================================================================

## Join it all up on PLOT
sclassPlt <- pnw$PLOT %>%
  mutate(PLT_CN = CN) %>%
  filter(PLT_CN %in% unique(plts$PLT_CN)) %>%
  select(PLT_CN, BPS_LLID) %>%
  # Join "observed" s-class
  left_join(distinct(select(pltSC, PLT_CN, sclass)), by = 'PLT_CN') %>%
  left_join(distinct(preds), by = 'PLT_CN') %>%
  # Update s-class with predictions when we couldn't "observe" it
  mutate(sclass = case_when(is.na(sclass) ~ sclass.pred, TRUE ~ sclass)) %>%
  select(PLT_CN, sclass)


## Save the results
write.csv(sclassPlt, './results/prepData/plt_sclass.csv', row.names = FALSE)


