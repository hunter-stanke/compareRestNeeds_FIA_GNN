##=====================================================
##=====================================================
##
## Estimate restoration need from s-class distributions
## and reference conditions defined by biophysical 
## settings. Have to modify slightly between FIA and 
## GNN based results, I'll update that soon.
##
## Created:       30 October 2020 - Hunter Stanke
## Last modified: 4 November 2020 - Hunter Stanke
##
##====================================================
##====================================================


##==============================================================================
##  Set up your working directory and number of cores to use -------------------
##==============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(parallel)

# Set working directory
setwd('/home/hunter/Dropbox/departureR6/')

## Number of cores to use
nCores = 10



##==============================================================================
##  Prep data and append reference conditions ----------------------------------
##==============================================================================

## S-class distributions
bps <- read.csv('./results/areaFIA/sclass_BPSLLID.csv')

## Reference conditions - NRV +/- 2 SD
refCon <- read.csv('./refCon/R6RefCon.txt') %>%
  select(BpS_Code = LF_BpS_Code, SCLASS = Sclass, avg = Avg_, low = Minus_2_SD, high = Plus_2_SD) %>%
  distinct()

## "Order of operations" for transferring land area between S-classes
rules <- read.csv('./refCon/restorationRules.csv') %>%
  select(BpS_Code = LF_BpS_Code, Order:PassiveOnly)


## Append reference conditions 
bps <- bps %>%
  # Need this for FIA summaries
  mutate(BPS = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,1],
         PVT = stringr::str_split(BPS_LLID, '_', simplify = TRUE)[,2],
         BpS_Code = paste(BPS, PVT, sep = '_')) %>%
  filter(AREA_TOTAL > 0) %>%
  ## Make missing values explicit
  # tidyr::complete(nesting(BpS_Code, BPS_LLID), SCLASS, fill = list(AREA=0, AREA_PCT=0)) %>%
  # group_by(BpS_Code, BPS_LLID) %>%
  # mutate(AREA_TOTAL = unique(AREA_TOTAL)[!is.na(unique(AREA_TOTAL))]) %>%
  # ungroup() %>%
  ## Above 4 are GNN only, don't use for FIA
  ## Join reference conditions
  left_join(refCon, by = c('BpS_Code', 'SCLASS')) %>%
  mutate(across(c(avg:high), .fns = function(x, total) {x * .01 * total}, total = .$AREA_TOTAL)) #%>%
  # # mutate(BpS_Code = as.character(BpS_Code),
  # #        BPS_LLID = as.character(BPS_LLID)) 
  # mutate(BpS_Code = as.character(BpS_Code))




##==============================================================================
##  Determine restoration need -------------------------------------------------
##==============================================================================

## Some functions to implement the "restoration" algorithm
source('./scripts/restNeedFunctions.R')

## An iterator that identifies each "strata"
x <- unique(bps$BPS_LLID)

## Run the algorithm in parallel, overkill for large units
out <- mclapply(X = x, FUN = restArea, strataID = BPS_LLID, stratArea = bps, restRules = rules, mc.cores = nCores)
out <- unlist(out, recursive = FALSE)

## Bring our results back into data.frames
transfer <- bind_rows(out[names(out) == 'transfer'])
strata_new <- bind_rows(out[names(out) == 'strata'])

## Save it all
write.csv(transfer, './results/areaFIA/areaTransfers_BPSLLID.csv', row.names = FALSE)
write.csv(strata_new, './results/areaFIA/sclass_BPSLLID_postRestoration.csv', row.names = FALSE)


## Make a cleaner dataset that shows prop need by type
rn <- transfer %>%
  mutate(type = case_when(ActiveOnly == 1 ~ 'active',
                          PassiveOnly == 1 ~ 'passive',
                          TRUE ~ 'both')) %>%
  select(BpS_Code, BPS_LLID, type, REST_ACRES) %>%
  group_by(BpS_Code, BPS_LLID, type) %>%
  summarize(REST_ACRES = sum(REST_ACRES, na.rm = TRUE)) %>%
  left_join(distinct(select(bps, BpS_Code, BPS_LLID, AREA_TOTAL)), by = c('BpS_Code', 'BPS_LLID')) %>%
  mutate(REST_PCT = REST_ACRES / AREA_TOTAL)
write.csv(rn, './results/areaFIA/restNeed_BPSLLID.csv')


