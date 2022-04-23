library(dplyr)
library(here)

## Total forestland by EW
fia.ew.total <- read.csv(here::here('results/FIA/totalForest/ORWA_EW_ti.csv')) %>%
  filter(YEAR == 2017) %>%
  select(YEAR, ew, AREA_TOTAL, AREA_TOTAL_VAR, N) %>%
  mutate(halfint = qt(0.975, N-1) * sqrt(AREA_TOTAL_VAR)) %>%
  ## acres to hectares
  mutate(AREA_TOTAL = AREA_TOTAL * 0.404686,
         halfint = halfint * 0.404686,
         low = round((AREA_TOTAL - halfint) / 1000, 0) * 1000,
         high = round((AREA_TOTAL + halfint) / 1000, 0) * 1000,
         AREA_TOTAL = round(AREA_TOTAL / 1000, 0) * 1000)

## Total forestland
fia.total <- read.csv(here::here('results/FIA/totalForest/ORWA_ti.csv')) %>%
  filter(YEAR == 2017) %>%
  select(YEAR, AREA_TOTAL, AREA_TOTAL_VAR, N) %>%
  mutate(halfint = qt(0.975, N-1) * sqrt(AREA_TOTAL_VAR)) %>%
  ## acres to hectares
  mutate(AREA_TOTAL = AREA_TOTAL * 0.404686,
         halfint = halfint * 0.404686,
         low = round((AREA_TOTAL - halfint) / 1000, 0) * 1000,
         high = round((AREA_TOTAL + halfint) / 1000, 0) * 1000,
         AREA_TOTAL = round(AREA_TOTAL / 1000, 0) * 1000)


## Total forestland by EW
gnn.ew.total <- read.csv(here::here('results/GNN/sclass/ORWA_EW.csv')) %>%
  filter(YEAR == 2017) %>%
  group_by(YEAR, ew) %>%
  summarize(AREA_TOTAL = sum(AREA_TOTAL)) %>%
  ## acres to hectares
  mutate(AREA_TOTAL = AREA_TOTAL * 0.404686)

## Total forestland
gnn.total <- gnn.ew.total %>%
  group_by(YEAR) %>%
  summarize(AREA_TOTAL = sum(AREA_TOTAL))


## Restoration needs by EW
fia.ew.rn <- read.csv(here::here('results/FIA/restNeed/ORWA_EW_BPS_annual_restNeed.csv')) %>%
  filter(YEAR == 2017) %>%
  select(YEAR, type, ew, BpS_Code, REST_ACRES) %>%
  group_by(YEAR, ew, type) %>%
  summarize(AREA_TOTAL = sum(REST_ACRES)) %>%
  ungroup() %>%
  ## acres to hectares
  mutate(AREA_TOTAL = AREA_TOTAL * 0.404686) %>%
  left_join(select(fia.ew.total, total = AREA_TOTAL, YEAR, ew, halfint),  by = c('YEAR', 'ew')) %>%
  mutate(pct = AREA_TOTAL / total * 100,
         low = round(AREA_TOTAL - halfint, 1),
         high = round(AREA_TOTAL + halfint, 1),
         AREA_TOTAL = round(AREA_TOTAL / 1000, 0) * 1000,
         pct = round(pct, 1),)

## Restoration needs full region
fia.rn <- fia.ew.rn %>%
  group_by(YEAR) %>%
  summarize(AREA_TOTAL = sum(AREA_TOTAL)) %>%
  left_join(select(fia.total, total = AREA_TOTAL, YEAR),  by = c('YEAR')) %>%
  mutate(pct = AREA_TOTAL / total * 100)


## Restoration needs by EW
gnn.ew.rn <- read.csv(here::here('results/GNN/restNeed/ORWA_EW_BPS_restNeed.csv')) %>%
  filter(YEAR == 2017) %>%
  select(YEAR, ew, type, BpS_Code, REST_ACRES) %>%
  group_by(YEAR, ew, type) %>%
  summarize(AREA_TOTAL = sum(REST_ACRES)) %>%
  ungroup() %>%
  ## acres to hectares
  mutate(AREA_TOTAL = AREA_TOTAL * 0.404686) %>%
  left_join(select(gnn.ew.total, total = AREA_TOTAL, YEAR, ew),  by = c('YEAR', 'ew')) %>%
  mutate(pct = AREA_TOTAL / total * 100)

## Restoration needs full region
gnn.rn <- gnn.ew.rn %>%
  group_by(YEAR) %>%
  summarize(AREA_TOTAL = sum(AREA_TOTAL)) %>%
  left_join(select(gnn.total, total = AREA_TOTAL, YEAR),  by = c('YEAR')) %>%
  mutate(pct = AREA_TOTAL / total * 100)

