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
         AREA_TOTAL = round(AREA_TOTAL / 1000, 0) * 1000,
         halfint = round(halfint / 1000, 0) * 1000)

## Total forestland
fia.total <- read.csv(here::here('results/FIA/totalForest/ORWA_ti.csv')) %>%
  filter(YEAR == 2017) %>%
  select(YEAR, AREA_TOTAL, AREA_TOTAL_VAR, N) %>%
  mutate(halfint = qt(0.975, N-1) * sqrt(AREA_TOTAL_VAR)) %>%
  ## acres to hectares
  mutate(AREA_TOTAL = AREA_TOTAL * 0.404686,
         halfint = halfint * 0.404686,
         AREA_TOTAL = round(AREA_TOTAL / 1000, 0) * 1000,
         halfint = round(halfint / 1000, 0) * 1000)


## Total forestland by EW
gnn.ew.total <- read.csv(here::here('results/GNN/sclass/ORWA_EW.csv')) %>%
  filter(YEAR == 2017) %>%
  group_by(YEAR, ew) %>%
  summarize(AREA_TOTAL = sum(AREA_TOTAL)) %>%
  ## acres to hectares
  mutate(AREA_TOTAL = AREA_TOTAL * 0.404686,
         AREA_TOTAL = round(AREA_TOTAL / 1000, 0) * 1000) 

## Total forestland
gnn.total <- gnn.ew.total %>%
  group_by(YEAR) %>%
  summarize(AREA_TOTAL = sum(AREA_TOTAL))


## Restoration needs by EW
fia.ew <- read.csv(here::here('results/FIA/sclass/ORWA_EW_ti.csv')) %>%
  filter(YEAR == 2017) %>%
  select(YEAR, sclass, ew, AREA_TOTAL, AREA_TOTAL_VAR, PERC_AREA_VAR, N) %>%
  group_by(YEAR, ew) %>%
  ## acres to hectares
  mutate(pct = AREA_TOTAL / sum(AREA_TOTAL) * 100,
         AREA_TOTAL_VAR = AREA_TOTAL_VAR * ((0.404686)^2)) %>%
  mutate(AREA_TOTAL = AREA_TOTAL * 0.404686,
         a.halfint = qt(0.975, N-1) * sqrt(AREA_TOTAL_VAR),
         a.halfint = round(a.halfint / 1000, 0) * 1000,
         AREA_TOTAL = round(AREA_TOTAL / 1000, 0) * 1000,
         p.halfint = qt(0.975, N-1) * sqrt(PERC_AREA_VAR),
         pct = round(pct, 1),
         p.halfint = round(p.halfint, 1))


## Restoration needs by EW
gnn.ew <- read.csv(here::here('results/GNN/sclass/ORWA_EW.csv')) %>%
  filter(YEAR == 2017) %>%
  select(YEAR, ew, sclass,  AREA_TOTAL) %>%
  ## acres to hectares
  mutate(AREA_TOTAL = AREA_TOTAL * 0.404686) %>%
  group_by(YEAR, ew) %>%
  mutate(AREA_TOTAL = round(AREA_TOTAL / 1000, 0) * 1000,
         pct = AREA_TOTAL / sum(AREA_TOTAL) * 100,
         pct = round(pct, 1))
