library(sf)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(pals)
library(here)


## Read our BPS, strata, and map zone shapefiles
mz <- st_read(here('data/GIS/mapzones_shp/mapzones_shp.shp'))
strata <- st_read(here('data/GIS/strataDeMeo/strataDeMeo.shp'))
ws <- st_read(here('data/GIS/mapzones_westside/mapzones_westside.shp')) # A mask for westside


## Read estimates
fiaStrata <- read.csv(here('results/FIA/restNeed/BPS_LLID_ti_restNeed.csv')) %>%
  select(-c(any_of(c('X', 'BpS_Code')))) %>%
  filter(YEAR == 2011) %>%
  tidyr::complete(BPS_LLID, type, fill = list(REST_PCT=0, REST_ACRES=0)) %>%
  mutate(source = 'FIA',
         REST_PCT = REST_PCT * 100,
         REST_PCT = tidyr::replace_na(REST_PCT, 0)) %>%
  group_by(BPS_LLID) %>%
  mutate(AREA_TOTAL = unique(AREA_STRATA[!is.na(AREA_STRATA)])) %>%
  ungroup()
gnnStrata <- read.csv(here('results/GNN/restNeed/restNeed_BPSLLID.csv')) %>%
  select(-c(any_of(c('X', 'BpS_Code')))) %>%
  tidyr::complete(BPS_LLID, type, fill = list(REST_PCT=0, REST_ACRES=0)) %>%
  mutate(source = 'GNN',
         REST_PCT = REST_PCT * 100,
         REST_PCT = tidyr::replace_na(REST_PCT, 0)) %>%
  group_by(BPS_LLID) %>%
  mutate(AREA_TOTAL = unique(AREA_TOTAL[!is.na(AREA_TOTAL)])) %>%
  ungroup()


## Make it spatial
strata <- strata %>%
  left_join(select(fiaStrata, BPS_LLID, type, AREA_FIA = REST_ACRES, AREA_TOTAL_FIA = AREA_TOTAL), by = 'BPS_LLID') %>%
  full_join(select(gnnStrata, BPS_LLID, type, AREA_GNN = REST_ACRES, AREA_TOTAL_GNN = AREA_TOTAL), by = c('BPS_LLID', 'type')) %>%
  mutate(type_name = case_when(type == 'active' ~ 'Disturbance',
                               type == 'passive' ~ 'Succession',
                               type == 'both' ~ 'Disturbance then succession')) %>%
  mutate(diff = ((AREA_GNN / AREA_TOTAL_GNN) - (AREA_FIA / AREA_TOTAL_FIA)) * 100) %>%
  ## Putting a max on things so we can pick up differences
  mutate(diff = case_when(is.na(diff) ~ NA_real_,
                          diff > 50 ~ 50,
                          diff < -50 ~ -50,
                          TRUE ~ diff))


type = c('active', 'passive', 'both', 'total')
for (i in type) {
  print(i)
  
  if(i == 'total'){
    
    strataPlt <- strata %>% 
      group_by(BPS_LLID) %>%
      summarize(AREA_GNN = sum(AREA_GNN, na.rm = TRUE),
                AREA_TOTAL_GNN = first(AREA_TOTAL_GNN),
                AREA_FIA = sum(AREA_FIA, na.rm = TRUE),
                AREA_TOTAL_FIA = first(AREA_TOTAL_FIA)) %>%
      mutate(diff = ((AREA_GNN / AREA_TOTAL_GNN) - (AREA_FIA / AREA_TOTAL_FIA)) * 100) %>%
      ## Putting a max on things so we can pick up differences
      mutate(diff = case_when(is.na(diff) ~ NA_real_,
                              diff > 30 ~ 30,
                              diff < -30 ~ -30,
                              TRUE ~ diff))
    pltTitle <- 'Relative bias in total restoration need'
    subTitle = ''
    
  } else {
    strataPlt <- strata %>% 
      filter(type == i) 
    
    pltTitle <- 'Relative bias in restoration need'
    subTitle = unique(filter(strata, type == i)$type_name)
    
  }
  
  ## Strata
  plt <- strataPlt %>%
    ggplot()+
    geom_sf(data = mz, fill = 'grey95', colour = 'black', size = .25) +
    geom_sf(aes(fill = diff), colour = NA) +
    geom_sf(data = mz, fill = NA, colour = 'black', size = .25) +
    geom_sf(data = ws, fill = 'grey95', colour = 'black', size = .25) +
    scale_fill_gradient2(high = coolwarm(10)[1], low = coolwarm(10)[10], mid = coolwarm(1000)[500], guide = "colourbar",
                         na.value = 'grey95', limits = c(-32, 32),
                         breaks = c(-30, -20, -10, 0, 10, 20, 30),
                         labels = c('< -30', '  -20', '  -10',  '   0', '  10', '  20', '> 30')) +
    guides(fill = guide_colourbar(title = "Land Area (%)\n(GNN - FIA)",
                                  reverse = FALSE,
                                  title.position = 'top',
                                  ticks.colour = 'black',
                                  frame.colour = 'black',
                                  frame.linewidth = 1.5,
                                  ticks.linewidth = 1.2)) +
    labs(title = pltTitle, 
         subtitle = subTitle) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(), 
          panel.grid = element_blank(),
          legend.key.height = unit(2.1, 'cm'),
          legend.key.width = unit(1, 'cm'),
          legend.box.margin=margin(c(50,0,50,0)),
          legend.title = element_text(size = 15, face = 'bold'),
          legend.text = element_text(size = 11, face = 'italic'),
          plot.subtitle = element_text(size = 15, face = 'italic', margin = margin(2, 0 , 4, 0)),
          plot.title = element_text(size = 17, face = 'bold'))
  
  
  ggsave(plt, filename = paste0(here('figs/rnDiff/'), 'strata_', ifelse(i == 'both', 'activePassive', i), '.png'),
         height = 7, width = 7)
}

