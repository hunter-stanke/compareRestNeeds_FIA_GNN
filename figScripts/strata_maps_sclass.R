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
fiaStrata <- read.csv(here('results/FIA/sclass/BPS_LLID_ti.csv')) %>%
  filter(YEAR == 2011) %>%
  group_by(BPS_LLID) %>%
  mutate(a = sum(AREA_TOTAL, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(PERC_AREA = a / AREA_TOTAL,
         PERC_AREA = tidyr::replace_na(PERC_AREA, 0))
gnnStrata <- read.csv(here('results/GNN/sclass/BPS_LLID.csv')) %>%
  tidyr::complete(BPS_LLID, SCLASS) %>%
  mutate(PERC_AREA = tidyr::replace_na(AREA_PCT, 0))


## Make it spatial
strata <- strata %>%
  left_join(select(fiaStrata, BPS_LLID, sclass, PERC_AREA_FIA = PERC_AREA), by = 'BPS_LLID') %>%
  full_join(select(gnnStrata, BPS_LLID, sclass = SCLASS, PERC_AREA_GNN = PERC_AREA), by = c('BPS_LLID', 'sclass')) %>%
  mutate(sclass_code = sclass,
         sclass = case_when(sclass == 0 ~ 'NF',
                                 sclass == 'A' ~ 'Early seral',
                                 sclass == 'B' ~ 'Mid-seral closed',
                                 sclass == 'C' ~ 'Mid-seral open',
                                 sclass == 'D' ~ 'Late-seral open',
                                 sclass == 'E' ~ 'Late-seral closed')) %>%
  mutate(diff = PERC_AREA_GNN - PERC_AREA_FIA) %>%
  ## Putting a max on things so we can pick up differences
  mutate(diff = case_when(is.na(diff) ~ NA_real_,
                          diff > 30 ~ 30,
                          diff < -30 ~ -30,
                          TRUE ~ diff))




sclass <- na.omit(unique(strata$sclass_code))
for (s in sclass) {
  print(s)
  
  # pltTitle = stringr::str_to_sentence(paste('Difference in', 
  #                                           unique(filter(strata, SCLASS == s)$SCLASS_NAME), 'forest'))
  ## Strata
  plt <- strata %>%
    filter(sclass_code == s) %>%
    ggplot()+
    geom_sf(data = mz, fill = 'grey95', colour = 'black', size = .25) +
    geom_sf(aes(fill = diff), colour = NA) +
    geom_sf(data = mz, fill = NA, colour = 'black', size = .25) +
    geom_sf(data = ws, fill = 'grey95', colour = 'black', size = .25) +
    scale_fill_gradient2(high = coolwarm(10)[1], low = coolwarm(10)[10], mid = coolwarm(1000)[500], guide = "colourbar",
                         na.value = 'grey95', limits = c(-32, 32),
                         breaks = c(-30, -20, -10, 0, 10, 20, 30),
                         labels = c('< -30', '  -20', '  -10',  '   0', '  10', '  20', '> 30')) +
    #scale_fill_viridis_c(breaks = c(0, 20, 40, 60, 80), na.value = 'grey95', limits = c(0, 100))+
    guides(fill = guide_colourbar(title = "Land Area (%)\n(GNN - FIA)",
                                  reverse = FALSE,
                                  title.position = 'top',
                                  ticks.colour = 'black',
                                  frame.colour = 'black',
                                  frame.linewidth = 1.5,
                                  ticks.linewidth = 1.2)) +
    labs(title = "Relative bias in S-class distributions", 
         subtitle = unique(filter(strata, sclass_code == s)$sclass)) +    
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
  
  ggsave(plt, filename = paste0(here('figs/sclassDiff/'), 'strata', s, '.png'),
         height = 7, width = 7)
}


