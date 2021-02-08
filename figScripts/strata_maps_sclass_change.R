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
fiaStrata <- read.csv(here('results/FIA/sclassChng/BPS_LLID_net.csv')) %>%
  filter(YEAR == 2019) %>%
  group_by(BPS_LLID) %>%
  filter(!is.infinite(PERC_CHNG))


## Make it spatial
strata <- strata %>%
  left_join(select(fiaStrata, BPS_LLID, sclass, PERC_CHNG), by = 'BPS_LLID') %>%
  mutate(sclass_code = sclass,
         sclass = case_when(sclass == 0 ~ 'NF',
                            sclass == 'A' ~ 'Early seral',
                            sclass == 'B' ~ 'Mid-seral closed',
                            sclass == 'C' ~ 'Mid-seral open',
                            sclass == 'D' ~ 'Late-seral open',
                            sclass == 'E' ~ 'Late-seral closed')) %>%
  ## Putting a max on things so we can pick up differences
  mutate(diff = case_when(is.na(PERC_CHNG) ~ NA_real_,
                          PERC_CHNG > 10 ~ 10,
                          PERC_CHNG < -10 ~ -10,
                          TRUE ~ PERC_CHNG))




sclass <- na.omit(unique(strata$sclass_code))
for (s in sclass) {
  print(s)
  

  plt <- strata %>%
    filter(sclass_code == s) %>%
    ggplot()+
    geom_sf(data = mz, fill = 'grey95', colour = 'black', size = .25) +
    geom_sf(aes(fill = diff), colour = NA) +
    geom_sf(data = mz, fill = NA, colour = 'black', size = .25) +
    geom_sf(data = ws, fill = 'grey95', colour = 'black', size = .25) +
    scale_fill_gradient2(high = coolwarm(10)[1], low = coolwarm(10)[10], mid = coolwarm(1000)[500], guide = "colourbar",
                         na.value = 'grey95', limits = c(-12, 12),
                         breaks = c(-10, -5, 0, 5, 10),
                         labels = c( '< -10', '  -5',  '   0', '   5', '> 10')) +
    #scale_fill_viridis_c(breaks = c(0, 20, 40, 60, 80), na.value = 'grey95', limits = c(0, 100))+
    guides(fill = guide_colourbar(title = "Land Area (%)",
                                  reverse = FALSE,
                                  title.position = 'top',
                                  ticks.colour = 'black',
                                  frame.colour = 'black',
                                  frame.linewidth = 1.5,
                                  ticks.linewidth = 1.2)) +
    labs(title = "Annual change in S-class distributions (2001-2019)", 
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
  
  ggsave(plt, filename = paste0(here('figs/sclassChange/'), 'strata', s, '.png'),
         height = 7, width = 7)
}


