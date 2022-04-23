library(rFIA)
library(sf)
library(dplyr)
library(ggplot2)
library(here)
library(ggspatial)

## Need a reference crs, this will stay on disk
refCRS <- stars::read_stars(here::here('data/GIS/BPS_LLID/BPS_LLID.tif'), proxy = TRUE)

## Map zones
mz <- st_read('data/GIS/mapzones_shp/') %>%
  st_transform(crs = st_crs(refCRS)) %>%
  st_make_valid() %>%
  filter(LLID != 'ZoneNull') %>%
  mutate(eastwest = case_when(LLID %in% c('OCR', 'OWC', 'WCR', 'WNC', 'WWC') ~ 'west',
                              TRUE ~ 'east')) %>%
  group_by(eastwest) %>%
  summarise() %>%
  ungroup() 

## Removing slivers
east <- mz %>%
  filter(eastwest == 'east') %>%
  st_buffer(.00001) 
west <- mz %>%
  filter(eastwest == 'west') %>%
  st_buffer(.00001) 
ew <- bind_rows(east, west)



## State boundaries
states <- st_read(here::here('data/GIS/ORWA/')) %>%
  st_transform(crs = st_crs(refCRS)) %>%
  st_make_valid() %>%
  st_intersection(summarize(ew))



## OR WA FIA database
db <- readFIA(here::here('data/FIA/'),
              nCores = 10)

## Spatial plots
plts <- area(db,
             grpBy = c(LON, LAT),
             polys = ew,
             nCores = 10,
             byPlot = TRUE)

## Zero one column for forestland present
plts$good <- as.numeric(plts$PERC_AREA > 0)

plts.sf <- st_as_sf(plts, coords = c('LON', 'LAT'))
st_crs(plts.sf) <- 4326

plts.sf <- plts.sf %>%
  st_transform(crs = st_crs(refCRS)) %>%
  st_intersection(mz)

state.back <- summarise(states)

mp <- ggplot() +
  ## A grey background
  geom_sf(data = state.back, fill = 'grey95', linetype = 1, size = .1) +
  geom_sf(data = ew, aes(fill = eastwest), size = .5) +
  geom_sf(data = states, fill = NA, linetype = 3, size = .4) +
  geom_sf(data = filter(plts.sf, good == 1), 
          colour = 'white', fill = NA, size = .3,) +
  geom_sf(data = filter(plts.sf, good == 1), 
          colour = '#052a02', fill = NA, size = .05, alpha = .5) +
  scale_fill_manual(values = c('grey95', 'grey82')) +
  xlab('') +
  ylab('') +
  
  ## East west labels
  annotate(geom = "text", y = 1690000, x =  450000, label = "Westside", 
           fontface = 'italic', color = "grey22", size = 7) +
  geom_segment(aes(x = 375000, xend = 325000, 
                   y = 1690000, yend =1690000), 
               lineend = 'round',
               linejoin = 'mitre',
               size = 1.5,
               colour = "grey22",
               arrow = arrow(length = unit(0.15, "inches"))) +
  annotate(geom = "text", y = 1690000, x =  620000, label = "Eastside", 
           fontface = 'italic', color = "grey22", size = 7) +
  geom_segment(aes(x = 700000, xend = 750000, 
                   y = 1690000, yend =1690000), 
               lineend = 'round',
               linejoin = 'mitre',
               size = 1.5,
               colour = "grey22",
               arrow = arrow(length = unit(0.15, "inches"))) +
  
  ## OR WA labels
  annotate(geom = "text", y = 1450000, x =  890000, label = "Washington", angle = -90,
           fontface = 'italic', color = "grey22", size = 7) +
  geom_segment(aes(x = 890000, xend = 890000,
                   y = 1550000, yend =1600000),
               lineend = 'round',
               linejoin = 'mitre',
               size = 1.5,
               colour = "grey22",
               arrow = arrow(length = unit(0.15, "inches"))) +
  annotate(geom = "text", y = 1170000, x =  890000, label = "Oregon", 
           fontface = 'italic', color = "grey22", size = 7, angle = -90) +
  geom_segment(aes(x = 890000, xend = 890000,
                   y = 1100000, yend =1050000),
               lineend = 'round',
               linejoin = 'mitre',
               size = 1.5,
               colour = "grey22",
               arrow = arrow(length = unit(0.15, "inches"))) +
  
  ## Map things
  annotation_scale(location = "bl",
                   width_hint = .6,
                   style = 'tick',
                   #tick_height = .5,
                   pad_y = unit(-0.25, "cm"),
                   pad_x = unit(1.25, 'cm'),
                   text_cex = 1) +
  annotation_north_arrow(location = "br", 
                         which_north = "true",
                         height = unit(1.2, 'cm'),
                         width = unit(.75, 'cm'),
                         pad_y = unit(-.45, "cm"),
                         pad_x = unit(2.5, 'cm')
  ) +
  coord_sf(clip='off') +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none')
#mp
ggsave(mp, filename = here::here('figs/studyArea.pdf'),
       height = 9, width = 7)    

