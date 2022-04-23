library(rFIA)
library(dplyr)
library(ggplot2)
library(sf)

## Read some FIA tables
pnw <- rFIA::readFIA(here::here('data/FIA/'),
                     states = c('WA'),
                     nCores = 10)

# plot-level structure classifications
sclass <- read.csv(here::here('results/FIA/prep/plt_sclass.csv'))

## Read FVS coefficients
coefCW <- read.csv(here::here('data/CW/canopy_cover_coeff.csv'))

## Get NRCS PLANT Dictionary symbols and join onto coefficient table
plantDict <- read.csv(here::here('data/CW/REF_SPECIES.csv')) %>%
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
tree <- pnw$COND %>%
  dplyr::filter(COND_STATUS_CD == 1) %>%
  dplyr::select(PLT_CN, CONDID) %>%
  dplyr::left_join(pnw$TREE, by = c('PLT_CN', 'CONDID')) %>%
  ## Drop all dead trees
  dplyr::filter(STATUSCD == 1) %>%
  ## Here PLT_CN is a unique plot visit ID, TRE_CN is a unique tree visit ID,
  ## DIA is pnwh, SPCD is a species code, HT is total tree height,
  ## CR is compacted crown ratio, and TPA_UNADJ is TPA each tree represents
  ## UNADJ refers to non-response bias, which is handled later. Think of it 
  ## as just standard TPA
  dplyr::select(PLT_CN, TRE_CN=CN, CONDID, SPCD, DIA, HT, CR, TPA_UNADJ, SUBP, DIST, AZIMUTH) %>%
  ## Join on plot attributes
  dplyr::left_join(dplyr::select(pnw$PLOT, CN, LAT, LON, ELEV, STATECD, UNITCD, COUNTYCD, PLOT), 
                   by = c('PLT_CN' = 'CN')) %>%
  dplyr::left_join(dplyr::select(pnw$COND, PLT_CN, CONDID, COND_STATUS_CD),
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
                               is.na(DIA) | is.na(HT) | is.na(CR), 
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
  dplyr::select(PLT_CN, pltID, CONDID, TRE_CN, SUBP, DIST, AZIMUTH, TPA_UNADJ, DIA, HT, crownWidth = cw, crownArea)


## Some plots with only one condition, classified as mid-seral open
plts <- sclass %>% 
  filter(sclass == 'E') %>%
  group_by(PLT_CN) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 1) %>%
  filter(PLT_CN %in% tree$PLT_CN)



pnts <- tree %>%
  filter(PLT_CN == plts$PLT_CN[267]) %>%
  ## Subplot only
  filter(TPA_UNADJ == 6.018046) %>%
  filter(SUBP == 3) %>%
  mutate(rad = AZIMUTH * (pi/180)) %>%
  mutate(x = case_when(AZIMUTH %in% c(0, 180) ~ 0,
                       AZIMUTH %in% c(90, 270) ~ DIST,
                       AZIMUTH < 90 ~ sin(rad) * DIST,
                       AZIMUTH < 180 ~ sin(pi - rad) * DIST,
                       AZIMUTH < 270 ~ -sin(rad - pi) * DIST,
                       AZIMUTH < 360 ~ -sin(2*pi - rad) * DIST),
         y = case_when(AZIMUTH %in% c(0, 180) ~ 0,
                       AZIMUTH %in% c(90, 270) ~ DIST,
                       AZIMUTH < 90 ~ cos(rad) * DIST,
                       AZIMUTH < 180 ~ -cos(pi - rad) * DIST,
                       AZIMUTH < 270 ~ -cos(rad - pi) * DIST,
                       AZIMUTH < 360 ~ cos(2*pi - rad) * DIST)) %>%
  st_as_sf(coords = rev(c('x', 'y')))
#plot(pnts)

crown.buff <- st_buffer(pnts, dist = pnts$crownWidth/1.5)
#plot(buff)

stem.buff <- st_buffer(pnts, dist = pnts$DIA / 12 /1.5)

center <- st_as_sf(data.frame(x=0,y=0), coords = c('x', 'y'))
cent.buff <- st_buffer(center, dist = 24)




crown.plt <- ggplot() +
  geom_sf(data = cent.buff, fill = 'grey95', linetype = 2, colour = 'black') +
  geom_sf(data = crown.buff, fill = '#004d00', alpha = .5) +
  geom_sf(data = stem.buff, fill = '#734d26', linetype = 1, colour = 'black', alpha = .5) +
  theme_void()
crown.plt
ggsave(crown.plt, filename = here::here('figs/conceptual figure/crownMap.pdf'), height = 5, width = 5)

