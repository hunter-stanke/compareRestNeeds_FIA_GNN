##=====================================================
##=====================================================
##
## Estimate total forest land area by S-class within
## various domains using the FIADB alone. Adapted
## an rFIA function to make this process easy.
##
## Created:       30 October 2020 - Hunter Stanke
## Last modified: 2 November 2020 - Hunter Stanke
##
##====================================================
##====================================================



##===============================================================================
##  Set up your working directory and number of cores to use --------------------
##===============================================================================

# Set working directory
setwd('/home/hunter/Dropbox/departureR6/')

library(rFIA)
library(dplyr)
library(sf)

# number of cores to use below
cores = 10


##===============================================================================
##  Read and prep FIA database --------------------------------------------------
##===============================================================================

## Read our FIA Data
pnw <- readFIA('/home/hunter/FIA', states = c('OR', 'WA'), nCores = cores)

## Read attributes of landscape units and sclass assignments
# plot-level classifications
sclass <- read.csv('./results/prepData/plt_sclass.csv')
# S-class attributes
sclassAtt <- read.csv('./inputRasters/attributes/sClass.txt')
# Landscape unit attributes
bpsAtt <- read.csv('./results/prepData/fiaPlts_attributes.csv') %>%
  left_join(read.csv('./results/prepData/fiaPlts.csv')) %>%
  select(PLT_CN, pltID, BPS_LLID, BpS, BpS_Code, BpS_Name, FRG, HUC10, HUC8, MAP_ZONE)


## Need to use S-class codes (i.e, 'A', 'B', etc.)
pnw$PLOT <- pnw$PLOT %>%
  left_join(sclass, by = c('CN' = 'PLT_CN')) %>%
  left_join(select(sclassAtt, Value, Sclass_Code), by = c('sclass' = 'Value')) %>%
  ## Add other plot attributes
  left_join(bpsAtt, by = c('CN' = 'PLT_CN'))


## Bring in the lidar footprint shapefile
shp <- st_read('./GNN_AA_lidarFootprints/footprint/footprint.shp') %>%
  summarize() ## Merge polygons so we get one estimate

## Source our estimator/helper functions
source('./scripts/popEstFunctions.R')



# ## Match OR and WA 2010 & 2011 inventories
pnw$POP_EVAL <- pnw$POP_EVAL %>%
  mutate(END_INVYR = case_when(END_INVYR == 2010 ~ 2011,
                               TRUE ~ as.double(END_INVYR)))



##===============================================================================
##  Estimate total/proportion area by S-class -----------------------------------
##===============================================================================

# Across entire region
full <- scArea(pnw, sclass = Sclass_Code, nCores = 2,
               areaDomain = MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') == FALSE) %>%
  filter(YEAR == 2011)

# Within BpS
bps <- scArea(pnw, grpBy = BpS_Code, sclass = Sclass_Code, nCores = 2,
              areaDomain = MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') == FALSE) %>%
  filter(YEAR == 2011)

# Within BPS_LLID
bpsLLID <- scArea(pnw, grpBy = BPS_LLID, sclass = Sclass_Code, nCores = 2,
                  areaDomain = MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') == FALSE) %>%
  filter(YEAR == 2011)

# Within BPS & map zone
bpsMZ <- scArea(pnw, grpBy = c(BpS_Code, MAP_ZONE), sclass = Sclass_Code, nCores = 2,
                areaDomain = MAP_ZONE %in% c('WCR', 'WWC', 'WNC', 'OCR', 'OWC') == FALSE) %>%
  filter(YEAR == 2011)

# Within WA Lidar footprint
lidar <- scArea(pnw, polys = shp, sclass = Sclass_Code, nCores = cores)


##===============================================================================
##  Save our results ------------------------------------------------------------
##===============================================================================
write.csv(full, './results/areaFIA/sclass_ORWA.csv', row.names = FALSE)
write.csv(bps, './results/areaFIA/sclass_BPS.csv', row.names = FALSE)
write.csv(bpsLLID, './results/areaFIA/sclass_BPSLLID.csv', row.names = FALSE)
write.csv(bpsMZ, './results/areaFIA/sclass_BPS_MAPZONE.csv', row.names = FALSE)
write.csv(lidar, './results/areaFIA/sclass_WA_lidarFP.csv', row.names = FALSE)







