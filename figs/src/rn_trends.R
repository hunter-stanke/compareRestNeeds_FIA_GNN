library(dplyr)
library(ggplot2)

## Total forestland for variances
total <- read.csv(here::here('results/FIA/totalForest/ORWA_EW_annual.csv')) %>%
  filter(YEAR < 2018) %>%
  select(YEAR, ew, AREA_TOTAL, AREA_TOTAL_VAR, N) %>%
  mutate(halfint = qt(0.975, N-1) * sqrt(AREA_TOTAL_VAR))

fia <- read.csv(here::here('results/FIA/restNeed/ORWA_EW_BPS_annual_restNeed.csv')) %>%
  filter(YEAR < 2018) %>%
  select(YEAR, type, ew, BpS_Code, REST_ACRES) %>%
  group_by(YEAR, ew, type) %>%
  summarize(AREA_TOTAL = sum(REST_ACRES)) %>%
  ungroup() %>%
  mutate(source = 'FIA') %>%
  left_join(select(total, YEAR, ew, halfint), by = c('YEAR', 'ew'))


gnn <- read.csv(here::here('results/GNN/restNeed/ORWA_EW_BPS_restNeed.csv')) %>%
  select(YEAR, type, ew, BpS_Code, REST_ACRES) %>%
  group_by(YEAR, ew, type) %>%
  summarize(AREA_TOTAL = sum(REST_ACRES)) %>%
  ungroup() %>%
  mutate(source = 'GNN')


dat <- bind_rows(fia, gnn) %>%
  mutate(type = case_when(type == 'both' ~ 'Disturbance then growth',
                          type == 'active' ~ 'Disturbance only',
                          type == 'passive' ~ 'Growth only')) %>%
  mutate(type = factor(type, levels = rev(c('Disturbance then growth',
                                            'Disturbance only',
                                            'Growth only')))) %>%
  mutate(ew = stringr::str_to_title(ew),
         ew = factor(ew, levels = c('Westside', 'Eastside'))) %>%
  ## Acres to 1000000 hectares
  mutate(a = AREA_TOTAL / 2.471 / 1e6,
         halfint = halfint / 2.471 / 1e6) %>%
  ## Adding transparent points to ensure y scales are the same
  group_by(type) %>%
  mutate(maxdiff = max(a + halfint, na.rm = TRUE) - min(a - halfint, na.rm = TRUE),
         av = mean(a, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(lower = floor(av - (max(maxdiff) / 2)),
         upper = ceiling(av + (max(maxdiff) / 2))) %>%
  mutate(upper = case_when(lower < 0 ~ upper -lower,
                           TRUE ~ upper),
         lower = case_when(lower < 0 ~ 0,
                           TRUE ~ lower)) %>%
  ## make intervals 
  mutate(int.low = a - halfint,
         int.high = a + halfint,
         int.low = case_when(int.low < 0 ~ 0,
                             TRUE ~ int.low),
         int.high = case_when(int.high > 100 ~ 100,
                              TRUE ~ int.high))




plt <- dat %>%
  
  
  ## Set up axes and colors
  ggplot(aes(x = YEAR, y = a, fill = source, colour = source)) +
  
  # Facet by sclass
  facet_grid(vars(type), vars(ew),  scales = 'free') +
  
  # Fixing the y-scales of each facet
  # geom_point(aes(x=YEAR, y = lower), alpha = 0) +
  # geom_point(aes(x=YEAR, y = upper), alpha = 0) +
  
  # Confidence intervals (95%)
  geom_ribbon(data = filter(dat, source == 'FIA'),
              aes(ymin = int.low,
                  ymax = int.high),
              alpha = .1,
              colour = "#1B9E77",
              fill = "#1B9E77",
              linetype = 'dotted') +
  
  # Observed series
  geom_line(alpha = .5) +
  geom_point(pch = 21, colour = 'grey30', size = 2) +
  geom_point(pch = 21, colour = 'grey30', fill = 'white', alpha = .25, size = 2) + # Psuedo transparency
  
  
  # Modeled time-series
  #geom_smooth(method ='loess', se = FALSE, span = 1.7) +
  
  
  
  ## Pretty it up
  theme_bw() +
  scale_fill_brewer(palette = "Dark2",
                    guide = guide_legend(title = NULL, nrow = 1)) +
  scale_colour_brewer(palette = "Dark2",
                      guide = guide_legend(title = NULL, size = 2, nrow = 1)) +
  xlab('') +
  ylab('Restoration Needs (million hectares)') +
  #labs(title = "Relative change in S-class abundance (2002-2017)", 
  #     subtitle = "Westside Washington & Oregon")+
  scale_x_continuous(limits = c(2002, 2017), breaks = c(seq(2002, 2016, 2))) +
  scale_y_continuous(limits = c(0, 3.1), breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(size = 7, angle = 40, 
                                   hjust = .9, vjust = 1),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 10, margin = margin(0, 5, 0, 0)),
        legend.position = 'top',
        legend.text = element_text(size = 11, face = 'bold',
                                   margin = margin(0, 20, 0, 0)),
        panel.grid = element_line(colour = 'grey95'),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = 'italic', margin = margin(2, 0 , 10, 0)),
        strip.text.x = element_text(size = 9, face = 'italic'),
        strip.text.y = element_text(size = 8)
  )
plt

ggsave(plt, filename =here::here('figs/rn.pdf'),
       height = 5.5, width = 3.5)


