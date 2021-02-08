library(dplyr)
library(ggplot2)

strat <- read.csv(here::here('results/FIA/restNeed/BPS_LLID_annual_restNeed.csv')) %>%
  group_by(YEAR, type) %>%
  summarize(a = sum(REST_ACRES, na.rm = TRUE),
            pct = a / 31011830.0764203 * 100) %>%
  mutate(scale = 'BPS x HUC') %>%
  filter(!c(YEAR %in% 2010:2011))

bps <- read.csv(here::here('results/FIA/restNeed/BPS_annual_restNeed.csv')) %>%
  group_by(YEAR, type) %>%
  summarize(a = sum(REST_ACRES, na.rm = TRUE),
            pct = a / 31011830.0764203 * 100) %>%
  mutate(scale = 'BPS')%>%
  filter(!c(YEAR %in% 2010:2011))

mz <- read.csv(here::here('results/FIA/restNeed/BPS_MAPZONE_annual_restNeed.csv')) %>%
  group_by(YEAR, type) %>%
  summarize(a = sum(REST_ACRES, na.rm = TRUE),
            pct = a / 31011830.0764203 * 100) %>%
  mutate(scale = 'BPS x Map Zone')%>%
  filter(!c(YEAR %in% 2010:2011))


## Restoration Need by type
stratType <- strat %>%
  mutate(type = case_when(type == 'both' ~ 'Disturbance then\nsuccession',
                          type == 'active' ~ 'Disturbance',
                          type == 'passive' ~ 'Succession')) %>%
  mutate(type = factor(type, levels = rev(c('Disturbance then\nsuccession',
                                            'Disturbance',
                                            'Succession')))) %>%
  ggplot(aes(x = YEAR, y = pct, colour = type)) +
  geom_smooth(method = 'lm', linetype = 'dashed', size = .5, se = FALSE, alpha = .1) +
  geom_smooth(alpha = .15, method = 'loess', span = .2, se = F) +
  geom_point(pch = 21, fill = 'white', size = 3) +
  scale_color_brewer(palette = "Dark2",
                     guide = guide_legend(title = NULL, size = 3)) +
  theme_bw() +
  xlab('') +
  ylab('% Restoration Need') +
  scale_x_continuous(limits = c(2002, 2019), breaks = c(seq(2002, 2018, 2))) +
  theme(axis.text.x = element_text(size = 10, angle = 40, 
                                   hjust = .9, vjust = .8),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 12, face = 'bold', margin = margin(0, 10, 0, 0)),
        legend.position = 'top',
        legend.text = element_text(size = 11, face = 'italic',
                                   margin = margin(0, 20, 0, 0)))
stratType
ggsave(stratType, filename = here::here('figs/restNeed_type_strata_annual.png'), height = 7, width = 7)

bpsType <- bps %>%
  mutate(type = case_when(type == 'both' ~ 'Disturbance then\nsuccession',
                          type == 'active' ~ 'Disturbance',
                          type == 'passive' ~ 'Succession')) %>%
  mutate(type = factor(type, levels = rev(c('Disturbance then\nsuccession',
                                            'Disturbance',
                                            'Succession')))) %>%
  ggplot(aes(x = YEAR, y = pct, colour = type)) +
  
  #geom_segment() +
  geom_segment(aes ( x = 2006, xend = 2006, y = 7.75, yend = 22.25), colour = 'grey50',
            arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "closed")) +
  geom_segment(aes ( x = 2011, xend = 2011, y = 7.7, yend = 22.25), colour = 'grey50',
               arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "closed")) +

  annotate('label', x = 2011, y = 22, label = 'DeMeo et al., 2011', fontface = 'italic',
           fill = 'white', colour = 'grey20') +
  annotate('label', x = 2006, y = 22, label = 'Haugo et al., 2006', fontface = 'italic',
           fill = 'white', colour = 'grey20', end = 'arrow') +
  
  geom_segment(aes( x = 2006, xend = 2011, y = 13.665, yend = 13.504), 
               colour = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[2],
               size = .25, linetype = 'dashed') +
  geom_segment(aes( x = 2006, xend = 2011, y = 19.78, yend = 19.09), 
               colour = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[3],
               size = .25, linetype = 'dashed') +
  geom_segment(aes( x = 2006, xend = 2011, y = 7.4, yend = 7.29), 
               colour = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[1],
               size = .25, linetype = 'dashed') +
  
  
  geom_point(aes(x = 2006, y = 13.665), fill = 'grey95',
             shape = 21, size = 4, colour = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[2]) +
  geom_point(aes(x = 2006, y = 19.78), fill = 'grey95',
             shape = 21, size = 4, colour = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[3]) +
  geom_point(aes(x = 2006, y = 7.4), fill = 'grey95',
             shape = 21, size = 4, colour = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[1]) +
  geom_point(aes(x = 2011, y = 13.504),  fill = 'grey95',
             shape = 21, size = 4, colour = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[2]) +
  geom_point(aes(x = 2011, y = 19.09), fill = 'grey95',
             shape = 21, size = 4, colour = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[3]) +
  geom_point(aes(x = 2011, y = 7.29), fill = 'grey95',
             shape = 21, size = 4, colour = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[1]) +
  
  geom_smooth(method = 'lm', linetype = 'dashed', size = .5, se = FALSE, alpha = .1) +
  geom_smooth(alpha = .15, method = 'loess', span = .2, se = F) +
  geom_point(pch = 21, fill = 'white', size = 3) +
  scale_color_brewer(palette = "Dark2",
                     guide = guide_legend(title = NULL, size = 3)) +
  



  theme_bw() +
  xlab('') +
  ylab('% Restoration Need') +
  scale_x_continuous(limits = c(2002, 2019), breaks = c(seq(2002, 2018, 2))) +
  theme(axis.text.x = element_text(size = 10, angle = 40, 
                                   hjust = .9, vjust = .8),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 12, face = 'bold', margin = margin(0, 10, 0, 0)),
        legend.position = 'top',
        legend.text = element_text(size = 11, face = 'italic',
                                   margin = margin(0, 20, 0, 0)),
        panel.grid = element_line(colour = 'grey95'))
bpsType
ggsave(bpsType, filename = here::here('figs/restNeed_type_bps_annual.png'), height = 7, width = 7)


mzType <- mz %>%
  mutate(type = case_when(type == 'both' ~ 'Disturbance then\nsuccession',
                          type == 'active' ~ 'Disturbance',
                          type == 'passive' ~ 'Succession')) %>%
  mutate(type = factor(type, levels = rev(c('Disturbance then\nsuccession',
                                            'Disturbance',
                                            'Succession')))) %>%
  ggplot(aes(x = YEAR, y = pct, colour = type)) +
  geom_smooth(method = 'lm', linetype = 'dashed', size = .5, se = FALSE, alpha = .1) +
  geom_smooth(alpha = .15, method = 'loess', span = .2, se = F) +
  geom_point(pch = 21, fill = 'white', size = 3) +
  scale_color_brewer(palette = "Dark2",
                     guide = guide_legend(title = NULL, size = 3)) +
  theme_bw() +
  xlab('') +
  ylab('% Restoration Need') +
  scale_x_continuous(limits = c(2002, 2019), breaks = c(seq(2002, 2018, 2))) +
  theme(axis.text.x = element_text(size = 10, angle = 40, 
                                   hjust = .9, vjust = .8),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 12, face = 'bold', margin = margin(0, 10, 0, 0)),
        legend.position = 'top',
        legend.text = element_text(size = 11, face = 'italic',
                                   margin = margin(0, 20, 0, 0)))
mzType
ggsave(mzType, filename = here::here('figs/restNeed_type_mz_annual.png'), height = 7, width = 7)

## Total needs
totals <- bps %>%
  bind_rows(strat) %>%
  #bind_rows(mz) %>%
  group_by(YEAR, scale) %>%
  summarize(a = sum(a, na.rm = TRUE)) %>%
  mutate(pct = a / 31011830.0764203 * 100) %>%
  ggplot(aes(x = YEAR, y = pct, colour = scale, group = scale)) +
  geom_smooth(method = 'lm', linetype = 'dashed', size = .5, se = FALSE, alpha = .1) +
  geom_smooth(alpha = .15, method = 'loess', span = .2, se = F) +
  geom_point(pch = 21, fill = 'white', size = 3) +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(n = 9, 'Blues')[8],
                                #RColorBrewer::brewer.pal(n = 9, 'Reds')[8],
                                RColorBrewer::brewer.pal(n = 9, 'Greens')[8]),
                    guide = guide_legend(title = NULL, size = 3)) +
  geom_segment(data = NULL, aes(x = 2006, xend = 2011, y = 41, yend = 37),
               size = .5, colour = 'grey50', linetype = 'dashed') +
  geom_point(aes(x = 2006, y = 41), colour = 'grey20', 
             shape = 21, size = 4, fill = 'grey95') +
  annotate('label', x = 2008.2, y = 42.5, label = 'Haugo et al: 41%', fontface = 'italic',
           fill = 'white', colour = 'grey20') +
  geom_point(aes(x = 2011, y = 37), colour = 'grey20',
             shape = 21, size = 4, fill = 'grey95') +
  annotate('label', x = 2013.2, y = 38.5, label = 'DeMeo et al: 37%', fontface = 'italic',
           fill = 'white', colour = 'grey20') +
  theme_bw() +
  xlab('') +
  ylab('% Restoration Need') +
  #scale_y_continuous(limits = c(49, 57)) +
  scale_x_continuous(limits = c(2002, 2019), breaks = c(seq(2002, 2018, 2))) +
  theme(axis.text.x = element_text(size = 10, angle = 40, 
                                   hjust = .9, vjust = .8),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 12, face = 'bold', margin = margin(0, 10, 0, 0)),
        legend.position = 'top',
        legend.text = element_text(size = 11, face = 'italic',
                                   margin = margin(0, 20, 0, 0)))
totals
ggsave(totals, filename = here::here('figs/restNeed_total_annual.png'), height = 7, width = 7)


