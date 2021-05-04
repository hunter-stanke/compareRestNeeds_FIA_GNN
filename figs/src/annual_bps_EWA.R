library(dplyr)
library(ggplot2)


bps <- read.csv(here::here('EWA/results/FIA/restNeed/BPS_annual_restNeed.csv')) 

bpsTotal <- bps %>%
  distinct(YEAR, BpS, AREA_STRATA) %>%
  group_by(YEAR) %>%
  summarise(total = sum(AREA_STRATA))


bpsType <- bps %>%
  group_by(YEAR, type) %>%
  summarize(a = sum(REST_ACRES, na.rm = TRUE)) %>%
  left_join(bpsTotal, by = c('YEAR')) %>%
  mutate(pct = a / total * 100) %>%
  mutate(type = case_when(type == 'both' ~ 'Disturbance then\nsuccession',
                          type == 'active' ~ 'Disturbance',
                          type == 'passive' ~ 'Succession')) %>%
  mutate(type = factor(type, levels = rev(c('Disturbance then\nsuccession',
                                            'Disturbance',
                                            'Succession')))) %>%
  ggplot(aes(x = YEAR, y = pct, colour = type)) +
  geom_segment(aes (x = 2016.75, xend = 2015,y = 17.5, yend = 12.5), colour = 'grey60',
               arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "closed")) +
  
  geom_segment(aes (x = 2016.5, xend = 2015,y = 13.4, yend = 12.5), colour = 'grey60',
               arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "closed")) +
  
  geom_segment(aes (x = 2016.5, xend = 2015,y = 8.25, yend = 12.5), colour = 'grey60',
               arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "closed")) +
  annotate('label', x = 2014, y = 12.5, label = '2017 GNN ', fontface = 'italic',
           fill = 'white', colour = 'grey20') +
  geom_point(aes(x = 2017, y = 13.5),  colour = 'grey50',
             shape = 21, size = 4, fill = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[2]) +
  geom_point(aes(x = 2017, y = 18.2), colour = 'grey50',
             shape = 21, size = 4, fill = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[3]) +
  geom_point(aes(x = 2017, y = 7.0), colour = 'grey50',
             shape = 21, size = 4, fill = RColorBrewer::brewer.pal(n=3, name = 'Dark2')[1]) +

  #geom_smooth(alpha = .15, method = 'lm', se = F) +
  geom_line(alpha = .5, linetype = 'dashed',) +
  
  geom_point(pch = 21, fill = 'white', size = 3, alpha = .5) +
  scale_color_brewer(palette = "Dark2",
                     guide = guide_legend(title = NULL, size = 3)) +
  
  geom_smooth(method ='loess', se = FALSE, span = 1.5) +
  theme_bw() +
  xlab('') +
  ylab('% Restoration Need') +
  scale_x_continuous(limits = c(2002, 2019), breaks = c(seq(2002, 2018, 2))) +
  labs(title = "Relative change in S-class abundnace (2002-2017)", 
       subtitle = "Eastern WA - Conservative strata")+
  theme(axis.text.x = element_text(size = 10, angle = 40, 
                                   hjust = .9, vjust = .8),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = 'italic', margin = margin(2, 0 , 10, 0)),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 12, face = 'bold', margin = margin(0, 10, 0, 0)),
        legend.position = 'top',
        legend.text = element_text(size = 11, face = 'italic',
                                   margin = margin(0, 20, 0, 0)),
        panel.grid = element_line(colour = 'grey95'))
bpsType
ggsave(bpsType, filename = here::here('EWA/figs/restNeed_type_bps_annual.png'), height = 6.5, width = 10)

