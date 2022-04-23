library(dplyr)
library(ggplot2)


fia <- read.csv(here::here('results/FIA/sclass/ORWA_EW_annual.csv'))%>%
  filter(YEAR < 2018) %>%
  select(YEAR, ew, sclass, AREA_TOTAL, PERC_AREA_VAR, AREA_TOTAL_VAR, N) %>%
  tidyr::complete(YEAR, ew, sclass, fill = list(AREA_TOTAL=0, PERC_AREA_VAR=0, AREA_TOTAL_VAR=0, N=0)) %>%
  mutate(halfint = qt(0.975, N-1) * sqrt(PERC_AREA_VAR)) %>%
  mutate(source = 'FIA')

gnn <- read.csv(here::here('results/GNN/sclass/ORWA_EW.csv')) %>%
  mutate(source = 'GNN')


dat <- bind_rows(fia, gnn) %>%
  group_by(source, YEAR, ew) %>%
  mutate(TOTAL = sum(AREA_TOTAL, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(PERC_AREA = AREA_TOTAL / TOTAL * 100) %>%
  ## Adding transparent points to ensure y scales are the same
  group_by(sclass) %>%
  mutate(maxdiff = max(PERC_AREA + halfint, na.rm = TRUE) - min(PERC_AREA - halfint, na.rm = TRUE),
         av = mean(PERC_AREA, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(lower = floor(av - (max(maxdiff) / 2)),
         upper = ceiling(av + (max(maxdiff) / 2))) %>%
  mutate(upper = case_when(lower < 0 ~ upper + -lower,
                           TRUE ~ upper),
         lower = case_when(lower < 0 ~ 0,
                           TRUE ~ lower)) %>%
  ## make intervals 
  mutate(int.low = PERC_AREA - halfint,
         int.high = PERC_AREA + halfint,
         int.low = case_when(int.low < 0 ~ 0,
                             TRUE ~ int.low),
         int.high = case_when(int.high > 100 ~ 100,
                              TRUE ~ int.high)) %>%
  ## Clean labels
  mutate(sclass = case_when(sclass == 'A' ~ 'Early seral',
                            sclass == 'B' ~ 'Mid-seral closed',
                            sclass == 'C' ~ 'Mid-seral open',
                            sclass == 'D' ~ 'Late-seral open',
                            sclass == 'E' ~ 'Late-seral closed',
                            TRUE ~ 'Non-forest')) %>%
  mutate(sclass = factor(sclass, levels = c('Non-forest', 'Early seral', 'Mid-seral open', 'Mid-seral closed',
                                            'Late-seral open', 'Late-seral closed'))) %>%
  mutate(ew = stringr::str_to_title(ew),
         ew = factor(ew, levels = c('Westside', 'Eastside')))




plt <- dat %>%
  

  ## Set up axes and colors
  ggplot(aes(x = YEAR, y = PERC_AREA, fill = source, colour = source)) +
  
  # Facet by sclass
  facet_grid(vars(sclass),  vars(ew), scales = 'free') +
  
  #Fixing the y-scales of each facet
  geom_point(aes(x=YEAR, y = lower), alpha = 0) +
  geom_point(aes(x=YEAR, y = upper), alpha = 0) +

  # Confidence intervals (95%)
  geom_ribbon(data = filter(dat, source == 'FIA'),
              aes(ymin = int.low, 
                  ymax = int.high),
              alpha = .2,
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
  ylab('% of Forested Landbase') +
  #labs(title = "Relative change in S-class abundance (2002-2017)", 
  #     subtitle = "Westside Washington & Oregon")+
  scale_x_continuous(limits = c(2002, 2017), breaks = c(seq(2002, 2016, 2))) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(size = 7, angle = 40, 
                                   hjust = .9, vjust = 1),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size = 10, margin = margin(0, 5, 0, 0)),
        legend.position = 'top',
        legend.text = element_text(size = 11, face = 'bold',
                                   margin = margin(0, 20, 0, 0)),
        panel.grid = element_line(colour = 'grey95'),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = 'italic', margin = margin(2, 0 , 0, 0)),
        strip.text.x = element_text(size = 9, face = 'italic'),
        strip.text.y = element_text(size = 8)
  )
plt


ggsave(plt, filename =here::here('figs/sclass.pdf'),
       height = 6.5, width = 3.5)
