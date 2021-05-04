library(dplyr)
library(ggplot2)


fia <- read.csv(here::here('EWA/results/FIA/restNeed/BPS_annual_restNeed.csv')) %>%
  filter(YEAR < 2018) %>%
  mutate(source = 'FIA')

years <- 2002:2017
gnnList <- list()
for (year in 1:length(years)) {
  dat <- read.csv(paste0(here::here('EWA/results/GNN/restNeed/'), 'BPS_', years[year], '_restNeed.csv')) %>%
    mutate(YEAR = years[year],
           source = 'GNN') 
  gnnList[[year]] <- dat
}

gnn <- bind_rows(gnnList)

dat <- bind_rows(fia, gnn)


totals <- dat %>%
  distinct(YEAR, BpS, AREA_STRATA, source) %>%
  group_by(YEAR, source) %>%
  summarise(total = sum(AREA_STRATA))





plt <- dat %>%
  group_by(source, YEAR, type) %>%
  summarize(a = sum(REST_ACRES, na.rm = TRUE)) %>%
  left_join(totals, by = c('source', 'YEAR')) %>%
  mutate(pct = a / total * 100) %>%
  mutate(type = case_when(type == 'both' ~ 'Disturbance then\nsuccession',
                          type == 'active' ~ 'Disturbance',
                          type == 'passive' ~ 'Succession')) %>%
  mutate(type = factor(type, levels = rev(c('Disturbance then\nsuccession',
                                            'Disturbance',
                                            'Succession')))) %>%
  ggplot(aes(x = YEAR, y = pct, colour = source)) +
  
  
  geom_line(alpha = .5, linetype = 'dashed',) +
  
  geom_point(pch = 21, fill = 'white', size = 3, alpha = .5) +
  # scale_color_brewer(palette = "Dark2",
  #                    guide = guide_legend(title = NULL, size = 3, nrow = 2)) +
  
  geom_smooth(method ='loess', se = FALSE, span = 1.7) +
  
  
  
  facet_wrap(~(type), scales = 'free_y') +
  theme_bw() +
  scale_color_brewer(palette = "Dark2",
                     guide = guide_legend(title = NULL, size = 3, nrow = 1)) +
  xlab('') +
  ylab('% Restoration Need') +
  scale_x_continuous(limits = c(2002, 2017), breaks = c(seq(2002, 2017, 2))) +
  labs(title = "Relative change in restoration need (2002-2017)", 
       subtitle = "Eastern WA - Conservative strata") +
  theme(axis.text.x = element_text(size = 9, angle = 40, 
                                   hjust = .9, vjust = .8),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 12, margin = margin(0, 10, 0, 0)),
        legend.position = c(.85, 1.5),
        legend.text = element_text(size = 14, face = 'bold',
                                   margin = margin(0, 20, 0, 0)),
        #legend.background = element_blank(),
        #legend.box.background = element_rect(colour = "black"),
        panel.grid = element_line(colour = 'grey95'),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = 'italic', margin = margin(2, 0 , 10, 0)),
        strip.text = element_text(size = 11, face = 'italic'))
plt


ggsave(plt, filename =here::here('EWA/figs/compare_annual_trends_restNeed.png'),
       height = 3.5, width = 10)
