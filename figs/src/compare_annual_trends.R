library(dplyr)
library(ggplot2)


fia <- read.csv(here::here('results/FIA/sclass/ORWA_annual.csv'))%>%
  filter(YEAR < 2018) %>%
  select(YEAR, sclass, AREA_TOTAL) %>%
  mutate(source = 'FIA')

gnn <- read.csv(here::here('results/GNN/sclass/ORWA.csv')) %>%
  mutate(source = 'GNN')


dat <- bind_rows(fia, gnn) %>%
  group_by(source, YEAR) %>%
  mutate(TOTAL = sum(AREA_TOTAL, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(PERC_AREA = AREA_TOTAL / TOTAL * 100)



plt <- dat %>%
  mutate(sclass = case_when(sclass == 'A' ~ 'Early seral ',
                            sclass == 'B' ~ 'Mid-seral closed ',
                            sclass == 'C' ~ 'Mid-seral open ',
                            sclass == 'D' ~ 'Late-seral open ',
                            sclass == 'E' ~ 'Late-seral closed ',
                            TRUE ~ 'Non-forest ')) %>%
  mutate(sclass = factor(sclass, levels = c('Non-forest ', 'Early seral ', 'Mid-seral closed ', 'Mid-seral open ',
                                            'Late-seral open ', 'Late-seral closed '))) %>%
  ggplot(aes(x = YEAR, y = PERC_AREA, colour = source)) +

  
  geom_line(alpha = .5, linetype = 'dashed',) +
  
  geom_point(pch = 21, fill = 'white', size = 3, alpha = .5) +
  # scale_color_brewer(palette = "Dark2",
  #                    guide = guide_legend(title = NULL, size = 3, nrow = 2)) +
  
  geom_smooth(method ='loess', se = FALSE, span = 1.7) +
  
  
  
  facet_wrap(~(sclass), scales = 'free_y') +
  theme_bw() +
  scale_color_brewer(palette = "Dark2",
                     guide = guide_legend(title = NULL, size = 3, nrow = 1)) +
  xlab('') +
  ylab('% of Forested Land Base') +
  labs(title = "Relative change in S-class abundnace (2002-2017)", 
       subtitle = "Eastern WA")+
  scale_x_continuous(limits = c(2002, 2019), breaks = c(seq(2002, 2018, 4))) +
  theme(axis.text.x = element_text(size = 10, angle = 40, 
                                   hjust = .9, vjust = .8),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 12, margin = margin(0, 10, 0, 0)),
        legend.position = c(.86, .23),
        legend.text = element_text(size = 14, face = 'bold',
                                   margin = margin(0, 20, 0, 0)),
        panel.grid = element_line(colour = 'grey95'),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = 'italic', margin = margin(2, 0 , 10, 0)),
        strip.text = element_text(size = 11, face = 'italic'))
plt


ggsave(plt, filename =here::here('figs/compare_annual_trends.png'),
       height = 6.5, width = 10)
