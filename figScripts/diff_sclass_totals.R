library(ggplot2)
library(dplyr)
library(pals)
library(here)


## Read estimates
fia <- read.csv(here('results/FIA/sclass/ORWA_ti.csv')) %>%
  filter(YEAR == 2011) %>%
  mutate(AREA = AREA_TOTAL,
         AREA_TOTAL = sum(AREA))
gnn <- read.csv('results/GNN/sclass/ORWA.csv')


plt <- fia %>%
  mutate(gnnArea = gnn$AREA,
         gnnPct = gnn$AREA_PCT,
         diffTotal = gnnArea - AREA,
         diffPct = (gnnPct - PERC_AREA) / 100) %>%
  mutate(sclass = case_when(sclass == 'A' ~ 'Early seral ',
                                 sclass == 'B' ~ 'Mid-seral closed ',
                                 sclass == 'C' ~ 'Mid-seral open ',
                                 sclass == 'D' ~ 'Late-seral open ',
                                 sclass == 'E' ~ 'Late-seral closed ')) %>%
  mutate(sclass = factor(sclass, levels = c('Early seral ', 'Mid-seral closed ','Mid-seral open ',
                                                      'Late-seral open ', 'Late-seral closed '))) %>%
  mutate(label = paste0(round(diffPct * 100, 1), '%'),
         label = case_when(diffPct < 0 ~ paste0(round(diffPct * 100, 1), '%'),
                           TRUE ~ paste0('+', round(diffPct * 100, 1), '%')),
         col = case_when(abs(diffPct) < .05 ~ 'black',
                         TRUE ~ 'white')) %>%
  ggplot() +
  theme_classic()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size =15.5,  margin = margin(10, 0, 0, 0)),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12.5, face = 'italic'),
        axis.text.y = element_text(size = 12.5, face = 'italic'),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 17, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 15, face = 'italic', margin = margin(2, 0 , 10, 0)),
        legend.position = 'none') +
  xlab('Difference in total land area (millions acres)') +
  ylab('')+
  labs(title = "Relative bias in GNN S-class distributions", 
       subtitle = "Eastern WA & OR", 
       colour = '')+
  # add a dummy point for scaling purposes
  geom_point(aes(x = 1, y = sclass), 
             size = 0, col = "white") + 
  geom_hline(yintercept = 1:5, colour = "grey80") +
  # add the horizontal discipline lines
  geom_segment(aes(y = .75, yend = 5.25, x = 0, xend = 0), colour = 'grey67', linetype = 2) +
  geom_segment(aes(x = 0, xend = diffTotal/1e6, y = sclass, yend = sclass), size = 4, colour = 'grey75')+
  #geom_vline(xintercept = 0, alpha = .67, colour = 'black')+
  # add a point for each difference in absolute land area
  geom_point(aes(x = diffTotal/1e6, y = sclass, fill = diffTotal), 
             size = 18, shape=21) +
  # add the text (%) for proportion difference
  # geom_text(aes(x = diffTotal/1e6, y = sclass, 
  #               label = paste0(round(diffPct * 100, 1), '%')),
  #           col = "white") +
  geom_text(aes(x = diffTotal/1e6, y = sclass, 
                label = label, colour = col)) +
  scale_colour_manual(values = c('black', 'white')) +
  scale_fill_gradient2(high = coolwarm(10)[1], low = coolwarm(10)[10], mid = coolwarm(1000)[500], guide = "colourbar") +
  #scale_x_continuous(limits = c(-4, 4.8),breaks = c(-4, -2, 0, 2, 4)) +
  # manually set the spacing above and below the plot
  scale_y_discrete(expand = c(0.2, 0))+
  annotate("text", x = -2.3, y = 5.5, label = "GNN underpredicts", fontface = 'bold.italic',
           colour = 'firebrick3', size = 4.5, alpha = .67) + 
  annotate("text", x = 2, y = 5.5, label = "GNN overpredicts", fontface = 'bold.italic',
           colour = 'blue3', size = 4.5, alpha = .67) 

plt


ggsave(plt, filename = here('figs/sclassDiff/diff_totals.png'),
       height = 6.5, width = 10)
