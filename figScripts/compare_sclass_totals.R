library(ggplot2)
library(dplyr)
library(pals)
library(here)


## Read estimates
fia <- read.csv(here('results/FIA/sclass/ORWA_ti.csv')) %>%
  filter(YEAR == 2011) %>%
  mutate(AREA = AREA_TOTAL,
         AREA_TOTAL = sum(AREA),
         test = AREA / AREA_TOTAL)
gnn <- read.csv('results/GNN/sclass/ORWA.csv')


plt <- fia %>%
  mutate(gnnArea = gnn$AREA,
         gnnTotal = gnn$AREA_TOTAL) %>%
  mutate(sclass_code = sclass,
         sclass = case_when(sclass == 'A' ~ 'Early seral ',
                                 sclass == 'B' ~ 'Mid-seral closed ',
                                 sclass == 'C' ~ 'Mid-seral open ',
                                 sclass == 'D' ~ 'Late-seral open ',
                                 sclass == 'E' ~ 'Late-seral closed ')) %>%
  mutate(sclass = factor(sclass, levels = c('Early seral ', 'Mid-seral closed ','Mid-seral open ',
                                                      'Late-seral open ', 'Late-seral closed '))) %>%
  ggplot() +
  # remove axes and superfluous grids
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size =15.5,  margin = margin(10, 0, 0, 0)),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12.5, face = 'italic'),
        axis.text.y = element_text(size = 12.5, face = 'italic'),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 17, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 15, face = 'italic', margin = margin(2, 0 , 10, 0)),) +
  xlab('Total land area (millions acres)') +
  ylab('')+
  labs(title = "Relative bias in GNN S-class distributions", 
       subtitle = "Eastern WA & OR")+
  # add a dummy point for scaling purposes
  geom_point(aes(x = 1, y = sclass), 
             size = 0, col = "white") + 
  # add the horizontal discipline lines
  geom_hline(yintercept = 1:5, col = "grey80") +
  # add a point for each FIA AREA
  geom_point(aes(x = AREA/1e6, y = sclass), 
             size = 16, col = "#97A2A2") +
  # add the text (%) FIA AREA
  geom_text(aes(x = AREA/1e6, y = sclass, 
                label = paste0(round(AREA/AREA_TOTAL * 100, 1), '%')),
            col = "black") +
  # add a point for each GNN AREA
  geom_point(aes(x = gnnArea/1e6, y = sclass),
             size = 16, col = "#18453b")  +
  # add the text (%) GNN AREA
  geom_text(aes(x = gnnArea/1e6, y = sclass, 
                label = paste0(round((gnnArea/gnnTotal*100), 1), '%')),
            col = "white") +
  # add a label above the first two points
  geom_text(aes(x = x, y = y, label = label, col = label),
            data.frame(x = c(12.5 - 1.1, 12.5 + 0.6), y = 5.5, 
                       label = rev(c("FIA", "GNN"))), size = 6) +
  scale_color_manual(values = c("#97A2A2", "#18453b"), guide = "none") +
  # manually specify the x-axis
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  # manually set the spacing above and below the plot
  scale_y_discrete(expand = c(0.2, 0)) 
plt



ggsave(plt, filename = here('figs/sclassDiff/compare_totals.png'),
       height = 6.5, width = 10)
