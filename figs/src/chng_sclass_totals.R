library(ggplot2)
library(dplyr)
library(pals)
library(here)


## Read estimates
fia <- read.csv(here('EWA/results/FIA/sclassChng/EWA_net.csv')) %>%
  filter(YEAR == 2019) %>%
  mutate(PREV_TOTAL = sum(PREV_AREA, na.rm = TRUE), 
         CURR_AREA = PREV_AREA + (AREA_CHNG * 18),
         CURR_TOTAL = sum(CURR_AREA, na.rm = TRUE),
         t1 = PREV_AREA / PREV_TOTAL,
         t2 = CURR_AREA / CURR_TOTAL)


plt <- fia %>%
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
  labs(title = "Relative change S-class distributions (2001-2019)", 
       subtitle = "Eastern WA & OR")+
  # add a dummy point for scaling purposes
  geom_point(aes(x = 1, y = sclass), 
             size = 0, col = "white") + 
  # add the horizontal discipline lines
  geom_hline(yintercept = 1:5, col = "grey80") +
  # add a point for each FIA AREA
  geom_point(aes(x = PREV_AREA/1e6, y = sclass), 
             size = 16, col = "#97A2A2") +
  # add the text (%) FIA AREA
  geom_text(aes(x = PREV_AREA/1e6, y = sclass, 
                label = paste0(round(t1 * 100, 1), '%')),
            col = "black") +
  # add a point for each GNN AREA
  geom_point(aes(x = CURR_AREA/1e6, y = sclass),
             size = 16, col = "#18453b")  +
  # add the text (%) GNN AREA
  geom_text(aes(x = CURR_AREA/1e6, y = sclass, 
                label = paste0(round((t2*100), 1), '%')),
            col = "white") +
  # add a label above the first two points
  geom_text(aes(x = x, y = y, label = label, col = label),
            data.frame(x = c(7 - 1.1, 7 + 0.6), y = 5.5, 
                       label = c("2001", "2019")), size = 6) +
  scale_color_manual(values = c("#97A2A2", "#18453b"), guide = "none") +
  # manually specify the x-axis
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8)) +
  # manually set the spacing above and below the plot
  scale_y_discrete(expand = c(0.2, 0)) 
plt



ggsave(plt, filename = here('EWA/figs/compare_sclass_chng.png'),
       height = 6.5, width = 10)
