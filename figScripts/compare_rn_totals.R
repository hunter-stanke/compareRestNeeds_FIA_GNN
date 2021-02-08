library(dplyr)
library(ggplot2)
library(pals)
library(here)

fia <- read.csv(here('results/FIA/restNeed/BPS_LLID_ti_restNeed.csv'))
gnn <- read.csv(here('results/GNN/restNeed/restNeed_BPSLLID.csv'))


fia <- fia %>%
  filter(YEAR == 2011) %>%
  group_by(type) %>%
  summarize(a = sum(REST_ACRES, na.rm = TRUE),
            pct = a / 31011830.0764203 * 100)

gnn <- gnn %>%
  group_by(type) %>%
  summarize(a = sum(REST_ACRES, na.rm = TRUE),
            pct = a / 28660410 * 100)



plt <- fia %>%
  mutate(gnnA = gnn$a,
         gnnPct = gnn$pct) %>%
  mutate(type = case_when(type == 'both' ~ 'Disturbance then\nsuccession ',
                          type == 'active' ~ 'Disturbance ',
                          type == 'passive' ~ 'Succession ')) %>%
  mutate(type = factor(type, levels = rev(c('Disturbance ',
                                            'Succession ',
                                            'Disturbance then\nsuccession ')))) %>%
  ggplot() +
  # remove axes and superfluous grids
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size =16,  margin = margin(10, 0, 0, 0)),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 14, face = 'italic'),
        axis.text.y = element_text(size = 14, face = 'italic'),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 17, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold.italic'),
        plot.subtitle = element_text(size = 15, face = 'italic', margin = margin(2, 0 , 10, 0)),) +
  xlab('Total land area (millions acres)') +
  ylab('')+
  labs(title = "Relative bias in GNN restoration needs", 
       subtitle = "Eastern WA & OR")+
  # add a dummy point for scaling purposes
  geom_point(aes(x = 1, y = type), 
             size = 0, col = "white") + 
  # add the horizontal discipline lines
  geom_hline(yintercept = 1:3, col = "grey80") +
  # add a point for each FIA AREA
  geom_point(aes(x = a/1e6, y = type), 
             size = 18, col = "#97A2A2") +
  # add the text (%) FIA AREA
  geom_text(aes(x = a/1e6, y = type, 
                label = paste0(round(pct, 1), '%')),
            col = "black") +
  # add a point for each GNN AREA
  geom_point(aes(x = gnnA/1e6, y = type),
             size = 18, col = "#18453b")  +
  # add the text (%) GNN AREA
  geom_text(aes(x = gnnA/1e6, y = type, 
                label = paste0(round(gnnPct, 1), '%')),
            col = "white") +
  # add a label above the first two points
  geom_text(aes(x = x, y = y, label = label, col = label),
            data.frame(x = c(6.5 - 1.1, 6.5 + 0.6), y = 3.5, 
                       label = rev(c("FIA", "GNN"))), size = 6) +
  scale_color_manual(values = c("#97A2A2", "#18453b"), guide = "none") +
  # manually specify the x-axis
  scale_x_continuous(breaks = 0:6) +
  # manually set the spacing above and below the plot
  scale_y_discrete(expand = c(0.3, 0)) 
plt


ggsave(plt, filename = here('figs/rnDiff/compareRN.png'),
       height = 6.5, width = 10)



plt <- fia %>%
  mutate(gnnA = gnn$a,
         gnnPct = gnn$pct,
         diffTotal = gnnA - a,
         diffPct = (gnnPct - pct)) %>%
  mutate(type = case_when(type == 'both' ~ 'Disturbance then\nsuccession ',
                          type == 'active' ~ 'Disturbance ',
                          type == 'passive' ~ 'Succession ')) %>%
  mutate(type = factor(type, levels = rev(c('Disturbance ',
                                            'Succession ',
                                            'Disturbance then\nsuccession ')))) %>%
  mutate(label = paste0(round(diffPct, 1), '%'),
         label = case_when(diffPct < 0 ~ paste0(round(diffPct, 1), '%'),
                           TRUE ~ paste0('+', round(diffPct, 1), '%')),
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
  geom_point(aes(x = 1, y = type), 
             size = 0, col = "white") + 
  geom_hline(yintercept = 1:3, colour = "grey80") +
  # add the horizontal discipline lines
  geom_segment(aes(y = .75, yend = 3.25, x = 0, xend = 0), colour = 'grey67', linetype = 2) +
  geom_segment(aes(x = 0, xend = diffTotal/1e6, y = type, yend = type), size = 4, colour = 'grey75')+
  #geom_vline(xintercept = 0, alpha = .67, colour = 'black')+
  # add a point for each difference in absolute land area
  geom_point(aes(x = diffTotal/1e6, y = type, fill = diffTotal), 
             size = 18, shape=21) +
  # add the text (%) for proportion difference
  # geom_text(aes(x = diffTotal/1e6, y = type, 
  #               label = paste0(round(diffPct * 100, 1), '%')),
  #           col = "white") +
  geom_text(aes(x = diffTotal/1e6, y = type, 
                label = label, colour = col)) +
  scale_colour_manual(values = c('black', 'white')) +
  scale_fill_gradient2(high = coolwarm(10)[1], low = coolwarm(10)[10], mid = coolwarm(1000)[500], guide = "colourbar") +
  #scale_x_continuous(limits = c(-4, 4.8),breaks = c(-4, -2, 0, 2, 4)) +
  # manually set the spacing above and below the plot
  scale_y_discrete(expand = c(0.3, 0))+
  scale_x_continuous(expand = c(0.1, 0))+
  annotate("text", x = -1.5, y = 3.5, label = "GNN underpredicts", fontface = 'bold.italic',
           colour = 'firebrick3', size = 4.5, alpha = .67) + 
  annotate("text", x = 1, y = 3.5, label = "GNN overpredicts", fontface = 'bold.italic',
           colour = 'blue3', size = 4.5, alpha = .67) 

plt

ggsave(plt, filename = here('figs/rnDiff/diffRN.png'),
       height = 6.5, width = 10)



