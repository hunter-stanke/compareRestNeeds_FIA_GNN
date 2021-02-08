library(dplyr)
library(ggplot2)
library(pals)
library(here)

dat <- read.csv(here('results/FIA/restNeedChng/BPS_LLID_restNeed.csv'))

## Area totals
a <- read.csv(here('results/FIA/sclassChng/ORWA_net.csv')) %>%
  filter(YEAR == 2019) %>%
  summarize(fa1 = sum(PREV_AREA, na.rm = TRUE),
            fa2 = sum(PREV_AREA + (AREA_CHNG*18), na.rm = TRUE))


dat <- dat %>%
  filter(YEAR == 2019) %>%
  group_by(type) %>%
  summarize(rn1 = sum(PREV_REST_ACRES, na.rm = TRUE),
            rn2 = sum(PREV_REST_ACRES + CHNG_REST_ACRES, na.rm = TRUE)) %>%
  mutate(fa1 = a$fa1,
         fa2 = a$fa2,
         p1 = rn1 / fa1,
         p2 = rn2 / fa2)



plt <- dat %>%
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
  labs(title = "Relative change in restoration need (2001-2019)", 
       subtitle = "Eastern WA & OR")+
  # add a dummy point for scaling purposes
  geom_point(aes(x = 1, y = type), 
             size = 0, col = "white") + 
  # add the horizontal discipline lines
  geom_hline(yintercept = 1:3, col = "grey80") +
  # add a point for each FIA AREA
  geom_point(aes(x = rn1/1e6, y = type), 
             size = 18, col = "#97A2A2") +
  # add the text (%) FIA AREA
  geom_text(aes(x = rn1/1e6, y = type, 
                label = paste0(round(p1*100, 1), '%')),
            col = "black") +
  # add a point for each GNN AREA
  geom_point(aes(x = rn2/1e6, y = type),
             size = 18, col = "#18453b")  +
  # add the text (%) GNN AREA
  geom_text(aes(x = rn2/1e6, y = type, 
                label = paste0(round(p2*100, 1), '%')),
            col = "white") +
  # add a label above the first two points
  geom_text(aes(x = x, y = y, label = label, col = label),
            data.frame(x = c(6.5 - 1.1, 6.5 + 0.6), y = 3.5, 
                       label = c("2001", "2019")), size = 6) +
  scale_color_manual(values = c("#97A2A2", "#18453b"), guide = "none") +
  # manually specify the x-axis
  scale_x_continuous(breaks = 0:6) +
  # manually set the spacing above and below the plot
  scale_y_discrete(expand = c(0.3, 0)) 
plt


ggsave(plt, filename = here('figs/rnChange/compare_rn_chng.png'),
       height = 6.5, width = 10)
