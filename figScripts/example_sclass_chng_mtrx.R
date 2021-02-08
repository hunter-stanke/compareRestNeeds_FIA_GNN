library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(here)

dat <- read.csv(here('results/FIA/sclassChng/ORWA_comp.csv')) %>% 
  filter(YEAR == 2019) %>%
  select(-c(AREA_DOMAIN1, AREA_DOMAIN2, STATUS1, STATUS2))
net <- read.csv(here('results/FIA/sclassChng/ORWA_net.csv')) %>% 
  filter(YEAR == 2019) %>%
  select(sclass, PERC_CHNG) %>%
  mutate(col = case_when(PERC_CHNG < 0 ~ 'firebrick4',
                         TRUE ~ 'blue4'),
         PERC_CHNG = paste0(round(PERC_CHNG, 2), '%'), 
         PERC_CHNG = case_when(str_sub(PERC_CHNG, 1, 1) != '-' ~ paste0('+', PERC_CHNG),
                               TRUE ~ PERC_CHNG),
         PERC_CHNG = case_when(str_sub(PERC_CHNG, -3, -3) == '.' ~ paste0(str_sub(PERC_CHNG, 1, 4), '0%'),
                               TRUE ~ PERC_CHNG))
dat <- dat %>%
  filter(!is.na(sclass1) & !is.na(sclass2)) %>% ## Drop non-forest for now
  left_join(net, by = c('sclass2' = 'sclass')) %>%
  mutate(sclass1 = case_when(sclass1 == 'A' ~ 'Early seral ',
                             sclass1 == 'B' ~ 'Mid-seral closed ',
                             sclass1 == 'C' ~ 'Mid-seral open ',
                             sclass1 == 'D' ~ 'Late-seral open ',
                             sclass1 == 'E' ~ 'Late-seral closed ',
                             TRUE ~ 'Non-forest ')) %>%
  mutate(sclass2 = case_when(sclass2 == 'A' ~ 'Early seral ',
                             sclass2 == 'B' ~ 'Mid-seral closed ',
                             sclass2 == 'C' ~ 'Mid-seral open ',
                             sclass2 == 'D' ~ 'Late-seral open ',
                             sclass2 == 'E' ~ 'Late-seral closed ',
                             TRUE ~ 'Non-forest ')) %>%
  mutate(sclass1 = factor(sclass1, levels = c('Non-forest ', 'Early seral ', 'Mid-seral closed ', 'Mid-seral open ',
                                              'Late-seral open ', 'Late-seral closed ')),
         sclass2 = factor(sclass2, levels = c('Non-forest ', 'Early seral ', 'Mid-seral closed ', 'Mid-seral open ',
                                              'Late-seral open ', 'Late-seral closed '))) %>%
  mutate(ac = AREA_CHNG / 1000)

  
  
mat <- dat %>%
  ggplot(aes(sclass2, sclass1, size = ac, fill = ac, label = PERC_CHNG)) +
  geom_point(shape = 21) +
  geom_text(y = 5.9, # Set the position of the text to always be at the top
            #hjust = 0,
            size = 4,
            fontface = 'italic',
            colour = dat$col) +
  scale_size_continuous(range = c(1, 10)) +
  theme_bw() +
  xlab('Transition To') +
  ylab('Transition From') +
  scale_fill_viridis_c(option = 'D', breaks = c(25, 50, 75, 100, 125)) +
  guides(size=FALSE, 
         fill = guide_colorbar(title = 'Acres / Year\n(x1000)')) +
  coord_cartesian(clip = 'off',
                  ylim = c(1, 5),
                  xlim = c(1, 5)) +   # This keeps the labels from disappearing
  theme(axis.ticks = element_blank(), 
        #panel.grid = element_blank(),
        #strip.text = element_text(size = 18, face = 'italic'),
        legend.key.height = unit(1.75, 'cm'),
        legend.key.width = unit(.75, 'cm'),
        legend.box.margin=margin(c(50,0,50,0)),
        legend.title = element_text(size = 12, face = 'bold.italic'),
        legend.text = element_text(size = 10,),
        axis.title.x = element_text(size = 15, margin=margin(20,0,0,0), face = 'bold'),
        axis.title.y = element_text(size = 15, margin=margin(0,20,0,0), face = 'bold'),
        axis.text.x = element_text(size = 11, angle = 30, vjust = .8, hjust = .8),
        axis.text.y = element_text(size = 11, angle = 30),
        plot.margin = unit(c(3,2,1,2), "lines"),
        panel.grid.major.x = element_line(size = 1)) # This widens the right margin)

mat
ggsave(mat, filename = here::here('figs/sclassChange/sclass_chng_mtrx_total.png'),
       height = 6, width = 9)
 
  
