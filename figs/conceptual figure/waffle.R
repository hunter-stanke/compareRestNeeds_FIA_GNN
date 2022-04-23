library(emojifont)  
library(dplyr)
library(patchwork)
library(ggplot2)

## Some datasets
rows <- 1:10
cols <- 1:10

dat <- tidyr::complete(data.frame(rows, cols), rows, cols)
dat$on <- c(rep('on', 50), rep('off', 50))

top <- ggplot(dat, aes(rows, cols, colour = on)) + 
  geom_point(shape = 15, size = 12, alpha = .8) +
  coord_equal() +
  #coord_flip() +
  theme_void() +
  scale_colour_manual(values = c('grey85', '#2740fa')) +
  theme(legend.position = 'none',
        plot.margin = margin(0, 0, 15, 0))

dat <- tidyr::complete(data.frame(rows, cols), rows, cols)
dat$on <- c(rep('on', 25), rep('off', 75))

bottom <- ggplot(dat, aes(rows, cols, colour = on)) + 
  geom_point(shape = 15, size = 12, alpha = .8) +
  coord_equal() +
  #coord_flip() +
  theme_void() +
  scale_colour_manual(values = c('grey85', '#2740fa')) +
  theme(legend.position = 'none',
        plot.margin = margin(15, 0, 0, 0))

plt <- top / bottom
plt
ggsave(plt, filename = here::here('figs/conceptual figure/waffle.pdf'), height = 10, width = 5)

