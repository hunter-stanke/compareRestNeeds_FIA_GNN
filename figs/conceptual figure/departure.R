library(dplyr)
library(ggplot2)


draws <- rnorm(n = 10000, .1, sd = .02)

rc <- (sd(draws) * 2) + mean(draws)

plt <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 20), colour = 'grey70', fill = 'grey98') +
  geom_density(aes(draws), adjust = 2, fill = '#990033', trim = TRUE) +
  geom_point(aes(y = 25, x = .25), shape = 21, fill = '#2740fa', size = 20) +
  geom_point(aes(y = 35, x = .35), shape = 21, fill = '#2740fa', size = 20) +
  geom_segment(aes(y = 25, yend = 25, x = .215, xend = .15),
               arrow = arrow(length = unit(0.5, "cm"), type="closed"),
               size = 1.4,
               colour = '#990033') +
  geom_segment(aes(y = 35, yend = 35, x = .315, xend = .15),
               arrow = arrow(length = unit(0.5, "cm"), type="closed"),
               size = 1.4,
               colour = '#990033') +
  geom_vline(xintercept = rc, size = 1.2, colour = 'grey35', linetype = 2) +
  theme_bw() +
  scale_x_continuous(limits = c(0.0, 0.4)) +
  scale_y_continuous(limits = c(0, 40), expand = c(0,0)) +
  xlab('Proportion of Forested Landbase') +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plt
ggsave(plt, filename = here::here('figs/conceptual figure/departure.pdf'), height = 5, width = 5)

