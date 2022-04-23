library(dplyr)
library(ggplot2)

## Reference conditions - NRV +/- 2 SD
refCon <- read.csv(here::here('data/refCon/R6RefCon.txt')) %>%
  dplyr::select(BpS_Code = LF_BpS_Code, sclass = Sclass, avg = Avg_, low = Minus_2_SD, high = Plus_2_SD) %>%
  dplyr::distinct()

## Total forestland for variances
total <- read.csv(here::here('results/FIA/sclass/ORWA_EW_ti.csv')) %>%
  filter(YEAR == 2017) %>%
  select(YEAR, ew, sclass, AREA_TOTAL, AREA_TOTAL_VAR, N) %>%
  mutate(halfint = qt(0.975, N-1) * sqrt(AREA_TOTAL_VAR))

fia <- read.csv(here::here('results/FIA/sclass/ORWA_EW_BPS_ti.csv')) %>%
  filter(YEAR == 2017) %>%
  select(YEAR, ew, BpS_Code, sclass, AREA_TOTAL) %>%
  group_by(YEAR, ew, BpS_Code) %>%
  mutate(AREA_STRATA = sum(AREA_TOTAL, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(refCon, by = c('BpS_Code', 'sclass')) %>%
  dplyr::mutate(dplyr::across(c(avg:high), .fns = function(x, total) {x * .01 * total}, total = .$AREA_STRATA)) %>%
  dplyr::mutate(departure = dplyr::case_when(AREA_TOTAL < low ~ AREA_TOTAL - low,
                                             AREA_TOTAL > high ~ AREA_TOTAL - high,
                                             TRUE ~ 0)) %>%
  group_by(YEAR, ew, sclass) %>%
  summarize(departure = sum(departure, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(select(total, ew, sclass, halfint), by = c('ew', 'sclass')) %>%
  ## Acres to 1000 hectares
  mutate(departure = departure / 2.471 / 1e6,
         halfint = halfint / 2.471 / 1e6) %>%
  mutate(source = 'FIA')


## Merge GNN files
gnn <- list()
for (i in 2002:2017) {
  gnn[[as.character(i)]] <- read.csv(paste0(here::here('results/GNN/sclass/annual/ORWA_EW_BPS_'), i, '.csv'))
}
gnn <- bind_rows(gnn) %>%
  filter(YEAR == 2017) %>%
  select(YEAR, ew, BpS_Code, sclass, AREA_TOTAL) %>%
  group_by(YEAR, ew, BpS_Code) %>%
  mutate(AREA_STRATA = sum(AREA_TOTAL, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(refCon, by = c('BpS_Code', 'sclass')) %>%
  dplyr::mutate(dplyr::across(c(avg:high), .fns = function(x, total) {x * .01 * total}, total = .$AREA_STRATA)) %>%
  dplyr::mutate(departure = dplyr::case_when(AREA_TOTAL < low ~ AREA_TOTAL - low,
                                             AREA_TOTAL > high ~ AREA_TOTAL - high,
                                             TRUE ~ 0)) %>%
  group_by(YEAR, ew, sclass) %>%
  summarize(departure = sum(departure, na.rm = TRUE)) %>%
  ungroup() %>%
  ## Acres to 1000 hectares
  mutate(departure = departure / 2.471 / 1e6) %>%
  mutate(source = 'GNN')




dat <- bind_rows(fia, gnn) %>%
  group_by(source, YEAR, ew) %>%
  ## Clean labels
  mutate(sclass = case_when(sclass == 'A' ~ 'Early seral',
                            sclass == 'B' ~ 'Mid-seral closed',
                            sclass == 'C' ~ 'Mid-seral open',
                            sclass == 'D' ~ 'Late-seral open',
                            sclass == 'E' ~ 'Late-seral closed',
                            TRUE ~ 'Non-forest')) %>%
  mutate(sclass = factor(sclass, levels = c('Non-forest', 'Early seral', 'Mid-seral open', 'Mid-seral closed',
                                            'Late-seral open', 'Late-seral closed')))%>%
  mutate(ew = stringr::str_to_title(ew),
         ew = factor(ew, levels = c('Westside', 'Eastside')))


plt <- dat %>%
  ggplot(aes(y = departure, x = source, fill = source, colour = source)) +

  facet_grid(vars(ew),  vars(sclass)) +

  
  ## Lollipop
  geom_segment(aes(y = departure, yend = 0, x = source, xend = source)) +
  geom_hline(yintercept = 0) +
  geom_point(pch = 21, colour = 'grey30', size = 4) +
  geom_point(pch = 21, colour = 'grey30', fill = 'white', alpha = .25, size = 3) + # Psuedo transparency
  
  
  theme_bw() +
  scale_fill_brewer(palette = "Dark2",
                    guide = guide_legend(title = NULL, nrow = 1)) +
  scale_colour_brewer(palette = "Dark2",
                      guide = guide_legend(title = NULL, size = 2, nrow = 1)) +
  ylab('Ecological Departure (million hectares)') +
  xlab('') +
  scale_y_continuous(breaks = seq(-3, 3, 1)) +
  annotate('text', x= .55, y= 2.5, label = 'Excess', size = 3, angle = 90, colour = 'grey50') +
  annotate('text', x= .55, y= -2.25, label = 'Deficit', size = 3, angle = 90, colour = 'grey50') +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 5 , 0, 0)),
        panel.grid.major.x = element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = 11, face = 'bold',
                                   margin = margin(0, 20, 0, 0)),
        panel.grid = element_line(colour = 'grey95'),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12, face = 'italic', margin = margin(2, 0 , 0, 0)),
        strip.text.y = element_text(size = 9, face = 'italic'),
        strip.text.x = element_text(size = 9))

plt
ggsave(plt, filename =here::here('figs/departure.pdf'),
       width = 7.25, height = 4)
