## Script to recreate Figure ED5 ##
setwd('BDD2018')
rm(list=ls())
library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)

## Pre-processed data
load('data/output/cap-impact-nonlinear.Rdata')
cap.nonlinear <- cap.impact
load('data/output/cap-impact.Rdata')
cap.impact %<>% filter(ssp == 'ssp1' & rcp == 'rcp26b' & spec == 'country-lag0') %>% rbind(cap.nonlinear) %>%
  mutate(warm = sapply(spec, function(x) if(x!='nonlinear') 'linear' else 'nonlinear'))
load('data/output/cum-impact.Rdata')
cum.impact %<>% filter(ssp == 'ssp1' & spec %in% c('country-lag0', 'nonlinear') &
                         rcp == 'rcp26b' & sub == 'All' & discount == 'uniform-2.5') %>%
  mutate(warm = sapply(spec, function(x) if(x!='nonlinear') 'linear' else 'nonlinear'))

figed5a <- ggplot(cap.impact, aes(GDPCapDamage*100, fill = warm, linetype = warm)) +
  theme_minimal() +
  geom_density(alpha = 0.3) +
  xlab('Change in global GDP/cap (%)') +
  ylab('') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #legend.position = 'none',
        axis.title.y = element_blank()) +
  scale_x_continuous(limits = c(-30,30)) +
  geom_vline(xintercept = 0, lty = 2)

figed5b <- ggplot(cum.impact, aes(-dam/1e12, fill = warm, linetype = warm)) +
  theme_minimal() +
  geom_density(alpha = 0.3, adjust = 0.75) +
  xlab('Change in cumulative gGDP (trillions USD)') +
  ylab('') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.title.y = element_blank()) +
  geom_vline(xintercept = 0, lty = 2)

## Plotting ##

pdf(file = 'figures/FigED5.pdf', width = 8, height = 4)
grid.arrange(figed5a, figed5b, nrow = 1)
dev.off()