## Script to recreate Figure ED4 ##
setwd('BDD2018')
rm(list=ls())
library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)

load('data/output/cap-impact.Rdata')
load('data/output/cum-impact.Rdata')
cap.impact %<>% filter(rcp != 'rcp60a') %>%
  mutate(proj = factor(sapply(rcp, function(x){if(x=='rcp26b') 2099 else 2049})))
cum.impact %<>% filter(rcp != 'rcp60a' & discount == 'uniform-3' & sub == 'All') %>%
  mutate(proj = factor(sapply(rcp, function(x){if(x=='rcp26b') 2099 else 2049})))

## Set up color schemes
col.pal <- c('#377eb8', '#e41a1c')
names(col.pal) <- levels(cap.impact$proj)
colScale <- scale_color_manual(name = 'proj', values = col.pal)
fillScale <- scale_fill_manual(name = 'proj', values = col.pal)

#### Panels a-b: Lags ####
cap.lag <- filter(cap.impact, substr(spec, 1, 7) == 'country' & ssp == 'ssp1') %>%
  mutate(lags = substr(as.character(spec), 12, 12))
cum.lag <- filter(cum.impact, substr(spec, 1, 7) == 'country' & ssp == 'ssp1')

cap.lag <- ggplot(cap.lag, aes(100*GDPCapDamage, fill = proj, linetype = lags)) +
  theme_minimal() +
  geom_density(alpha = 0.3, adjust = 1.5) +
  xlab('Change in global GDP/capita (%)') +
  ylab('') + facet_wrap(~ proj, ncol = 1) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(limits = c(-25, 40)) +
  colScale + fillScale

cum.lag <- ggplot(cum.lag, aes(-dam/1e12, fill = proj,
                               linetype = substr(as.character(spec), 9, 12))) +
  theme_minimal() +
  geom_density(alpha = 0.3, adjust = 1.5) +
  xlab('Change in cumulative global GDP (trillions USD)') +
  ylab('') + facet_wrap(~ proj, ncol = 1) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(limits = c(-150, 300)) +
  colScale + fillScale

#### Panels c-d: bootstrap schemes ####

cap.boot <- filter(cap.impact, spec != 'country-lag1' & spec != 'country-lag5' & ssp == 'ssp1') %>%
  mutate(bootstrap.scheme = sapply(as.character(spec), function(x){if(x=='country-lag0') 'country' else x}))
cum.boot <- filter(cum.impact, spec != 'country-lag1' & spec != 'country-lag5' & ssp == 'ssp1' &
                     spec != 'nonlinear' & discount == 'uniform-3')

cap.boot <- ggplot(cap.boot, aes(100*GDPCapDamage, fill = proj, linetype = bootstrap.scheme)) +
  theme_minimal() +
  geom_density(alpha = 0.3) +
  xlab('Change in global GDP/capita (%)') +
  ylab('') + facet_wrap(~ proj, ncol = 1) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(limits = c(-10, 15)) +
  colScale + fillScale

cum.boot <- ggplot(cum.boot, aes(-dam/1e12, fill = proj, linetype = spec)) +
  theme_minimal() +
  geom_density(alpha = 0.3) +
  xlab('Change in cumulative global GDP (trillions USD)') +
  ylab('') + facet_wrap(~ proj, ncol = 1) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(limits = c(-80, 150)) +
  colScale + fillScale

#### Panels e-f: SSPs ####

cap.ssp <- filter(cap.impact, spec == 'country-lag0' & ssp %in% c('ssp1', 'ssp5'))
cum.ssp <- filter(cum.impact, spec == 'country-lag0' & ssp %in% c('ssp1', 'ssp5') &
                    discount == 'uniform-3')

cap.ssp <- ggplot(cap.ssp, aes(100*GDPCapDamage, fill = proj, linetype = ssp)) +
  theme_minimal() +
  geom_density(alpha = 0.3) +
  xlab('Change in global GDP/capita (%)') +
  ylab('') +
  facet_wrap(~ proj, ncol = 1) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(limits = c(-10, 15)) +
  colScale + fillScale

cum.ssp <- ggplot(cum.ssp, aes(-dam/1e12, fill = proj, linetype = ssp)) +
  theme_minimal() +
  geom_density(alpha = 0.3) +
  xlab('Change in cumulative global GDP (trillions USD)') +
  ylab('') +  facet_wrap(~ proj, ncol = 1) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(limits = c(-100, 200)) +
  colScale + fillScale

#### Plotting ####

pdf(file = 'figures/FigED4.pdf', width = 13.5, height = 6)
grid.arrange(cap.lag, cap.boot, cap.ssp,
             cum.lag, cum.boot, cum.ssp,
             ncol = 3)
dev.off()