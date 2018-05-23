## Script to recreate Figure 2 ##
setwd('BDD2018')
rm(list=ls())
library(magrittr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
"%&%"<-function(x,y) paste0(x,y)
filter <- dplyr::filter

# Load data
load('data/output/cap-impact.Rdata') # GDP/cap impacts
cap.impact %<>% filter(ssp == 'ssp1' & spec == 'country-lag0') %>%
  mutate(GDPCapDamage = GDPCapDamage*100)
load('data/output/cum-impact.Rdata') # Cumulative GWP impacts
cum.impact %<>% filter(substr(discount, 1, 7)=='uniform' & ssp == 'ssp1' & sub == 'All') %>%
  mutate_at('discount', as.character)

# Color palette for the relevant RCPs
cap.impact$rcp %<>% factor
cum.impact$rcp %<>% factor
col.pal <- c('#e41a1c', '#377eb8','#984ea3')
names(col.pal) <- levels(cap.impact$rcp)
colScale <- scale_color_manual(name = 'rcp', values = col.pal)
fillScale <- scale_fill_manual(name = 'rcp', values = col.pal)

#### Panel a: Impact on per-capita GWP ####

cap.plot <- function(r){
  df <- filter(cap.impact, rcp == r)
  p <- ggplot() +
    theme_minimal() +
    xlab('Change in GDP/capita (%)') +
    ylab('') +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position='none') +
    geom_vline(xintercept = 0, lty = 2) +
    scale_y_continuous(breaks = NULL, limits = c(-0.008767168, 0.184110537)) +
    scale_x_continuous(breaks = seq(-15, 15, 5), limits = c(-10, 15)) +
    colScale + fillScale
  
  q5 <- quantile(df$GDPCapDamage, 0.05)
  q25 <- quantile(df$GDPCapDamage, 0.25)
  q50 <- median(df$GDPCapDamage)
  q75 <- quantile(df$GDPCapDamage, 0.75)
  q95 <- quantile(df$GDPCapDamage, 0.95)
  
  den <- geom_density(data = df, aes(GDPCapDamage, fill = rcp), alpha = 0.2, adjust = 0.75, color = NA)
  p.data <- ggplot_build(p + den)$data[[2]]
  fill = p.data$fill %>% unique
  p <- p + den +
    geom_area(data = filter(p.data, x >= q25 & x <= q75), aes(x=x,y=y), fill = fill, alpha = 0.3) +
    geom_area(data = filter(p.data, x >= q5 & x <= q95), aes(x=x,y=y), fill = fill, alpha = 0.2) +
    geom_vline(data = df, xintercept = q50) +
    theme(legend.position = 'none')
  return(p)
}

#### Panel b: Impact on cumulative GWP ####

cum.plot <- function(r, xmin, xmax){
  cum.impact.sub <- filter(cum.impact, rcp == r)
  p <- ggplot(cum.impact.sub, aes(-dam/1e12, fill = rcp, linetype = discount)) +
    theme_minimal() +
    geom_density(alpha = 0.3, adjust = 0.75) +
    scale_x_continuous(limits = c(xmin, xmax)) +
    xlab('Difference in cumulative global GDP (trillions USD)') +
    ylab('') +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          legend.position='none') +
    geom_vline(xintercept = 0, lty = 2) +
    colScale + fillScale
  return(p)
}

pdf(file = 'figures/Fig2.pdf', width = 12, height = 6)
grid.arrange(cap.plot('rcp45a'), cum.plot('rcp45a', -40, 50),
             cap.plot('rcp60a'), cum.plot('rcp60a', -40, 50),
             cap.plot('rcp26b'), cum.plot('rcp26b', -100, 200),
             ncol = 2)
dev.off()