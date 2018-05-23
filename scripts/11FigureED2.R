## Script to recreate Figure ED2 ##
setwd('BDD2018')
rm(list=ls())
library(magrittr)
library(dplyr)
library(reshape2)
library(rgdal)
library(classInt)
library(fields)
library(plotrix)
library(ggplot2)
library(gridExtra)
oldw <- getOption('warn')
'%&%' <- function(x,y) paste0(x,y)
select <- dplyr::select

## Load data ##
load('data/output/baseline.Rdata')
load('data/output/projectionOutput/popProjections.Rdata')
load('data/output/country-proj.Rdata')
load('data/output/ctydams2049.Rdata')
load('data/output/ctydams2099.Rdata')
load('data/output/boots.Rdata')
boots %<>% filter(spec == 'country-lag0') %>% arrange(optimal)
load('data/output/globetemp.Rdata')

# 2049 data 
dta.2049 <- filter(country.proj, year == 2049) %>%
  select(iso, GDPcapNoCC) %>% unique %>%
  merge(popProjections[[1]][,c('iso', '2049')], by = 'iso') %>%
  rename(pop.2049 = `2049`, gdpcap.2049 = GDPcapNoCC) %>%
  mutate(pop.2049 = pop.2049*1e6,
         gdp.2049 = pop.2049*gdpcap.2049)

# 2099 data
dta.2099 <- filter(country.proj, year == 2099) %>%
  select(iso, GDPcapNoCC) %>% unique %>%
  merge(popProjections[[1]][,c('iso', '2099')], by = 'iso') %>%
  rename(pop.2099 = `2099`, gdpcap.2099 = GDPcapNoCC) %>%
  mutate(pop.2099 = pop.2099*1e6,
         gdp.2099 = pop.2099*gdpcap.2099)

# 2099 global warming data
dTemp.2099 <- filter(melt(globe.temp), Var1 == 'rcp26b') %>%
  mutate(dTemp = 0.8+value*(2099-2010)) %>%
  select(-c(Var1, value)) %>% rename(mod = Var2) %>% na.omit

dta.all <- merge(baseline, dta.2049, by = 'iso') %>%
  merge(dta.2099, by = 'iso') %>%
  rename(gdp.base = gdp,
         gdpcap.base = gdpcap,
         pop.base = pop,
         base.temp = meantemp) %>%
  mutate(gdpp.base = gdp.base/sum(gdp.base),
         gdpp.2049 = gdp.2049/sum(gdp.2049),
         gdpp.2099 = gdp.2099/sum(gdp.2099))

# 2099 country warming data
country.2099 <- filter(country.proj, year == 2099) %>%
  select(iso, mod, dTemp) %>% unique %>%
  merge(dta.all[,c('iso', 'base.temp')], ., by = 'iso')

# Correlations
dTemp.2099$cor <- sapply(dTemp.2099$mod, function(x) filter(country.2099, mod == x) %>%
                           summarise(cor = cor(base.temp, dTemp)) %>% as.numeric)

#### Panels a-b: gGDP Maps ####

# Map data
world <- readOGR('data/input/shape', 'country')
world.robinson <- spTransform(world, CRS('+proj=robin'))
world.robinson@data$GMI_CNTRY %<>% as.character
world.robinson@data$GMI_CNTRY[world.robinson@data$GMI_CNTRY=='ROM'] <- 'ROU'
world.robinson@data$GMI_CNTRY[world.robinson@data$GMI_CNTRY=='ZAR'] <- 'COD'
world.robinson@data$GMI_CNTRY %<>% as.factor
dta <- world.robinson@data

# Function to plot maps
vars <- c('gdpp.base', 'gdpp.2049', 'gdpp.2099')
ggdp.map <- function(var, min.val = 1e-2, max.val = 0.16, n = 10){
  var2 <- vars[vars!=var][1]
  var3 <- vars[vars!=var][2]
  dta.all[,var][dta.all[,var] > max.val] <- max.val
  dta.all[,var][dta.all[,var] < min.val] <- min.val
  dta.all[,var2][dta.all[,var2] > max.val] <- max.val
  dta.all[,var2][dta.all[,var2] < min.val] <- min.val
  dta.all[,var3][dta.all[,var3] > max.val] <- max.val
  dta.all[,var3][dta.all[,var3] < min.val] <- min.val
  mm <- match(dta$GMI_CNTRY, dta.all$iso)
  brks <- log(c(0.01, 0.02, 0.04, 0.08, 0.16))
  proj <- dta.all[mm,var]
  proj2 <- dta.all[mm,var2]
  proj3 <- dta.all[mm,var3]
  options(warn = -1) # suppress warning caused by mismatch of countries between map and projection data
  quant <- classIntervals(log(c(proj,proj2,proj3)), style = 'fixed', fixedBreaks = brks)
  options(warn = oldw)
  col.pal <- designer.colors(n, col = c('#ffffd4', '#fed98e', '#fe9929', '#d95f0e', '#993404'))
  col.plot <- findColours(quant,col.pal)
  toplot = dta[,3]!='Antarctica'
  
  par(mar=c(0,0,0,0))
  plot(world.robinson[toplot,],col=col.plot[toplot],ylim=c(-80,80))  
  
  # add legend
  rb = c(-20,-60,80,-50)*world.robinson@bbox/world@bbox
  gradient.rect(rb[1], rb[2], rb[3], rb[4], col = smoothColors(col.pal), gradient = 'x', border = 'black')
  tx <- c('< 1', '2', '4', '8', '16+')
  txw = which(brks%in%tx)
  zz = seq(rb[1], rb[3], length = length(tx))
  text(zz, rb[2]-2, tx, pos = 1, cex = 1)
  text((rb[1] + rb[3])/2, rb[2]-13*world.robinson@bbox[2]/world@bbox[2], 'Proportion of gGDP (%)', cex = 1)
  segments(zz,rb[2]-world.robinson@bbox[2]/world@bbox[2], zz, rb[2])
}

#### Panel c: Change in GDP distribution ####

# Coloring the three highlighted bootstraps from Figure 1
samp <- floor(quantile(1:max(boots$boot), c(0.1, 0.5, 0.9))) # 10th, 50th, and 90th percentile optimizing bootstrap
cols <- data.frame(ind = samp,
                   boot = sprintf('boot%0'%&%nchar(max(boots$boot))%&%'d', boots$boot[samp]),
                   color = c('#1b9e77', '#7c4b2a', '#7570b3'))

figed1c <- ggplot(dta.all, aes(x = base.temp)) +
  geom_density(aes(weight = gdp.base/sum(gdp.base)), color = 'black', alpha = 0.2, adjust = 0.1) +
  geom_density(aes(weight = gdp.2099/sum(gdp.2099)), color = '#e41a1c', alpha = 0.2, adjust = 0.1) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab('') +
  xlab('Baseline Temperature (C)') +
  scale_x_continuous(limits = c(-4, 30))

for (i in 1:nrow(cols)){
  figed1c <- figed1c +
    geom_rug(data = boots[cols$ind[i],], aes(optimal), color = cols$color[i], size = 1.5)
}

#### Panel d: Scatter plot ####

figed1d <- ggplot(dTemp.2099, aes(dTemp, cor)) +
  theme_bw() +
  geom_point(shape = 1, color = '#e41a1c') +
  xlab('Global warming (Celsius)') +
  ylab('Correlation (baseline T, delta T)')

#### Plotting ####

pdf(file = 'figures/FigED2ab.pdf', width = 8, height = 8)
layout(matrix(c(1,2,1,2), 2))
ggdp.map('gdpp.base')
ggdp.map('gdpp.2099')
dev.off()

pdf(file = 'figures/FigED2cd.pdf', width = 4, height = 8)
grid.arrange(figed1c, figed1d, nrow = 2)
dev.off()