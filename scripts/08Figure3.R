## Script to recreate Figure 3 ##
setwd('BDD2018')
rm(list=ls())
library(dplyr)
library(ggplot2)
library(rgdal)
library(magrittr)
library(classInt)
library(fields)
library(plotrix)
library(ggplot2)
library(gridExtra)
oldw <- getOption('warn')

## Load pre-processed data
# Impact data
load('data/output/ctydams2049.Rdata')
load('data/output/ctydams2099.Rdata')
load('data/output/baseline.Rdata')
# Assign colors to RCPs
points.df <- rbind(ctydams.2049, ctydams.2099) %>% merge(baseline, by = 'iso')
col.pal <- c('#377eb8', '#e21e25')
names(col.pal) <- levels(points.df$RCP)
colScale <- scale_color_manual(name = 'RCP', values = col.pal)
fillScale <- scale_fill_manual(name = 'RCP', values = col.pal)

# Map data
world <- readOGR('data/input/shape', 'country')
world.robinson <- spTransform(world, CRS('+proj=robin'))
world.robinson@data$GMI_CNTRY %<>% as.character
world.robinson@data$GMI_CNTRY[world.robinson@data$GMI_CNTRY=='ROM'] <- 'ROU'
world.robinson@data$GMI_CNTRY[world.robinson@data$GMI_CNTRY=='ZAR'] <- 'COD'
world.robinson@data$GMI_CNTRY %<>% as.factor
dta <- world.robinson@data

#### Panels a-b, d-e: Maps ####

# Function to create damage map
dam.map <- function(df, min = -30, max = 20, n = 10){
  df$dam[df$dam < min] <- min
  df$dam[df$dam > max] <- max
  mm <- match(dta$GMI_CNTRY, df$iso)
  proj <- df$dam[mm]
  
  brks <- seq(min, max, length.out = n)
  options(warn = -1) # suppress warning caused by mismatch of countries between map and projection data
  quant <- classIntervals(proj, style = 'fixed', fixedBreaks = c(seq(min, 0, length.out = floor(length(brks)/2)),
                                                                 seq(0, max, length.out = ceiling(length(brks)/2))))
  options(warn = oldw)
  col.pal <- designer.colors(length(brks),
                             col = c('#a41e21', '#f79632', 'white', '#78bfd5', '#06529c'),
                             x = seq(0, 1, 0.25))
  col.plot <- findColours(quant,col.pal)
  toplot <- dta[,3]!='Antarctica'  # make plot without Antarctica
  
  par(mar=c(0,0,0,0))
  plot(world.robinson[toplot,],col=col.plot[toplot],ylim=c(-80,80))  
  
  # add legend
  rb = c(-30,-75,30,-65)*world.robinson@bbox/world@bbox
  gradient.rect(rb[1],rb[2],rb[3],rb[4],col=smoothColors(col.pal),gradient='x',border='black')
  tx <- c(min, min/2, 0, max/2, max)
  txw = which(brks%in%tx)
  zz = seq(rb[1],rb[3],length=length(tx))
  text(zz,rb[2]-2,as.character(tx),pos=1,cex=1)
  text((rb[1]+ rb[3])/2,rb[2]-13*world.robinson@bbox[2]/world@bbox[2],'Gain in GDP/cap (%)',cex=1)
  segments(zz,rb[2]-world.robinson@bbox[2]/world@bbox[2],zz,rb[2])
}

# Function to create probability map
prob.map <- function(df){
  mm <- match(dta$GMI_CNTRY, df$iso)
  proj <- df$prob[mm]
  
  brks <- seq(0,1,length.out = 10)
  options(warn = -1) # suppress warning caused by mismatch of countries between map and projection data
  quant <- classIntervals(proj, style = 'fixed', fixedBreaks = brks)
  options(warn = oldw)
  col.pal <- designer.colors(length(brks),
                             col = c('#a41e21', '#f79632', 'white', '#78bfd5', '#06529c'),
                             x = seq(0, 1, 0.25))
  col.plot <- findColours(quant,col.pal)
  toplot = dta[,3]!='Antarctica'
  
  par(mar=c(0,0,0,0))
  plot(world.robinson[toplot,],col=col.plot[toplot],ylim=c(-80,80))  
  
  # now add legend
  rb = c(-30,-75,30,-65)*world.robinson@bbox/world@bbox
  gradient.rect(rb[1],rb[2],rb[3],rb[4],col=smoothColors(col.pal),gradient='x',border='black')
  tx <- seq(0, 100, 25)
  txw = which(brks%in%tx)
  zz = seq(rb[1],rb[3],length=length(tx))
  text(zz,rb[2]-2,as.character(tx),pos=1,cex=1)
  text((rb[1]+ rb[3])/2,rb[2]-13*world.robinson@bbox[2]/world@bbox[2],'Probability of economic gain (%)',cex=1)
  segments(zz,rb[2]-world.robinson@bbox[2]/world@bbox[2],zz,rb[2])
}

#### Panels c, f: Scatters ####

# Scatters
point.plot <- ggplot(points.df, aes(gdpcap, dam, color = rcp)) +
  scale_x_continuous(trans = 'log', breaks = c(10^2, 10^3, 10^4, 10^5), labels = c(0.1, 1, 10, 100)) +
  #coord_cartesian(ylim = c(-30, 25)) +
  geom_hline(yintercept = 0, col = 'red', lty = 2) +
  geom_point(alpha = 0.5, shape = 1) +
  geom_smooth(method = 'loess') +
  colScale + fillScale +
  xlab('Baseline GDP/cap (thousands USD, log scale)') +
  ylab('Change in GDP/cap, 1.5 vs. 2.0 (%)') +
  theme_bw() +
  theme(legend.position='none') +
  facet_grid(~year, scales = 'free')

prob.plot <- ggplot(points.df, aes(gdpcap, prob*100, color = rcp)) +
  scale_x_continuous(trans = 'log', breaks = c(10^2, 10^3, 10^4, 10^5), labels = c(0.1, 1, 10, 100)) +
  coord_cartesian(ylim = c(0, 100)) +
  geom_point(alpha = 0.5, shape = 1) +
  geom_smooth(method = 'loess') +
  colScale + fillScale +
  geom_hline(yintercept = 50, col = 'red', lty = 2) +
  xlab('Baseline GDP/cap (thousands USD, log scale)') +
  ylab('Probability of economic gain, 1.5 vs. 2.0 (%)') +
  theme_bw() +
  theme(legend.position='none') +
  facet_grid(~year, scales = 'free')

prob.plot <- ggplot_build(prob.plot)
prob.plot$data[[2]]$ymax[prob.plot$data[[2]]$ymax > 100] <- 100
prob.plot$data[[2]]$y[prob.plot$data[[2]]$y>100] <- NA
prob.plot$data[[2]]$ymin[prob.plot$data[[2]]$ymin < 0] <- 0
prob.plot$data[[2]]$y[prob.plot$data[[2]]$y<0] <- NA
prob.plot <- ggplot_gtable(prob.plot)

#### Save plots ####

pdf(file = 'figures/Fig3abde.pdf', width = 24, 12)
layout(matrix(1:4,2,2))
dam.map(ctydams.2049)
dam.map(ctydams.2099)
prob.map(ctydams.2049)
prob.map(ctydams.2099)
dev.off()

pdf(file = 'figures/Fig3cf.pdf', width = 16, height = 4)
grid.arrange(point.plot, prob.plot, nrow = 1)
dev.off()