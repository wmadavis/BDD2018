## Script to recreate Figure ED6 ##
setwd('BDD2018')
rm(list=ls())
library(dplyr)

load('data/output/globe-proj-median.Rdata')

point.impacts <- filter(globe.proj, year == '2099') %>% mutate(dY = dY*100)
ssps <- unique(point.impacts$ssp)
rcps <- unique(point.impacts$rcp)

colz = c('#66c2a5', '#fc8d62', '#8da0cb', '#e78ac3')
cll <- apply(sapply(colz, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=0.6)) 

## Plotting ##

pdf(file = 'figures/FigED6.pdf', width = 6, height = 10, useDingbats = F)
par(mfrow = c(3, 2))
for (k in 1:5){
  plot(1, type='n', xlim=c(0.5, 5.5), ylim = c(-40, 0), las = 1, xlab = 'Global warming (C)',
       ylab = 'Change in global GDP (%)',
       cex.axis = 1.5, cex.lab = 1.3, main = ssps[k])
  abline(v = c(1.5, 2, 2.9))
  abline(h = seq(-40, -10, 10), lty = 2, col= 'grey')
  abline(h = 0,lty = 1, col = 'grey')
  for (s in 1:length(rcps)){
    dta <- point.impacts[point.impacts$rcp==rcps[s] & point.impacts$ssp==ssps[k],]
    points(dta$dTemp,dta$dY,col=cll[s],pch=19,cex=1.5)
  }
  # plot regression lines last
  for (s in 1:length(rcps)){
    dta <- point.impacts[point.impacts$rcp == rcps[s] & point.impacts$ssp==ssps[k],]
    mod <- lm(dY ~ dTemp, data=dta)
    segments(min(dta$dTemp), max(mod$fitted.values), max(dta$dTemp), min(mod$fitted.values), col=colz[s], lwd=4)
    segments(min(dta$dTemp), max(mod$fitted.values), max(dta$dTemp), min(mod$fitted.values), col='black', lwd=0.5)
  }
}
dev.off()