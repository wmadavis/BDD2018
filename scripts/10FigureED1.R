## Script to recreate Figure ED1 ##
setwd('BDD2018')
rm(list=ls())
library(magrittr)
library(dplyr)
library(reshape2)
'%&%' <- function(x,y) paste0(x,y)

load('data/output/globe-proj-country-lag0.Rdata')
load('data/output/disc-norams.Rdata')
disc.norams %<>% merge(data.frame(discount = c('newellpizer', 'groom'),
                                  col = c('#3953A3', '#EB2426'),
                                  unc = F, sub = 'All'))
load('data/output/disc-rams.Rdata')
# Create time series of various growth rates weighted by GDP
globe.proj %<>% filter(year > 2010 & rcp == 'rcp26b' & ssp == 'ssp1')
growth.series <- globe.proj %>% mutate(year = year/100) %>% group_by(year) %>%
  summarize(rich.growth.cc = weighted.mean(Growth.CC.Rich, GDP.CC.Rich),
            rich.growth.nocc = weighted.mean(Growth.NoCC.Rich, GDP.NoCC.Rich),
            poor.growth.cc = weighted.mean(Growth.CC.Poor, GDP.CC.Poor),
            poor.growth.nocc = weighted.mean(Growth.NoCC.Poor, GDP.NoCC.Poor),
            all.growth.cc = weighted.mean(Growth.CC.All, GDP.CC.All),
            all.growth.nocc = weighted.mean(Growth.NoCC.All, GDP.NoCC.All))*100

growth.var.ts <- list()
for (t in 2011:2099){
  t.sub <- filter(globe.proj, year == t)
  growth.var.ts[[t-2011+1]] <- data.frame(year = t, sub = c('All', 'Poor', 'Rich'),
                                          var = c(var(100*t.sub$Growth.CC.All),
                                                  var(100*t.sub$Growth.CC.Poor),
                                                  var(100*t.sub$Growth.CC.Rich)))
}
growth.var.ts %<>% do.call('rbind', .)

discs <- melt(growth.series, id.vars = 'year') %>%
  merge(disc.rams) %>% mutate(rho = 100*rho) %>%
  mutate(sub = sub('(*.)[.].*', '\\1', variable),
         sub = toupper(substr(sub, 1, 1))%&%substr(sub, 2, nchar(sub))) %>%
  merge(growth.var.ts) %>%
  filter(variable %in% c('rich.growth.cc', 'poor.growth.cc', 'all.growth.cc')) %>%
  mutate(cc = rho + eta*value,
         cc.unc = rho + eta*value-0.5*eta^2*var) %>%
  arrange(discount, year) %>% select(year, discount, sub, cc, cc.unc) %>%
  merge(data.frame(discount = c('nordhaus', 'stern', 'weitzman'),
                   col = c('black', '#8150A0', '#F9A41B'))) %>%
  melt(id.vars = c('year', 'discount', 'sub', 'col')) %>% mutate(discount = discount%&%'.'%&%variable,
                                                          unc = variable!='cc') %>%
  select(year, discount, sub, unc, r = value, col) %>%
  rbind(disc.norams) %>%
  arrange(discount, year)

## Plotting ##
pdf(file='figures/FigED1.pdf',width=10,height=11)
par(mfrow=c(2,2),mar=c(5,5,2,1))
# Panel a
plot(2011:2099, growth.series$all.growth.nocc, las=1, ylim = c(0,3),
     xlim = c(2010, 2100), ylab = 'avg GDP growth rate', xlab = 'year',
     type = 'l', lwd = 2, cex.axis = 1.5, cex.lab = 1.5)
lines(2011:2099, growth.series$all.growth.cc, lwd = 2, lty = 2)
# Panel b
plot(1, type = 'n', las = 1, ylim = c(0, 7.5),
     xlim = c(2011, 2100), ylab = 'discount rate', xlab='year',
     cex.axis = 1.5, cex.lab = 1.5)
for (d in 1:length(unique(discs$discount))){
  sub <- filter(discs, discount == unique(discs$discount)[d] & year > 2010 & sub == 'All')
  color <- sub$col %>% as.character %>% unique
  lines(sub$year, sub$r, col = color, lty=if(T%in%sub$unc) 2 else 1)
}
# Panel c
plot(growth.series$year, growth.series$poor.growth.nocc, las = 1, ylim = c(0, 7),
     xlim = c(2010, 2100), ylab = 'avg GDP growth rate', xlab = 'year',
     type = 'l', lwd = 2, col = 'black', cex.axis = 1.5, cex.lab = 1.5)
lines(growth.series$year, growth.series$poor.growth.cc, col='black', lwd=2, lty=2)
lines(growth.series$year, growth.series$rich.growth.nocc, col='grey70', lwd=2, lty=1)
lines(growth.series$year, growth.series$rich.growth.cc, col='grey70', lwd=2, lty=2)
# Panel d
cols = c('black','#EB2426','#3953A3')
plot(1, type = 'n', las = 1, ylim = c(0, 15), xlim = c(2011, 2100), ylab = 'discount rate', xlab = 'year',
     cex.axis = 1.5, cex.lab = 1.5)
for (d in c('weitzman.cc', 'nordhaus.cc', 'stern.cc')){
  sub.rich <- filter(discs, sub == 'Rich' & unc == F & discount == d)
  sub.poor <- filter(discs, sub == 'Poor' & unc == F & discount == d)
  color <- sub.rich$col %>% as.character %>% unique
  lines(sub.rich$year,sub.rich$r, col = color, lty=1)
  lines(sub.poor$year, sub.poor$r, col = color, lty=2)
}
dev.off()