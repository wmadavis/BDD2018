## Script to recreate Figure 4 ##
setwd('BDD2018')
rm(list=ls())
library(dplyr)
library(magrittr)
select <- dplyr::select

load('data/output/globe-proj-median.Rdata')
load('data/output/cumdam-df-country-lag0.Rdata')

#### Panel a ####
point.impacts <- filter(globe.proj, ssp == 'ssp1')
df.out <- data.frame(rcp = unique(point.impacts$rcp), year = c(2049, 2099)) %>%
  merge(point.impacts) %>% mutate(dY = dY*100)

col <- c('#66c2a5','#fc8d62','#8da0cb','#e78ac3')
colz <- rep(col, each=2)
cll <- apply(sapply(colz, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=0.6)) 
mkty <- rep(c(21, 19), 4)  # Marker style
mksz <- rep(c(1, 1.5), 4)  # Marker size

pdf(file = 'figures/Fig4a.pdf', width=8, height=8.5, useDingbats = F)
plot(1, type='n', xlim=c(0.5, 5.5), ylim = c(-40, 0), las=1, xlab = 'Global warming (C)',
     ylab = 'Change in global GDP (%)', cex.axis = 1.5, cex.lab = 1.3)
abline(v = c(1.5, 2))
for (s in 1:length(unique(df.out$rcp))) {
  dta <- filter(df.out, rcp == unique(df.out$rcp)[s])
  points(dta$dTemp, dta$dY, col=cll[s], pch=mkty[s], cex = mksz[s])
}
# Added shaded region for NDC projected changes
dta <- filter(df.out, rcp=='rcp45b') # Use reg line as upper boundary
mod <- lm(dY ~ dTemp, data=dta)
xx <- c(2.5,3.1)  # From Rogelj et al (2016)
yy <- mod$coefficients[1] + mod$coefficients[2]*xx
polygon(c(xx, rev(xx)), c(-50, -50, rev(yy)), col = 'grey90', border=NA)
# Add regression lines
for (s in 1:length(unique(df.out$rcp))) {
  dta <- filter(df.out,  rcp == unique(df.out$rcp)[s])
  mod <- lm(dY ~ dTemp, data = dta)
  segments(min(dta$dTemp), max(mod$fitted.values), max(dta$dTemp), min(mod$fitted.values), col=colz[s], lwd=4)
  segments(min(dta$dTemp), max(mod$fitted.values), max(dta$dTemp), min(mod$fitted.values), col='black', lwd=0.5)
}
dev.off()

#### Panels b-c ####
cumdam.df %<>% mutate_at('discount', as.character)

# Re-format to treat differentiated response as separate sub
dta <- cumdam.df %>%
  filter(sub!='All' & discount %in% c('nordhaus', 'stern', 'weitzman')) %>%
  mutate(discount = paste0(discount, '_richpoor')) %>%
  group_by(discount, mod, boot, rcp, ssp) %>%
  summarize(cumGWPloss = sum(cumGWPloss),
            cumGWPloss.unc = sum(cumGWPloss.unc),
            dTemp=mean(dTemp)) %>%
  bind_rows(select(filter(cumdam.df, sub == 'All'), -sub))

# Re-format to treat uncertainty estimates as separate discount scheme
dta %<>% as.data.frame() %>%
  filter(is.na(cumGWPloss.unc)==F) %>%
  mutate(cumGWPloss = cumGWPloss.unc,
         discount = paste0(discount, '_unc')) %>%
  bind_rows(dta) %>% select(-cumGWPloss.unc)

## End-of-century RCP2.6
dt <- filter(dta, rcp == 'rcp26b')

# calculate 'median' climate model, discount rate, bootstrap
# median climate model is with respect to dTemp
# other medians are with respect to impacts.
mod <- group_by(dt, mod) %>% summarize(vm = mean(dTemp)) 
med <- abs(mod$vm-median(mod$vm))
modmed = sapply(mod[which(med==min(med)), 1], as.character)
# calculate median bootstrap
mod <- group_by(dt, boot) %>% summarize(vm = mean(cumGWPloss)) 
med <- abs(mod$vm-median(mod$vm))
bootmed = sapply(mod[which(med==min(med)), 1], as.character)  #could set this to regression point estimate
# set reference discount rate to uniform 3%
meddiscount <- 'uniform-3'
# median SSP
mod <- group_by(dt, ssp) %>% summarize(vm = mean(cumGWPloss)) 
med <- abs(mod$vm - median(mod$vm))
medssp <- sapply(mod[which(med==min(med)),1],as.character)  

trimquant <- function(x){
  qq <- quantile(x, probs=c(0.01,0.99))
  xx <- x[x>qq[1] & x<qq[2]] 
  return(xx)
}

# Now calculate uncertainty in discounted impacts, holding all factors except one constant
unc <- list()
unc[[1]] <- dt$cumGWPloss[dt$discount==meddiscount & dt$boot==bootmed & dt$ssp==medssp]  # climate model uncertainty
unc[[2]] <- dt$cumGWPloss[dt$discount==meddiscount & dt$mod==modmed & dt$ssp==medssp]  # bootstrap uncertainty
unc[[3]] <- dt$cumGWPloss[dt$mod==modmed & dt$boot==bootmed & dt$ssp==medssp]  # discount rate uncertainty
unc[[4]] <- dt$cumGWPloss[dt$discount==meddiscount & dt$mod==modmed & dt$boot==bootmed ]  # ssp uncertainty

## Mid-century RCP4.5
dt <- filter(dta,rcp=='rcp45a')
# 'Median' climate model
mod <- group_by(dt, mod) %>% summarize(vm = mean(dTemp)) 
med <- abs(mod$vm - median(mod$vm))
modmed <- sapply(mod[which(med==min(med)),1],as.character)
# median bootstrap
mod <- dt %>% group_by(boot) %>% summarize(vm = mean(cumGWPloss)) 
med <- abs(mod$vm - median(mod$vm))
bootmed <- sapply(mod[which(med==min(med)),1],as.character)  #setting this to regression point estimate; can change if needed
# setting reference discount rate to unfirom 3%
meddiscount <- 'uniform-3'
# median SSP
mod <- dt %>% group_by(ssp) %>% summarize(vm = mean(cumGWPloss)) 
med <- abs(mod$vm - median(mod$vm))
medssp <- sapply(mod[which(med==min(med)),1],as.character)
# Now calculate uncertainty in discounted impacts, holding all factors except one constant
unc_mid <- list()
unc_mid[[1]] <- dt$cumGWPloss[dt$discount==meddiscount & dt$boot==bootmed & dt$ssp==medssp]  #climate model uncertainty
unc_mid[[2]] <- dt$cumGWPloss[dt$discount==meddiscount & dt$mod==modmed & dt$ssp==medssp]  #bootstrap uncertainty
unc_mid[[3]] <- dt$cumGWPloss[dt$mod==modmed & dt$boot==bootmed & dt$ssp==medssp]  #discount rate uncertainty
unc_mid[[4]] <- dt$cumGWPloss[dt$discount==meddiscount & dt$mod==modmed & dt$boot==bootmed ]  #ssp uncertainty

## Panel b: 2049 ##
pdf(file = 'figures/Fig4bc.pdf', width = 4, height = 8, useDingbats = F)
par(mfrow=c(2,1),mar=c(4,3,1,3))
cll <- apply(sapply('black', col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=0.5))
cll2 <- apply(sapply('black', col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=0.1))
cll3 <- apply(sapply('black', col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=0.03))
cllz = c(cll,cll2,cll,cll)  #setting alpha on bootstrap plot to be lighter to distribution is more visible
plot(1, type='n', xlim = c(-150, 100), ylim = c(1, 7), ylab='',
     axes = F, xlab = 'Cumulative GDP loss, 2049 (trillion $)')
abline(v = 0, lty = 2, col = 'grey')
for (i in 1:4) {
  segments(unc_mid[[i]]/1e12, i, unc_mid[[i]]/1e12, i+0.8,  col = cllz[i], lwd = 1)
  mm <- median(unc_mid[[i]])
  segments(mm/1e12, i-0.02, mm/1e12, i+0.82, col = 'red', lwd = 2)
}
axis(1, at = seq(-150, 100, 50))
i <- 5
dt <- filter(dta, rcp == 'rcp45a')
toplot <- dt$cumGWPloss/1e12
ss <- sample(toplot,5000)
segments(ss, i, ss, i+0.8, col = cll3, lwd= 1)
mm <- median(toplot)
segments(mm, i-0.02, mm, i+0.82, col = 'red', lwd = 2)

## Panel c: 2099 ##
plot(1, type='n', xlim = c(-500, 250), ylim = c(1,7), ylab='', axes=F, xlab = 'Cumulative GDP loss, 2099 (trillion $)')
abline(v=0, lty = 2, col='grey')
for (i in 1:4) {
  segments(unc[[i]]/1e12, i, unc[[i]]/1e12, i+0.8, col = cllz[i], lwd=1)
  mm <- median(unc[[i]])
  segments(mm/1e12, i-0.02, mm/1e12, i+0.82, col='red', lwd=2)
}
axis(1, at = seq(-500, 250, 250))
i <- 5
dt <- filter(dta, rcp == 'rcp26b')
toplot <- dt$cumGWPloss/1e12
ss <- sample(toplot, 5000)
segments(ss, i, ss, i+0.8, col = cll3, lwd=1)
mm <- median(toplot)
segments(mm, i-0.02, mm, i+0.82, col='red', lwd=2)
dev.off()