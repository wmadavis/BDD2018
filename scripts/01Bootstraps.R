## Script to derive empirical climate response functions ##
setwd('BDD2018')
rm(list=ls())
library(readstata13)
library(dplyr)
library(data.table)
library(lfe)
'%&%' <- function(x,y) paste0(x,y)
select <- dplyr::select
shift <- data.table::shift
dir.create('data/output')

#### PART 1: Historical Dataset ####

dta <- read.dta13('data/input/GrowthClimateDataset_Stata13.dta') %>%
  select(iso, country = countryname, year, pop = Pop, gdp = TotGDP, growth = growthWDI,temp = UDel_temp_popweight, prec = UDel_precip_popweight) %>%
  mutate(temp2 = temp^2, prec2 = prec^2, time = year-1960, time2 = time^2) %>%
  filter(!(iso %in% c('COD', 'ROU'))) %>%
  mutate(iso = replace(iso, which(iso=='ZAR'), 'COD')) %>%
  mutate(iso = replace(iso, which(iso=='ROM'), 'ROU')) %>% na.omit() %>%
  group_by(iso) %>% arrange(iso, year) %>%
  # Add lag variables
  do(data.frame(., setNames(shift(.$temp, 1:5), paste0('temp.l', 1:5)))) %>%
  do(data.frame(., setNames(shift(.$temp2, 1:5), paste0('temp2.l', 1:5)))) %>%
  do(data.frame(., setNames(shift(.$prec, 1:5), paste0('prec.l', 1:5)))) %>%
  do(data.frame(., setNames(shift(.$prec2, 1:5), paste0('prec2.l', 1:5)))) %>%
  as.data.frame()
# Save baseline data for later
baseline <- dta %>%
  filter(year >= 2000) %>%
  group_by(iso) %>% summarize(meantemp = mean(temp),
                              gdp = mean(gdp),
                              growth = mean(growth),
                              gdpcap = mean(gdp/pop),
                              pop = mean(pop)) %>% as.data.frame
# Dummy for whether country is below median GDP
baseline$poor <- baseline$gdpcap < quantile(baseline$gdpcap, 0.5)
save(baseline, file = 'data/output/baseline.Rdata')

#### PART 2: Running different bootstrap specifications ####

## Shorthand for terms in the various specifications
# Time trends
iso <- unique(dta$iso)
for (y in 1:length(iso)){				
  dta[,'timelin'%&%iso[y]] <- as.numeric(dta$iso==iso[y])*dta$time
  dta[,'timequad'%&%iso[y]] <- as.numeric(dta$iso==iso[y])*dta$time2			
}
trend.lin <- names(dta)[substr(names(dta), 1, 7)=='timelin'] %>%
  paste0(' + ') %>% as.list %>% do.call(paste0, .)
trend.quad <- names(dta)[substr(names(dta), 1, 8)=='timequad'] %>%
  paste0(' + ') %>% as.list %>% do.call(paste0, .)
# Lagged climate variables
temp.lags <- names(dta)[substr(names(dta), 1, 6)=='temp.l'] %>%
  paste0(' + ') %>% as.list %>% do.call(paste0, .)
temp2.lags <- names(dta)[substr(names(dta), 1, 7)=='temp2.l'] %>%
  paste0(' + ') %>% as.list %>% do.call(paste0, .)
prec.lags <- names(dta)[substr(names(dta), 1, 6)=='prec.l'] %>%
  paste0(' + ') %>% as.list %>% do.call(paste0, .)
prec2.lags <- names(dta)[substr(names(dta), 1, 7)=='prec2.l'] %>%
  paste0(' + ') %>% as.list %>% do.call(paste0, .)
# Lag specifications as formula objects
lag0.form <- as.formula('growth ~ temp + temp2 + '%&%trend.lin%&%trend.quad%&%'prec + prec2 | iso + year')
lag1.form <- as.formula('growth ~ temp + temp2 + temp.l1 + temp2.l1 + prec.l1 + prec2.l1 +'%&%trend.lin%&%trend.quad%&%'prec + prec2 | iso + year')
lag5.form <- as.formula('growth ~ temp + temp2 + '%&%temp.lags%&%temp2.lags%&%prec.lags%&%prec2.lags%&%trend.lin%&%trend.quad%&%'prec + prec2 | iso + year')
# Suppress warnings from inclusion of unused regressors. Regression results are not affected by their inclusion.
options(warn = -1) 
# Set number of bootstraps. Changes here will carry over to all other scripts.
N = 1000

## Bootstrap, sampling by countries (with three different lag specifications)
set.seed(94305)
bootC0 <- bootC1 <- bootC5 <- array(dim=c(N+1,2))
for (j in 1:(N+1)){
  cl  <- sample(iso, size = length(iso), replace=T)
  df.bs <- sapply(cl, function(x) which(dta[,'iso']==x))
  dta.boot <- if(j==1) dta else{dta[unlist(df.bs),]} # First row uses all observations
  mod0 <- felm(lag0.form, data = dta.boot)
  mod1 <- felm(lag1.form, data = dta.boot)
  mod1.temp <- sum(mod1$coefficients[c(1,3)])
  mod1.temp2 <- sum(mod1$coefficients[c(2,4)])
  mod5 <- felm(lag5.form, data = dta.boot)
  mod5.temp <- sum(mod5$coefficients[c(1,3:7)])
  mod5.temp2 <- sum(mod5$coefficients[c(2,8:12)])
  bootC0[j,] <- mod0$coefficients[1:2]
  bootC1[j,] <- c(mod1.temp, mod1.temp2)
  bootC5[j,] <- c(mod5.temp, mod5.temp2)
  if(j%%50==0){print(j)}
}

## Bootstrap, sampling by year
set.seed(94305)
bootY <- array(dim=c(N+1,2))  
yrs <- unique(dta$year)
for (j in 1:(N+1)){
  cl  <- sample(yrs, size = length(yrs), replace=T)
  df.bs <- sapply(cl, function(x) which(dta[,'year']==x))
  dta.boot <- dta[unlist(df.bs),]  
  mod <- felm(lag0.form, data=dta.boot)
  bootY[j,] <- mod$coefficients[1:2]
  if(j%%50==0){print(j)}
}

## Bootstrap, sampling by five-year blocks (roughly the length of a business cycle)
set.seed(94305)
dta$yrgroup <- round((dta$year+2)/5)*5  # Assigns each year to one of 10 disjoint blocks
yrg <- unique(dta$yrgroup)
bootYR <- array(dim=c(N+1,2))
for (j in 1:(N+1)) {
  cl  <- sample(yrg, size = length(yrg), replace=T)
  df.bs <- sapply(cl, function(x) which(dta[,'yrgroup']==x))
  dta.boot <- dta[unlist(df.bs),]  
  mod <- felm(lag0.form, data=dta.boot )
  bootYR[j,] <- mod$coefficients[1:2]
  if(j%%50==0){print(j)}
}
options(warn = 0) # Turn off warning suppressor

boots <- rbind(data.frame(bootC0, spec = 'country-lag0'),
               data.frame(bootC1, spec = 'country-lag1'),
               data.frame(bootC5, spec = 'country-lag5'),
               data.frame(bootY, spec = 'year'),
               data.frame(bootYR, spec = 'year-blocks')) %>%
  data.frame(boot = 0:1000) %>%
  rename(b1 = X1, b2 = X2) %>%
  mutate(optimal = -b1/(2*b2),
         normalize = b2*optimal^2+b1*optimal) %>%
  mutate_at('spec', as.character)
save(boots, file = 'data/output/boots.Rdata')