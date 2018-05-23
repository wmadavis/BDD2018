## Script to compute discounted global damages ##
setwd('BDD2018')
rm(list=ls())
library(dplyr)
library(reshape2)
library(magrittr)
'%&%' <- function(x,y) paste0(x,y)

# Load bootstrap data
load('data/output/boots.Rdata')

#### PART 1: Preparing discount schemes ####
## Ramsey schemes
disc.rams <- data.frame(discount = c('nordhaus', 'stern', 'weitzman'),
                        rho = c(1.5, 0.1, 2)/100, # using decimal growth rates
                        eta = c(2,1,2))
ssps <- 'ssp'%&%1:5
rcps <- rep(c('rcp26', 'rcp45', 'rcp60', 'rcp85'), each = 2)%&%c('a', 'b')
gcms <- c(32, 42, 25, 39)
specs <- c(unique(boots$spec), 'nonlinear')
rams <- unique(disc.rams$discount)

# For Newell-Pizer and Groom discount rates, we pulled data from plots in their paper since annual estimates not published
# Data were digitized using https://apps.automeris.io/wpd/
options(warn = -1) # Suppress inconsequential warning
disc.np <- read.csv('data/input/NewellPizer2003data.csv', col.names = c('year', 'r')) %>% arrange(year) %>%
  lm(r ~ poly(year, 5, raw = T), data = .) %>%
  predict.lm(data.frame(year=2000:2099)) %>%
  data.frame(year = 2000:2099, discount = 'newellpizer', r = .)
disc.norams <- read.csv('data/input/Groom2007data.csv', col.names = c('year', 'r')) %>% arrange(year) %>%
  lm(r ~ poly(year, 5, raw = T), data = .) %>%
  predict.lm(data.frame(year=2000:2099)) %>%
  data.frame(year = 2000:2099, discount = 'groom', r = .) %>%
  rbind(disc.np) %>% mutate(year = year+10) %>% filter(year <= 2099) %>% arrange(discount, year) %>%
  # Add uniform discount schemes
  rbind(merge(data.frame(year = 2010:2099), data.frame(discount = 'uniform-'%&%c(2.5,3,5), r = c(2.5,3,5)))) %>%
  mutate(r = r/100)
options(warn = 0)

# Save discount rate time series for constructing Fig ED1
save(disc.rams, file = 'data/output/disc-rams.Rdata')
save(disc.norams, file = 'data/output/disc-norams.Rdata')

#### PART 2: Apply discount schemes to compute cumulative damages ####

# Loop to produce discounted damages
beg <- Sys.time()
for (x in 1:length(specs)){
  print('----------- '%&%specs[x]%&%' -----------')
  print(Sys.time()-beg); print('Start = '%&%Sys.time())
  load('data/output/globe-proj-'%&%specs[x]%&%'.Rdata')
  
  # Create all growth-variance time series
  growth.var.ts <- group_by(globe.proj, rcp, year, ssp) %>%
    summarize(All = var(Growth.CC.All),
              Poor = var(Growth.CC.Poor),
              Rich = var(Growth.CC.Rich)) %>%
    melt(id.vars = c('year', 'rcp', 'ssp')) %>%
    rename(sub = variable, var = value)
  
  cumdam.df <- list()
  rcps.sub <- intersect(rcps[c(2:3)], globe.proj$rcp) # indexes the  RCPs used
  ssps.sub <- unique(globe.proj$ssp) # indexes the SSPs used
  rcp.ssp <- expand.grid(rcp = rcps.sub, ssp = ssps.sub)
  if(x == 1){
    rcp.ssp %<>% rbind(data.frame(rcp = rcps[5], ssp = ssps[1])) # Add RCP6.0 if using baseline spec
  }
  boot.list <- list()
  for (j in 1:length(unique(globe.proj$boot))){
    boot.sub <- filter(globe.proj, boot == sprintf('boot%0'%&%nchar(length(unique(boots$boot)))%&%'d',
                                                   unique(boots$boot)[j]))
    if(j%%200==0){print(paste0(j%&%'/'%&%length(unique(boots$boot))))}
    rcp.list <- list()
    for (s in 1:length(unique(rcp.ssp$rcp))){
      rcp.ssp.sub <- filter(rcp.ssp, rcp == unique(rcp.ssp$rcp)[s])
      rcp.sub <- filter(boot.sub, rcp == unique(rcp.ssp$rcp)[s] & ssp %in% rcp.ssp.sub$ssp)
      yr <- max(rcp.sub$year)
      ssp.list <- list()
      for (k in 1:length(unique(rcp.ssp.sub$ssp))){
        ssp.sub <- filter(rcp.sub, ssp == unique(rcp.ssp.sub$ssp)[k])
        mod.list <- list()
        for (m in 1:length(unique(ssp.sub$mod))){
          mod.sub <- filter(ssp.sub, mod == sprintf('gcm%02d',m))[,c(1,21:25, 4:16)] %>%
            melt(id.vars = c('spec', 'boot', 'rcp', 'ssp', 'mod', 'dTemp', 'year')) %>%
            mutate(sub = substring(variable, regexpr('CC.', variable)+3),
                   gdp = sapply(grepl('GDP', variable), function(x) if(x==T) 'GDP' else 'Growth'),
                   nocc = sapply(grepl('NoCC', variable), function(x) if(x==T) 'NoCC' else 'CC')) %>%
            dcast(year + rcp + ssp + dTemp + sub ~ gdp + nocc, value.var = 'value') %>% arrange(year, sub) %>%
            merge(growth.var.ts, by = c('year', 'sub', 'rcp', 'ssp'))
          # Apply different Ramsey discount schemes
          df.disc.list <- list()
          for (d1 in 1:length(rams)){
            disc.sub <- filter(disc.rams, discount == rams[d1])
            # Add columns for year-by-year discount rates
            df.rams <- mod.sub %>%
              mutate(disc.cc = disc.sub$rho + disc.sub$eta*Growth_CC,
                     disc.cc.unc = disc.sub$rho + disc.sub$eta*Growth_CC-0.5*(disc.sub$eta^2)*var,
                     disc.nocc = disc.sub$rho + disc.sub$eta*Growth_NoCC,
                     discount = disc.sub$discount)
            #dss.ord <- names(df.rams)
            df.disc.list[[d1]] <- df.rams
          }
          # Now apply non-Ramsey discounting methods
          for (d2 in 1:length(unique(disc.norams$discount))){
            disc.sub <- filter(disc.norams, discount == unique(disc.norams$discount)[d2])
            df.norams <- mod.sub %>%
              merge(disc.sub, by = 'year') %>%
              mutate(disc.cc = r,
                     disc.cc.unc = r,
                     disc.nocc = r) %>%
              select(-r)
            df.disc.list[[d1+d2]] <- df.norams
          }
          df.disc <- do.call('rbind', df.disc.list) %>%
            mutate(GWPloss.pv = (GDP_CC-GDP_NoCC)/((1+disc.cc)^(year-2010)),
                   GWPloss.unc.pv = (GDP_CC-GDP_NoCC)/((1+disc.cc.unc)^(year-2010))) %>%
            group_by(sub, discount) %>%
            summarize(cumGWPloss = sum(GWPloss.pv),
                      cumGWPloss.unc = sum(GWPloss.unc.pv)) %>%
            mutate(dTemp = unique(mod.sub$dTemp[mod.sub$year==yr]),
                   mod = sprintf('gcm%02d',m))
          df.disc$cumGWPloss.unc[which(df.disc$discount%in%disc.norams$discount)] <- NA
          mod.list[[m]] <- df.disc
        }
        ssp.list[[k]] <- do.call('rbind', mod.list) %>% mutate(ssp = unique(rcp.ssp.sub$ssp)[k])
      }
      rcp.list[[s]] <- do.call('rbind', ssp.list) %>% mutate(rcp = unique(rcp.ssp$rcp)[s])
    }
    boot.list[[j]] <- do.call('rbind', rcp.list) %>%
      mutate(boot = sprintf('boot%0'%&%nchar(length(unique(boots$boot)))%&%'d', unique(boots$boot)[j]))
  }
  cumdam.df <- do.call('rbind', boot.list) %>% mutate(spec = specs[x])
  save(cumdam.df, file = 'data/output/cumdam-df-'%&%specs[x]%&%'.Rdata')
}

#### PART 3: Regressions to compute differential cumulative impacts ####
# Collect all cumulative damages across regression specifications
cum.dam <- list()
for (x in 1:length(specs)){
  load('data/output/cumdam-df-'%&%specs[x]%&%'.Rdata')
  cum.dam[[x]] <- cumdam.df
}
cum.dam %<>% do.call('bind_rows', .)

sub <- unique(cum.dam[,c('ssp', 'spec', 'sub', 'rcp')])
sub$dam <- sub$dam.unc <- NA
boot.list <- list()
for (j in 1:length(unique(boots$boot))){
  boot.sub <- filter(cum.dam, boot == sprintf('boot%0'%&%nchar(length(unique(boots$boot)))%&%'d', unique(boots$boot)[j])) %>%
    mutate_at('discount', as.character)
  disc.list <- list()
  for (d in 1:length(unique(boot.sub$discount))){
    disc.sub <- filter(boot.sub, discount == unique(boot.sub$discount)[d])
    disc.df <- data.frame(boot = sprintf('boot%0'%&%nchar(length(unique(boots$boot)))%&%'d', unique(boots$boot)[j]),
                          discount = unique(boot.sub$discount)[d], sub)
    for (g in 1:nrow(disc.df)){
      group.sub <- merge(disc.sub, sub[g,])
      disc.df$dam[g] <- coefficients(lm(cumGWPloss ~ dTemp, data = group.sub))[2]/2
      if(unique(group.sub$discount) %in% c('nordhaus', 'stern', 'weitzman')){
        disc.df$dam.unc[g] <- coefficients(lm(cumGWPloss.unc ~ dTemp, data = group.sub))[2]/2
      }
    }
    disc.list[[d]] <- disc.df
  }
  boot.list[[j]] <- do.call('rbind', disc.list)
}
cum.impact <- do.call('rbind', boot.list)
save(cum.impact, file = 'data/output/cum-impact.Rdata')