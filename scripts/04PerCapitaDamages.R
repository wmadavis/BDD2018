## Script to compute global per capita damages ##
setwd('BDD2018')
rm(list=ls())
library(dplyr)
library(magrittr)
library(reshape2)
'%&%' <- function(x,y) paste0(x,y)

# Function to compute end-of-projection damage
damage <- function(df){
  rcps <- unique(df$rcp)
  ssps <- unique(df$ssp)
  boots <- unique(df$boot)
  df.out <- list()
  for (s in 1:length(rcps)){
    ssp.list <- list()
    for (k in 1:length(ssps)){
      boot.list <- list()
      for (j in 1:length(unique(df$boot))){
        sub <- filter(df, rcp == rcps[s] & boot == sprintf('boot%04d', j-1) & ssp == ssps[k])
        yr <- max(sub$year)
        sub %<>% filter(year == yr)
        # For each bootstrap, save the per-0.5-degree Celsius effect of warming
        GDPCapDamage <- lm(-dY ~ dTemp, data = sub)$coefficients[2]/2 # Per capita
        boot.list[[j]] <- data.frame(rcp = rcps[s], GDPCapDamage, boot = sprintf('boot%04d', j-1), ssp = ssps[k])
        if (j%%500==0){print(paste0('RCP: ',s,'/',length(rcps),
                                    ' -- SSP: ',k,'/',length(ssps),
                                    ' -- Bootstrap: ',j,'/',length(boots)))}      }
      ssp.list[[k]] <- do.call('rbind', boot.list)
    }
    df.out[[s]] <- do.call('rbind', ssp.list)
  }
  df.out %<>% do.call('rbind', .)
  return(df.out)
}

#### PART 1: Global damages from warming ####
globes <- list.files('data/output/', pattern = 'globe-proj') %>%
  setdiff(list.files('data/output/', pattern = 'median')) %>%
  setdiff(list.files('data/output/', pattern = 'nonlinear'))

cap.impact <- list()
beg <- Sys.time()
for (g in 1:length(globes)){
  load('data/output/'%&%globes[g])
  # Reduce to end-of-projection and calculate per capita damage
  globe.boot <- globe.proj %>% damage %>%
    cbind(spec = substr(globes[g], 12, nchar(globes[g])-6))
  row.names(globe.boot) <- 1:nrow(globe.boot)
  cap.impact[[g]] <- globe.boot
  print('Spec: '%&%g%&%'/'%&%length(globes))
  print('Start: '%&%beg%&%' -- Current: '%&%Sys.time()); print('Diff: '%&%(Sys.time()-beg))
}
cap.impact %<>% do.call('rbind', .)
save(cap.impact, file = 'data/output/cap-impact.Rdata')

#### PART 2: Global damages from non-linear warming ####
load('data/output/globe-proj-nonlinear.Rdata')
cap.impact <- globe.proj %>% damage %>% cbind(spec = 'nonlinear')
save(cap.impact, file = 'data/output/cap-impact-nonlinear.Rdata')

#### PART 3: Country-level damages ####
load('data/output/country-proj.Rdata')
country.2049 <- filter(country.proj, rcp == 'rcp45a' & year == 2049)
country.2099 <- filter(country.proj, rcp == 'rcp26b' & year == 2099)

# Merge in global temperature change
load('data/output/globetemp.Rdata')
dTemp <- melt(globe.temp, varnames = c('rcp', 'mod'), value.name = 'globetemp') %>% mutate_at(c('rcp', 'mod'), 'as.character')
country.2049 %<>% merge(dTemp, by = c('rcp', 'mod')) %>% mutate(globetemp = globetemp*(year-2010))
country.2099 %<>% merge(dTemp, by = c('rcp', 'mod')) %>% mutate(globetemp = globetemp*(year-2010))

# Function to calculate point estimates and probability of benefit for countries
dam <- function(df){
  ctys <- unique(df$iso)
  cty.dam <- cty.prob <- list()
  for (i in 1:length(ctys)){
    sub <- filter(df, iso == ctys[i])
    boot.list <- list()
    for (j in 1:(length(unique(df$boot)))){
      sub2 <- filter(sub, boot == sprintf('boot%04d',j-1))
      boot.list[[j]] <- data.frame(iso = ctys[i],
                                   dam = 100*lm(-dY ~ globetemp, data = sub2)$coefficients[2]/2,
                                   boot = sprintf('boot%04d',j-1))
    }
    temp <- do.call('rbind', boot.list)
    cty.dam[[i]] <- data.frame(iso = ctys[i], dam = median(temp$dam), prob = length(temp$dam[temp$dam > 0])/(length(unique(df$boot))))
  }
  cty.dam %<>% do.call('rbind', .)
  row.names(cty.dam) <- 1:nrow(cty.dam)
  return(cty.dam)
}
ctydams.2049 <- dam(country.2049) %>% cbind(data.frame(year = 2049, rcp = 'RCP4.5'))
ctydams.2099 <- dam(country.2099) %>% cbind(data.frame(year = 2099, rcp = 'RCP2.6'))

save(ctydams.2049, file = 'data/output/ctydams2049.Rdata')
save(ctydams.2099, file = 'data/output/ctydams2099.Rdata')
