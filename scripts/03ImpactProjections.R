## Script to run all climate projections ##
setwd('BDD2018')
rm(list=ls())
library(dplyr)
library(tibble)
library(magrittr)
library(reshape2)
'%&%' <- function(x,y) paste0(x,y)

# Load pre-processed data
load('data/output/boots.Rdata')
load('data/output/baseline.Rdata')
load('data/output/cty.Rdata')
load('data/output/globetemp.Rdata')
load('data/output/ctytemp.Rdata')
load('data/output/projectionOutput/popProjections.Rdata')
load('data/output/projectionOutput/growthProjections.Rdata')

## Identifying our projection inputs
rcps <- rep(c('rcp26', 'rcp45', 'rcp60', 'rcp85'), each = 2)%&%c('a', 'b')
gcms <- c(32, 42, 25, 39) # These are the number of GCMs associated with the four RCPs
ssps <- 'ssp'%&%1:5
specs <- unique(boots$spec)

## Note that here and in all other scripts, we try to maintain consistent indexing such that
# x indexes bootstrapping method
# s indexes RCP
# j indexes bootstrap
# m indexes GCM
# k indexes SSP
# t indexes year
# d indexes discount scheme
# i indexes country

#### PART 1: Global economic pathways — all bootstraps ####
# For flexibility, we separate the global and country-level projections
# These are the main results

## The projection loop
for (x in 1:length(specs)){
  # Restricting to scenarios that appear in the main text. Can be adapted to include other scenarios
  rcp <- if(specs[x] == 'country-lag0') c(2,3,5) else 2:3 # indexes the three RCPs used
  ssp <- if(specs[x] == 'country-lag0') 1:5 else 1 # indexes the two SSPs used
  boot.spec <- filter(boots, spec == specs[x])
  globe.proj <- list()
  for (s in rcp){ # Begin RCP loop
    nmod <- gcms[ceiling(s/2)] # Number of GCMs in the RCP indexed s
    yr <- if (s%%2!=0) 2049 else 2099
    boot.list <- list()
    for (j in 1:nrow(boot.spec)){ # Begin bootstrap loop
      mod.list <- list()
      for (m in 1:nmod){ # Begin GCM loop
        df <- data.frame(cty@data$ISO, rownames_to_column(data.frame(cty.temp[,s,m])))
        colnames(df) <- c('iso', 'country', 'dTemp')
        df %<>% merge(popProjections[[1]][,c(1,2,4)], ., by = 'iso')
        # Annualize projected temperature changes for each country
        ccd <- if(s%%2!=0) df$dTemp/(mean(c(2046, 2065))-mean(c(1986, 2005))) else{
          df$dTemp/(mean(c(2081, 2100))-mean(c(1986, 2005)))
        }
        # Read in bootstrapped estimates
        ssp.list <- list()
        for (k in ssp){ # Begin SSP loop
          # Array for country-level projections
          countrylevel <- array(dim=c(dim(growthProjections[[1]])[1], length(2010:yr), 6))
          dimnames(countrylevel) <- list(growthProjections[[1]][,1], 2010:yr, c('GDPcapNoCC', 'GDPcapCC', 'TempNoCC', 'TempCC', 'GrowthNoCC', 'GrowthCC'))
          # Array for global projections
          global <- array(dim=c(length(2010:yr), 15))
          dimnames(global) <- expand.grid(c('NoCC.', 'CC.'), c('All', 'Poor', 'Rich'), c('GDP.', 'Growth.')) %$%
            paste0(Var3, Var1, Var2) %>%
            c('GDPcap.NoCC', 'GDPcap.CC', ., 'dTemp') %>%
            list(2010:yr, .)
          # Subset to relevant SSP
          growthproj <- growthProjections[[k]]
          popproj <- popProjections[[k]]
          # Binary markers for countries' income level
          poor <- (growthproj$poor==T)*1
          rich <- (growthproj$poor==F)*1
          basegdpcap <- popproj$gdpcap
          temp <- popproj$meantemp
          wt <- popproj[,which(names(popproj)==2010)] # Baseline pop weights
          ## Year 0
          countrylevel[,,3] <- countrylevel[,1,4] <- temp # Uniform temperature for all No-CC scenarios
          countrylevel[,1,1:2] <- basegdpcap
          global[1,1:2] <- weighted.mean(countrylevel[,1,2], wt)
          global[1,3:4] <- sum(countrylevel[,1,2]*wt*1e6)
          global[1,5:6] <- sum(countrylevel[,1,2]*wt*1e6*poor)
          global[1,7:8] <- sum(countrylevel[,1,2]*wt*1e6*rich)
          global[1,9:14] <- countrylevel[,1,5:6] <- 0 # Zero growth in year 2010 relative to self
          global[1,15] <- 0.8 # Add baseline global warming of 0.8 degrees Celcius since pre-industrial era
          ## Projections
          seq <- 2010:yr
          for (t in 2:length(seq)){ # Begin year loop
            basegrowth <- growthproj[,which(names(growthproj)==seq[t])]
            wt <- popproj[,which(names(popproj)==seq[t])]
            # No climate change
            countrylevel[,t,1] <- countrylevel[,t-1,1]*(1+basegrowth) # Secular growth projections
            countrylevel[,t,5] <- basegrowth
            global[t,1] <- weighted.mean(countrylevel[,t,1], wt)
            global[t,3] <- sum(countrylevel[,t,1]*wt*1e6)
            global[t,5] <- sum(countrylevel[,t,1]*wt*1e6*poor)
            global[t,7] <- sum(countrylevel[,t,1]*wt*1e6*rich)
            global[t,9] <- weighted.mean(countrylevel[,t,5], wt*countrylevel[,t,1])
            global[t,11] <- weighted.mean(countrylevel[,t,5], poor*wt*countrylevel[,t,1])
            global[t,13] <- weighted.mean(countrylevel[,t,5], rich*wt*countrylevel[,t,1])
            # With climate change
            newtemp <- temp + (t-1)*ccd
            bg <- boot.spec$b1[j]*temp+boot.spec$b2[j]*temp^2
            dg <- boot.spec$b1[j]*newtemp+boot.spec$b2[j]*newtemp^2
            diff <- dg-bg
            countrylevel[,t,2] <- countrylevel[,t-1,2]*(1+basegrowth+diff)
            countrylevel[,t,4] <- newtemp
            countrylevel[,t,6] <- basegrowth+diff
            global[t,2] <- weighted.mean(countrylevel[,t,2], wt)
            global[t,4] <- sum(countrylevel[,t,2]*wt*1e6)
            global[t,6] <- sum(countrylevel[,t,2]*wt*1e6*poor)
            global[t,8] <- sum(countrylevel[,t,2]*wt*1e6*rich)
            global[t,10] <- weighted.mean(countrylevel[,t,6], wt*countrylevel[,t,1])
            global[t,12] <- weighted.mean(countrylevel[,t,6], poor*wt*countrylevel[,t,1])
            global[t,14] <- weighted.mean(countrylevel[,t,6], rich*wt*countrylevel[,t,1])
            global[t,15] <- global[t-1,15]+globe.temp[s,m]
          } # End year loop
          add <- melt(global, varnames = c('year', 'var')) %>%
            dcast(year ~ var) %>% mutate(pop = GDP.CC.All/GDPcap.CC,
                                         dY = GDP.CC.All/GDP.NoCC.All-1,
                                         dYpoor = GDP.CC.Poor/GDP.NoCC.Poor-1,
                                         dYrich = GDP.CC.Rich/GDP.NoCC.Rich-1,
                                         ssp = ssps[k])
          ssp.list[[k]] <- add; rm(global); rm(countrylevel)
        } # End SSP loop
        ssp.list %<>% do.call('rbind', .)
        ssp.list$mod <- sprintf('gcm%02d',m)
        mod.list[[m]] <- ssp.list; rm(ssp.list)
      } # End GCM loop
      mod.list %<>% do.call('rbind', .) %>% mutate(boot = sprintf('boot%0'%&%nchar(nrow(boot.spec))%&%'d', j-1))
      boot.list[[j]] <- mod.list
      if (j%%500==0){print('Spec: '%&%x%&%'/'%&%length(specs)%&%' -- RCPs: '%&%which(rcp==s)%&%'/'%&%length(rcp)%&% ' -- Bootstrap '%&%j%&%'/'%&%nrow(boot.spec)); rm(mod.list)}
    } # End bootstrap loop
    boot.list %<>% do.call('rbind', .) %>% mutate(rcp = rcps[s])
    globe.proj[[s]] <- boot.list; rm(boot.list)
  } # End RCP loop
  globe.proj %<>% do.call('rbind', .) %>% mutate(spec = specs[x])
  save(globe.proj, file = 'data/output/globe-proj-'%&%specs[x]%&%'.Rdata')
}

#### Part 2: Global economic pathways — median bootstrap for all RCPs, SSPs ####
# These simulations are used to produce Figures 4a and ED6
for (x in 1){ # we use just the baseline specification
  rcp <- 1:8 # we use all RCPs
  ssp <- 1:5 # we use all SSPs
  boot.spec <- filter(boots, spec == specs[x])
  globe.proj <- list()
  for (s in rcp){ # Begin RCP loop
    nmod <- gcms[ceiling(s/2)]
    yr <- if (s%%2!=0) 2049 else 2099
    boot.list <- list()
    for (j in 1){ # Begin bootstrap loop, only using full-sample response function
      mod.list <- list()
      for (m in 1:nmod){ # Begin GCM loop
        df <- data.frame(cty@data$ISO, rownames_to_column(data.frame(cty.temp[,s,m])))
        colnames(df) <- c('iso', 'country', 'dTemp')
        df %<>% merge(popProjections[[1]][,c(1,2,4)], ., by = 'iso')
        # Annualize projected temperature changes for each country
        ccd <- if(s%%2!=0) df$dTemp/(mean(c(2046, 2065))-mean(c(1986, 2005))) else{
          df$dTemp/(mean(c(2081, 2100))-mean(c(1986, 2005)))
        }
        # Read in bootstrapped estimates
        ssp.list <- list()
        for (k in ssp){ # Begin SSP loop
          countrylevel <- array(dim=c(dim(growthProjections[[1]])[1], length(2010:yr), 6))
          dimnames(countrylevel) <- list(growthProjections[[1]][,1], 2010:yr, c('GDPcapNoCC', 'GDPcapCC', 'TempNoCC', 'TempCC', 'GrowthNoCC', 'GrowthCC'))
          global <- array(dim=c(length(2010:yr), 15))
          dimnames(global) <- expand.grid(c('NoCC.', 'CC.'), c('All', 'Poor', 'Rich'), c('GDP.', 'Growth.')) %$%
            paste0(Var3, Var1, Var2) %>%
            c('GDPcap.NoCC', 'GDPcap.CC', ., 'dTemp') %>%
            list(2010:yr, .)
          growthproj <- growthProjections[[k]]
          popproj <- popProjections[[k]]
          poor <- (growthproj$poor==T)*1
          rich <- (growthproj$poor==F)*1
          basegdpcap <- popproj$gdpcap
          temp <- popproj$meantemp
          wt <- popproj[,which(names(popproj)==2010)]
          ## Year 0
          countrylevel[,,3] <- countrylevel[,1,4] <- temp
          countrylevel[,1,1:2] <- basegdpcap
          global[1,1:2] <- weighted.mean(countrylevel[,1,2], wt)
          global[1,3:4] <- sum(countrylevel[,1,2]*wt*1e6)
          global[1,5:6] <- sum(countrylevel[,1,2]*wt*1e6*poor)
          global[1,7:8] <- sum(countrylevel[,1,2]*wt*1e6*rich)
          global[1,9:14] <- countrylevel[,1,5:6] <- 0
          global[1,15] <- 0.8
          ## Projections
          seq <- 2010:yr
          for (t in 2:length(seq)){ # Begin year loop
            basegrowth <- growthproj[,which(names(growthproj)==seq[t])]
            wt <- popproj[,which(names(popproj)==seq[t])]
            # No climate change
            countrylevel[,t,1] <- countrylevel[,t-1,1]*(1+basegrowth)
            countrylevel[,t,5] <- basegrowth
            global[t,1] <- weighted.mean(countrylevel[,t,1], wt)
            global[t,3] <- sum(countrylevel[,t,1]*wt*1e6)
            global[t,5] <- sum(countrylevel[,t,1]*wt*1e6*poor)
            global[t,7] <- sum(countrylevel[,t,1]*wt*1e6*rich)
            global[t,9] <- weighted.mean(countrylevel[,t,5], wt*countrylevel[,t,1])
            global[t,11] <- weighted.mean(countrylevel[,t,5], poor*wt*countrylevel[,t,1])
            global[t,13] <- weighted.mean(countrylevel[,t,5], rich*wt*countrylevel[,t,1])
            # With climate change
            newtemp <- temp + (t-1)*ccd
            bg <- boot.spec$b1[j]*temp+boot.spec$b2[j]*temp^2
            dg <- boot.spec$b1[j]*newtemp+boot.spec$b2[j]*newtemp^2
            diff <- dg-bg
            countrylevel[,t,2] <- countrylevel[,t-1,2]*(1+basegrowth+diff)
            countrylevel[,t,4] <- newtemp
            countrylevel[,t,6] <- basegrowth+diff
            global[t,2] <- weighted.mean(countrylevel[,t,2], wt)
            global[t,4] <- sum(countrylevel[,t,2]*wt*1e6)
            global[t,6] <- sum(countrylevel[,t,2]*wt*1e6*poor)
            global[t,8] <- sum(countrylevel[,t,2]*wt*1e6*rich)
            global[t,10] <- weighted.mean(countrylevel[,t,6], wt*countrylevel[,t,1])
            global[t,12] <- weighted.mean(countrylevel[,t,6], poor*wt*countrylevel[,t,1])
            global[t,14] <- weighted.mean(countrylevel[,t,6], rich*wt*countrylevel[,t,1])
            global[t,15] <- global[t-1,15]+globe.temp[s,m]
          } # End year loop
          add <- melt(global, varnames = c('year', 'var')) %>%
            dcast(year ~ var) %>% mutate(pop = GDP.CC.All/GDPcap.CC,
                                         dY = GDP.CC.All/GDP.NoCC.All-1,
                                         dYpoor = GDP.CC.Poor/GDP.NoCC.Poor-1,
                                         dYrich = GDP.CC.Rich/GDP.NoCC.Rich-1,
                                         ssp = ssps[k])
          ssp.list[[k]] <- add; rm(global); rm(countrylevel)
        } # End SSP loop
        ssp.list %<>% do.call('rbind', .) %>% mutate(mod = sprintf('gcm%02d',m))
        mod.list[[m]] <- ssp.list; rm(ssp.list)
      } # End GCM loop
      mod.list %<>% do.call('rbind', .) %>% mutate(boot = sprintf('boot%0'%&%nchar(nrow(boot.spec))%&%'d', j-1))
      boot.list[[j]] <- mod.list
      if (j%%500==0){print('Spec: '%&%x%&%'/'%&%length(specs)%&%' -- RCPs: '%&%which(rcp==s)%&%'/'%&%length(rcp)%&% ' -- Bootstrap '%&%j%&%'/'%&%nrow(boot.spec)); rm(mod.list)}
    } # End bootstrap loop
    boot.list %<>% do.call('rbind', .) %>% mutate(rcp = rcps[s])
    globe.proj[[s]] <- boot.list; rm(boot.list)
  } # End RCP loop
  globe.proj %<>% do.call('rbind', .) %>% mutate(spec = specs[x])
  save(globe.proj, file = 'data/output/globe-proj-median.Rdata')
}

#### Part 3: Global economic pathways with non-linear warming ####
# These simulations are used in Figure ED5
beg <- Sys.time()
for (x in 1){ # Only use the baseline specification
  # Restricting to scenarios that appear in the main text. Can easily be adapted to include other scenarios
  rcp <- 2 # Only use RCP2.6
  ssp <- 1 # Only use SSP1
  boot.spec <- filter(boots, spec == specs[x])
  globe.proj <- list()
  for (s in rcp){ # Begin RCP loop
    nmod <- gcms[ceiling(s/2)] # Number of GCMs in the RCP indexed s
    yr <- if (s%%2!=0) 2049 else 2099 # The final year we will project for RCP indexed s
    boot.list <- list()
    for (j in 1:nrow(boot.spec)){ # Begin bootstrap loop
      mod.list <- list()
      for (m in 1:nmod){ # Begin GCM loop
        df <- data.frame(cty@data$ISO, rownames_to_column(data.frame(cty.temp[,s,m])))
        colnames(df) <- c('iso', 'country', 'dTemp')
        df %<>% merge(popProjections[[1]][,c(1,2,4)], ., by = 'iso')
        # Annualize projected temperature changes for each country
        ccd <- if(s%%2!=0) df$dTemp/(mean(c(2046, 2065))-mean(c(1986, 2005))) else{
          df$dTemp/(mean(c(2081, 2100))-mean(c(1986, 2005)))
        }
        ccd <- ccd/((2049-2010)/(yr-2010)) # Have change in warming annualized s.t. all warming occurs linearly to 2049
        # Read in bootstrapped estimates
        ssp.list <- list()
        for (k in ssp){ # Begin SSP loop
          countrylevel <- array(dim=c(dim(growthProjections[[1]])[1], length(2010:yr), 6))
          dimnames(countrylevel) <- list(growthProjections[[1]][,1], 2010:yr, c('GDPcapNoCC', 'GDPcapCC', 'TempNoCC', 'TempCC', 'GrowthNoCC', 'GrowthCC'))
          global <- array(dim=c(length(2010:yr), 15))
          dimnames(global) <- expand.grid(c('NoCC.', 'CC.'), c('All', 'Poor', 'Rich'), c('GDP.', 'Growth.')) %$%
            paste0(Var3, Var1, Var2) %>%
            c('GDPcap.NoCC', 'GDPcap.CC', ., 'dTemp') %>%
            list(2010:yr, .)
          growthproj <- growthProjections[[k]]
          popproj <- popProjections[[k]]
          poor <- (growthproj$poor==T)*1
          rich <- (growthproj$poor==F)*1
          basegdpcap <- popproj$gdpcap
          temp <- popproj$meantemp
          wt <- popproj[,which(names(popproj)==2010)] # Baseline pop weights
          ## Year 0
          countrylevel[,,3] <- countrylevel[,1,4] <- temp # Uniform temperature for all No-CC scenarios
          countrylevel[,1,1:2] <- basegdpcap
          global[1,1:2] <- weighted.mean(countrylevel[,1,2], wt)
          global[1,3:4] <- sum(countrylevel[,1,2]*wt*1e6)
          global[1,5:6] <- sum(countrylevel[,1,2]*wt*1e6*poor)
          global[1,7:8] <- sum(countrylevel[,1,2]*wt*1e6*rich)
          global[1,9:14] <- countrylevel[,1,5:6] <- 0 # Zero growth in year 2010 relative to self
          global[1,15] <- 0.8 # Add baseline global warming of 0.8 degrees Celcius since pre-industrial era
          ## Projections
          seq <- 2010:yr
          for (t in 2:length(seq)){ # Begin year loop
            basegrowth <- growthproj[,which(names(growthproj)==seq[t])]
            wt <- popproj[,which(names(popproj)==seq[t])]
            # No climate change
            countrylevel[,t,1] <- countrylevel[,t-1,1]*(1+basegrowth)
            countrylevel[,t,5] <- basegrowth
            global[t,1] <- weighted.mean(countrylevel[,t,1], wt)
            global[t,3] <- sum(countrylevel[,t,1]*wt*1e6)
            global[t,5] <- sum(countrylevel[,t,1]*wt*1e6*poor)
            global[t,7] <- sum(countrylevel[,t,1]*wt*1e6*rich)
            global[t,9] <- weighted.mean(countrylevel[,t,5], wt*countrylevel[,t,1])
            global[t,11] <- weighted.mean(countrylevel[,t,5], poor*wt*countrylevel[,t,1])
            global[t,13] <- weighted.mean(countrylevel[,t,5], rich*wt*countrylevel[,t,1])
            # With climate change
            newtemp <- temp + min(c(2049-2010, t-1))*ccd # No warming after mid-century
            bg <- boot.spec$b1[j]*temp+boot.spec$b2[j]*temp^2
            dg <- boot.spec$b1[j]*newtemp+boot.spec$b2[j]*newtemp^2
            diff <- dg-bg
            countrylevel[,t,2] <- countrylevel[,t-1,2]*(1+basegrowth+diff)
            countrylevel[,t,4] <- newtemp
            countrylevel[,t,6] <- basegrowth+diff
            global[t,2] <- weighted.mean(countrylevel[,t,2], wt)
            global[t,4] <- sum(countrylevel[,t,2]*wt*1e6)
            global[t,6] <- sum(countrylevel[,t,2]*wt*1e6*poor)
            global[t,8] <- sum(countrylevel[,t,2]*wt*1e6*rich)
            global[t,10] <- weighted.mean(countrylevel[,t,6], wt*countrylevel[,t,1])
            global[t,12] <- weighted.mean(countrylevel[,t,6], poor*wt*countrylevel[,t,1])
            global[t,14] <- weighted.mean(countrylevel[,t,6], rich*wt*countrylevel[,t,1])
            global[t,15] <- global[t-1,15]+globe.temp[s,m]
          } # End year loop
          add <- melt(global, varnames = c('year', 'var')) %>%
            dcast(year ~ var) %>% mutate(pop = GDP.CC.All/GDPcap.CC,
                                         dY = GDP.CC.All/GDP.NoCC.All-1,
                                         dYpoor = GDP.CC.Poor/GDP.NoCC.Poor-1,
                                         dYrich = GDP.CC.Rich/GDP.NoCC.Rich-1,
                                         ssp = ssps[k])
          ssp.list[[k]] <- add; rm(global); rm(countrylevel)
        } # End SSP loop
        ssp.list %<>% do.call('rbind', .) %>% mutate(mod = sprintf('gcm%02d',m))
        mod.list[[m]] <- ssp.list; rm(ssp.list)
      } # End GCM loop
      mod.list %<>% do.call('rbind', .) %>% mutate(boot = sprintf('boot%0'%&%nchar(nrow(boot.spec))%&%'d', j-1))
      boot.list[[j]] <- mod.list
      if (j%%500==0){print('Bootstrap '%&%j%&%'/'%&%nrow(boot.spec)); rm(mod.list)}
    } # End bootstrap loop
    boot.list %<>% do.call('rbind', .) %>% mutate(rcp = rcps[s])
    globe.proj[[s]] <- boot.list; rm(boot.list)
  } # End RCP loop
  globe.proj %<>% do.call('rbind', .) %>% mutate(spec = specs[x])
  save(globe.proj, file = 'data/output/globe-proj-nonlinear.Rdata')
}

#### Part 4: Country-level economic pathways ####
# These simulations are used in Figures 3 and ED2
# Because of the large size of the resulting dataset, we restrict country-level projections to
# observations at the end of the respective projection periods in lieu of creating separate time series
# This allows us to compute country-level impacts on GDP/cap but not cumulative impacts

specs <- unique(boots$spec)

beg <- Sys.time()
for (x in 1){ # Only use our main specification with 0 lags and bootstrapping on countries
  rcp <- c(2,3) # Only use RCP4.5 for mid-century and RCP2.6 for end of century
  ssp <- 1 # Only use SSP1
  boot.spec <- filter(boots, spec == specs[x])
  rcp.list <- list()
  for (s in rcp){ # Begin RCP loop
    nmod <- gcms[ceiling(s/2)] # Number of GCMs in the RCP
    yr <- if (s%%2!=0) 2049 else 2099
    boot.list <- list()
    for (j in 1:nrow(boot.spec)){ # Begin bootstrap loop; no bootstrapping, applying only whole-sample regression
      mod.list <- list()
      for (m in 1:nmod){ # Begin GCM loop
        df <- data.frame(cty@data$ISO, rownames_to_column(data.frame(cty.temp[,s,m])))
        colnames(df) <- c('iso', 'country', 'dTemp')
        df <- merge(popProjections[[1]][,1:3], df, by = 'iso')
        # Annualize projected temperature changes for each country
        ccd <- if(s%%2!=0) df$dTemp/(mean(c(2046, 2065))-mean(c(1986, 2005))) else{
          df$dTemp/(mean(c(2081, 2100))-mean(c(1986, 2005)))
        }
        # Read in bootstrapped estimates
        ssp.list <- list()
        for (k in ssp){ # Begin SSP loop
          countrylevel <- array(dim=c(dim(growthProjections[[1]])[1], length(2010:yr), 6))
          dimnames(countrylevel) <- list(growthProjections[[1]][,1], 2010:yr, c('GDPcapNoCC', 'GDPcapCC', 'TempNoCC', 'TempCC', 'GrowthNoCC', 'GrowthCC'))
          growthproj <- growthProjections[[k]]
          popproj <- popProjections[[k]]
          basegdpcap <- popproj$gdpcap
          temp <- popproj$meantemp
          wt <- popproj[,which(names(popproj)==2010)] # Baseline pop weights
          ## Year 0
          countrylevel[,,3] <- countrylevel[,1,4] <- temp # Uniform temperature for all No-CC scenarios
          countrylevel[,1,1:2] <- basegdpcap
          ## Projections
          seq <- 2010:yr
          for (t in 2:length(seq)){ # Begin year loop
            basegrowth <- growthproj[,which(names(growthproj)==seq[t])]
            wt <- popproj[,which(names(popproj)==seq[t])]
            # No climate change
            countrylevel[,t,1] <- countrylevel[,t-1,1]*(1+basegrowth)
            countrylevel[,t,5] <- basegrowth
            # With climate change
            newtemp <- temp + (t-1)*ccd
            bg <- boot.spec$b1[j]*temp+boot.spec$b2[j]*temp*temp
            dg <- boot.spec$b1[j]*newtemp+boot.spec$b2[j]*newtemp*newtemp
            diff <- dg-bg
            countrylevel[,t,2] <- countrylevel[,t-1,2]*(1+basegrowth+diff)
            countrylevel[,t,4] <- newtemp
            countrylevel[,t,6] <- basegrowth+diff
          } # End year loop
          add <- melt(countrylevel, varnames = c('iso', 'year', 'var')) %>%
            dcast(iso + year ~ var) %>% mutate(dY = GDPcapCC/GDPcapNoCC-1,
                                               dTemp = TempCC-TempNoCC,
                                               ssp = ssps[k])
          ssp.list[[k]] <- filter(add, year == yr)
        } # End SSP loop
        ssp.list <- do.call('rbind', ssp.list) %>% mutate(mod = sprintf('gcm%02d',m))
        mod.list[[m]] <- ssp.list; rm(ssp.list)
      } # End GCM loop
      mod.list <- do.call('rbind', mod.list) %>% mutate(boot = sprintf('boot%0'%&%nchar(nrow(boot.spec))%&%'d', j-1))
      boot.list[[j]] <- mod.list; rm(mod.list)
      if (j%%500==0){print('Spec: '%&%x%&%'/'%&%length(specs)%&%' -- RCPs: '%&%which(rcp==s)%&%'/'%&%length(rcp)%&% ' -- Bootstrap '%&%j%&%'/'%&%nrow(boot.spec))}
    } # End bootstrap loop
    boot.list <- do.call('rbind', boot.list) %>% mutate(rcp = rcps[s])
    rcp.list[[s]] <- boot.list; rm(boot.list)
  } # End RCP loop
  country.proj <- do.call('rbind', rcp.list); rm(rcp.list)
  save(country.proj, file = 'data/output/country-proj.Rdata'); rm(country.proj)
}