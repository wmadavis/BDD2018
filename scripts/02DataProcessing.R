## Script to run projections of economic impacts of temperature change ##
setwd('BDD2018')
rm(list=ls())
library(raster)
library(rgdal)
library(ncdf4)
library(dplyr)
library(magrittr)
library(reshape2)
library(zoo)
extract <- raster::extract
select <- dplyr::select
'%&%' <- function(x,y) paste0(x,y)

# Pre-processed data
load('data/output/baseline.Rdata')

## Identifying our projection inputs
# RCPs; 'a' and 'b' refer to mid-century and end of century respectively.
rcps <- rep(c('rcp26', 'rcp45', 'rcp60', 'rcp85'), each = 2)%&%c('a', 'b')
# The range of years we're projecting for
yrs <- 2010:2099 # We will subset to 2010:2049 for mid-century projections

#### PART 1: Global climate projections ####

# Set up gridded raster for extraction
rast <- raster(nrows=72, ncols = 144, xmn = -180, xmx = 180, ymn = -90, ymx = 90)

# Download and read GADM global shapefile
dir <- 'data/output/gadm28_levels.shp'
dir.create(dir)
download.file('http://biogeo.ucdavis.edu/data/gadm2.8/gadm28_levels.shp.zip', dir%&%'/gadm28_levels.shp.zip')
unzip(dir%&%'/gadm28_levels.shp.zip', exdir = dir)
del <- list.files(dir)[substr(list.files(dir),8,11)!='adm0'] # Keep just country-level data
unlink(dir%&%'/'%&%del); rm(del)
cty <- readOGR(dir)
save(cty, file = 'data/output/cty.Rdata')

rast[] <- 1:ncell(rast)
rastc <- extract(rast, cty, small=T, progress='text') # List of cell numbers corresponding to each polygon

# Load the suite of climate projections
cmip <- list.files('data/input/CMIP5_annual_temperature')
tmp <- list()
for (s in 1:length(cmip)){
  nc <- nc_open('data/input/CMIP5_annual_temperature/'%&%cmip[s])
  tmp[[s]] <- aperm(ncvar_get(nc, nc$var[[4]])[c(73:144,1:72),72:1,],c(2,1,3)) # Extract layer with projected temperature changes
}

# Compute global warming for each RCP s and GCM m weighting by cell cize
globe.temp <- array(dim = c(length(tmp), 42))
dimnames(globe.temp) <- list(rcps, sprintf('gcm%02d',1:42))
cellweight <- raster(nrows=72, ncols = 144)
crs(cellweight) <- crs(rast)
cellweight <- as.vector(as.matrix(area(cellweight))) # Weight each cell to account for longitude
for (s in 1:length(tmp)){
  per <- if(s%%2!=0) mean(c(2046, 2065))-mean(c(1986, 2005)) else mean(c(2081, 2100))-mean(c(1986, 2005))
  for (m in 1:dim(tmp[[s]])[3]){
    rcpgcm <- as.vector(tmp[[s]][,,m])
    globe.temp[s,m] <- weighted.mean(rcpgcm, cellweight)/per
  }
}
save(globe.temp, file = 'data/output/globetemp.Rdata')

#### PART 2: Country-level climate projections ####

# Load population-weighted grid of the world (derived from CIESIN-GRUMP data)
load('data/input/popweight.Rdata')

# Function to create population-weighted country-level averages
pweight <- function(x){
  if (is.null(zz[[x]])) NA else
    if (dim(zz[[x]])[1]==1) zz[[x]] else
      apply(zz[[x]],2,weighted.mean,popweight[[x]],na.rm=T)}

# Create array of country-level temperature projections for all GCMs
countries <- as.character(cty@data$NAME_ENGLI)
cty.temp <- array(dim = c(length(countries), 8, 42))
dimnames(cty.temp) <- list(countries, rcps, sprintf('gcm%02d',1:42))

options(warn = -1) # Suppressing warning (comes from differing number of GCMs per RCP)
for (s in 1:length(tmp)){ # For every projection file
  layer <- raster(nrows=72, ncols = 144)
  for (m in 1:dim(tmp[[s]])[3]){ # For every gcm contained therein
    rcpgcm <- tmp[[s]][,,m]
    layer <- addLayer(layer, raster(rcpgcm))
  }
  extent(layer) <- c(-180, 180, -90, 90)
  crs(layer) <- crs(cty)
  zz <- sapply(1:length(rastc), function(x) layer[rastc[[x]]])
  cty.temp[,s,1:dim(tmp[[s]])[3]] <- t(sapply(1:length(zz), pweight, simplify = T))
  print(s)
  layer <- rcpmod <- zz <- NULL
}
options(warn = 0) # Turn off warning suppressor
save(cty.temp, file = 'data/output/ctytemp.Rdata')

#### Part 3: READING GROWTH AND POPULATION PROJECTIONS ####

pop <- read.csv('data/input/populationData/WPP2012_DB02_POPULATIONS_ANNUAL.csv') %>%
  filter(Time %in% yrs & Variant == 'Medium' & LocID < 900) %>%
  select(1,5,9) %>%
  reshape(v.names='PopTotal', timevar = 'Time', idvar = 'LocID', direction = 'wide')

cod <- read.csv('data/input/populationData/WPP2012_F01_LOCATIONS.csv') %>%
  select(iso = ISO3_Code, LocID) %>%
  mutate_at('iso', as.character) %>%
  filter(iso != 'GRL' & LocID < 900)

popprojb <- merge(pop, cod, by = 'LocID') %>%
  merge(baseline, ., by = 'iso') %>% # 166 countries for which we have both baseline data and pop projections
  select(-LocID)
names(popprojb)[8:ncol(popprojb)] <- yrs
popprojb[8:ncol(popprojb)] <- popprojb[8:ncol(popprojb)]/1000 # population in millions to match SSP data format

popProjections <- NULL  #initialize list that we will fill with population projections for each scenario
growthProjections <- NULL   #same for growth

# Function to interpolate between years in the SSP data
ipolate <- function(df){
  id <- unique(select(df, 2:3)) %>% filter(Scenario != '')
  df.out <- select(df, -c(X2000, X2005, Variable, Unit)) %>%
    filter(Scenario != '') %>%
    melt %>% mutate_at('variable', funs(substr(.,2,5))) %>%
    mutate_at('variable', as.numeric) %>%
    right_join(merge(id, data.frame(variable = 2010:2100))) %>%
    arrange(Scenario, Region, variable) %>%
    group_by(Scenario, Region) %>%
    mutate(value = na.locf(na.approx(value, na.rm = F))) %>%
    dcast(Scenario + Region ~ variable) %>% as.data.frame()
  return(df.out)
}

# Add in SSP projections
pop <- read.csv('data/input/SSP/SSP_PopulationProjections.csv') %>% head(-1) %>%
  mutate_at(c('Scenario', 'Model'), as.character) %>%
  select(-Notes) %>% mutate(Scenario=replace(Scenario, which(Scenario=='SSP4d_v9_130115'), 'SSP4_v9_130115')) # renaming one of the scenarios for uniformity
growth <- read.csv('data/input/SSP/SSP_GrowthProjections.csv') %>% head(-1) %>%
  mutate_at('Model', as.character) %>% filter(Model == 'OECD Env-Growth')
pop %<>% ipolate
growth %<>% ipolate
growth[,names(growth) %in% 2010:2100] <- growth[,names(growth) %in% 2010:2100]/100

# First, we merge countries in the historical database with the growth and pop projections from SSP, restricted to the scenario we want
# We are using growth projections from OECD, which are the only ones with data for every country; population projections are from IIASA
popSSP <- merge(baseline, pop, by.x = 'iso', by.y = 'Region')  # merge our data with SSP population
growthSSP <- merge(baseline, growth, by.x = 'iso', by.y = 'Region')
for (k in 1:5) {
  growthProjections[[k]] <- filter(growthSSP, Scenario == 'SSP'%&%k%&%'_v9_130325')
  popProjections[[k]] <- filter(popSSP, Scenario == 'SSP'%&%k%&%'_v9_130115')
}

dir.create('data/output/projectionOutput')
save(popProjections, file = 'data/output/projectionOutput/popProjections.Rdata')
save(growthProjections, file = 'data/output/projectionOutput/growthProjections.Rdata')