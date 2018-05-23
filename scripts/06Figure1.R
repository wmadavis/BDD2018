## Script to recreate Figure 1 ##
setwd('BDD2018')
rm(list=ls())
library(magrittr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
'%&%' <- function(x,y) paste0(x,y)
options(expressions = 1e5) # Allows more layers in our ggplot outputs

# Data load
load('data/output/baseline.Rdata')
load('data/output/boots.Rdata')
boots %<>% filter(spec == 'country-lag0') %>% arrange(optimal)
load('data/output/globe-proj-country-lag0.Rdata')
globe.proj %<>% filter(ssp == 'ssp1') %>% mutate(dY = dY*100) %>%
  mutate_at('rcp', as.factor)
series <- filter(globe.proj, rcp == 'rcp26b')

## Color schemes
# Coloring three highlighted bootstraps: 10th and 90th percentiles and point estimate
samp <- floor(quantile(1:max(boots$boot), c(0.1, 0.5, 0.9))) # 10th, 50th, and 90th percentile optimizing bootstrap
cols <- data.frame(ind = samp,
                   boot = sprintf('boot%0'%&%nchar(max(boots$boot))%&%'d', boots$boot[samp]),
                   color = c('#1b9e77', '#7c4b2a', '#7570b3'))
# Coloring the other bootstraps
boots$color[order(boots$optimal)] <- colorRampPalette(c('#f03b20', '#feb24c', '#ffeda0'))(nrow(boots))
boots$color %<>% factor
col.pal <- levels(boots$color)
names(col.pal) <- col.pal
colScale <- scale_color_manual(name = 'color', values = col.pal)
fillScale <- scale_fill_manual(name = 'color', values = col.pal)
# RCP color scheme
rcp.cols <- data.frame(rcp = levels(globe.proj$rcp), color = c('#e21e25','#377eb8', '#984ea3'))

#### Panel a: Response functions and population distribution ####

## Response functions
fig1a <- ggplot() +
  theme_minimal() +
  xlab('Temperature') +
  ylab('Temperature effect on growth rate') +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(-0.15, 0.002))

# Plot bootstraps individually
for (j in c(c(1:nrow(boots))[-samp], samp)){
  alpha <- if(j%in%samp) 1 else 0.08
  size <- if(j%in%samp) 1.2 else 0.8
  color <- if(j%in%samp){
    cols$color[cols$ind==j]
  } else as.character(boots$color[j])
  lty <- if(j%in%samp) 2 else 1
  fun = function(x) (boots$b2[j]*x^2+boots$b1[j]*x-boots$normalize[j])
  df <- data.frame(x = seq(-4, 30, 0.1))
  df$y = fun(df$x)
  fig1a <- fig1a +
    geom_line(data = df, aes(x = x, y = y), color = color, alpha = alpha, size = size, lty = lty)
}

# Add rug plot at bottom
fig1a <- fig1a +
  geom_rug(data = boots, aes(optimal, color = color), size = 1) +
  theme(legend.position='none') + colScale
for (i in 1:nrow(cols)){
  fig1a <- fig1a +
    geom_rug(data = boots[cols$ind[i],], aes(optimal), color = cols$color[i], size = 1.5)
}

## GDP distribution
fig1a.gdp <- ggplot(baseline, aes(x = meantemp, weight = gdp)) +
  geom_density(fill = 'black', adjust = 0.1) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab('') +
  xlab('Temperature') +
  scale_x_continuous(limits = c(-4, 30))

#### Panels b-g: End-of-century projected impacts of global warming for three highlighted bootstraps ####

# b-d: Time series plots
for (i in 1:nrow(cols)){
  g <- ggplot() +
    theme_minimal() +
    scale_y_continuous(limits = c(6, 45)) +
    xlab('Year') +
    ylab('Global GDP/cap (USD)') +
    theme(legend.position = 'none')
  ts <- filter(series, boot == cols$boot[i])
  color = cols$color[i]
  for (j in 1:length(unique(ts$mod))){
    ts.sub <- filter(ts, mod == unique(ts$mod)[j])
    g <- g +
      # Plot models corresponding to different climate models
      geom_line(data = ts.sub, aes(year, GDPcap.CC/1000), color = color, alpha = 0.8) +
      geom_point(data = filter(ts.sub, year == 2099), aes(year, GDPcap.CC/1000), color = color, shape = 1, alpha = 0.8, size = 1.5) +
      # Plot constant-temperature time series
      geom_line(data = filter(series, mod == 'mod1' & boot == 'boot0'), aes(year, GDPcap.NoCC/1000), color = 'black') +
      geom_point(data = filter(series, mod == 'mod1' & boot == 'boot0' & year == 2099), aes(year, GDPcap.NoCC/1000), color = 'black', size = 1.5)
  }
  assign('ts.'%&%i, g)
}

# e-g: Damages vs. global warming
for (i in 1:nrow(cols)){
  g <- ggplot() +
    theme_minimal() +
    geom_vline(xintercept = c(1.5, 2.0), color = 'red', lty= 2) +
    scale_y_continuous(limits = c(-35, 12)) +
    ylab('Change in gGDP by 2099') +
    xlab('Global warming (C)')
  sub <- filter(series, year == 2099 & boot==cols$boot[i])
  color = cols$color[i]
  g <- g +
    geom_smooth(data = sub, aes(dTemp, dY), color = cols$color[i], method = 'lm', fill = 'lightgoldenrod3') +
    geom_point(data = sub, aes(dTemp, dY), color = cols$color[i], shape = 1, size = 1.5)
  assign('linear.'%&%i, g)
}

#### Panels h-i: Damages vs. global warming across all bootstraps by mid-century and end-of-century ####

df.2049a <- filter(globe.proj, rcp == 'rcp45a' & year == 2049)
df.2049b <- filter(globe.proj, rcp == 'rcp60a' & year == 2049)
df.2099 <- filter(globe.proj, rcp == 'rcp26b' & year == 2099)

# Pathway function
fig1hi <- function(rcp.df){
  p <- ggplot() +
    theme_minimal() +
    ylab('% Change in gGDP') +
    xlab('Change in Global Temperature (Celsius)') +
    theme(legend.position = 'none') +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 1.5, color = 'red', lty = 2) +
    geom_vline(xintercept = 2.0, color = 'red', lty = 2) +
    scale_x_continuous(limits = c(0.7183414, 2.7268093)) +
    colScale + fillScale
  p <- p +
    geom_rug(data = unique(rcp.df[,c('rcp', 'dTemp')]), aes(x = dTemp),
             color = rcp.cols$color[rcp.cols$rcp==unique(rcp.df$rcp)])
  k <- 1
  for (j in c(1:nrow(boots)[-samp], samp)){
    sub <- filter(rcp.df, boot == sprintf('boot%0'%&%nchar(max(boots$boot))%&%'d', boots$boot[j]))
    alpha <- if(j%in%samp) 1 else 0.2
    size <- if(j%in%samp) 1.2 else 0.5
    color <- if(j%in%samp) cols$color[cols$boot==sprintf('boot%0'%&%nchar(max(boots$boot))%&%'d', boots$boot[j])] else as.character(boots$color[j])
    p <- p +
      geom_line(data = sub, aes(x = dTemp, y = dY),
                stat = 'smooth', method = 'lm', alpha = alpha, color = color, size = size)
    if(k%%200==0){print(k%&%'/'%&%1001)}
    k <- k + 1
  }
  return(p)
}

pdf(file = 'figures/Fig1.pdf', width = 13/0.8, height = 6/0.8)
layout <- rbind(c(1,1,1,1,1,1,2,2,5,5,8,8,8),
                c(1,1,1,1,1,1,2,2,5,5,8,8,8),
                c(1,1,1,1,1,1,3,3,6,6,8,8,8),
                c(1,1,1,1,1,1,3,3,6,6,9,9,9),
                c(1,1,1,1,1,1,4,4,7,7,9,9,9),
                c(1,1,1,1,1,1,4,4,7,7,9,9,9)
)
grid.arrange(fig1a,
             ts.1, ts.2, ts.3,
             linear.1, linear.2, linear.3,
             fig1hi(df.2049a), fig1hi(df.2099),
             layout_matrix = layout)
dev.off()

pdf(file = 'figures/Fig1dist.pdf', width = 8, height = 2)
fig1a.gdp
dev.off()