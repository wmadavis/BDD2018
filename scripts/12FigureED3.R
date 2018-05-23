## Script to recreate Figure ED3 ##
setwd('BDD2018')
rm(list=ls())
library(magrittr)
library(dplyr)

load('data/output/cum-impact.Rdata')
cum.impact %<>% filter(ssp == 'ssp1' & spec == 'country-lag0' & rcp == 'rcp26b') %>%
  mutate_at('discount', as.character)

# Re-format to treat differentiated response as separate discount method
dta <- cum.impact %>%
  filter(sub!='All' & discount %in% c('nordhaus', 'stern', 'weitzman')) %>%
  group_by(boot, ssp, rcp, discount) %>% summarize(dam.unc = NA, dam = sum(dam)) %>%
  mutate(discount = paste0(discount, '_richpoor')) %>%
  bind_rows(select(filter(cum.impact, sub == 'All'), -sub))
# Re-format to treat uncertainty estimates as separate discount scheme
dta %<>% as.data.frame() %>%
  filter(is.na(dam.unc)==F) %>%
  mutate(discount = paste0(discount, '_unc'), dam = dam.unc) %>%
  bind_rows(dta) %>% select(-dam.unc)
order <- group_by(dta, ssp, rcp, discount, spec) %>% summarize(dam = median(dam)) %>% arrange(dam) %>% data.frame() %>% select(discount)
dta %<>% left_join(order, .) # get discounts in order of median damages

## Plotting ##
pdf(file = 'figures/FigED3.pdf', width = 7, height = 8, useDingbats = F)
cll <- apply(sapply('black', col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=0.1))
plot(1, type = 'n', ylim = c(1, nrow(order)+1), xlim = c(-200, 1500),
     axes = F, ylab='', xlab = 'Change in cumulative gGDP by end of century (trillions USD)')
segments(0, 0, 0, nrow(order)+1, lwd = 2, col='blue')
axis(1, seq(-200, 1000, 200))
end <- list()
for (d in 1:nrow(order)){
  toplot <- -filter(dta, discount == order$discount[d])$dam/1e12
  cut <- quantile(toplot,c(0.01,0.99))
  toplot <- toplot[toplot > cut[1] & toplot < cut[2]]
  segments(toplot, d, toplot, d+0.7, col = cll, lwd = 1)
  mm <- median(toplot)
  segments(mm, d-0.02, mm, d+0.72, col='red', lwd = 2)
  end[[d]] <- max(toplot)
}
att <- unlist(lapply(end, function(x) quantile(x, probs=0.01)))
text(att[1:nrow(order)]+50, 1:nrow(order)+0.35, labels = order$discount, pos = 4, cex = 1)
dev.off()