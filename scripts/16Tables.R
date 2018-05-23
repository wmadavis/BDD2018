## Script to recreate Tables ED1-3 ##
setwd('BDD2018')
rm(list=ls())
library(dplyr)
library(magrittr)
library(tibble)
library(reshape2)
library(xtable)
'%&%' <- function(x,y) paste0(x, y)

## Data load ##
load('data/output/cum-impact.Rdata') # Cumulative GDP impacts
# Re-format to treat differentiated response as separate discount method
cum.disc.df <- cum.impact %>%
  filter(sub != 'All' & discount %in% c('nordhaus','stern','weitzman')) %>%
  group_by(spec, boot, ssp, rcp, discount) %>%
  summarize(dam.unc = sum(dam.unc), dam = sum(dam)) %>%
  mutate(discount = paste0(discount, '_richpoor')) %>%
  bind_rows(select(filter(cum.impact, sub == 'All'), -sub))
# Re-format to treat uncertainty estimates as separate discount scheme
cum.disc.df %<>% as.data.frame() %>%
  filter(is.na(dam.unc)==F & substr(discount, nchar(discount)-7, nchar(discount))!='richpoor') %>%
  mutate(discount = paste0(discount, '_unc'),
         dam = dam.unc) %>%
  bind_rows(cum.disc.df) %>% select(-dam.unc) %>% filter(ssp == 'ssp1' & spec == 'country-lag0')
cum.uni.df <- filter(cum.disc.df, substr(discount, 1, 3) == 'uni')

load('data/output/cap-impact.Rdata') # GDP impacts
cap.df <- filter(cap.impact, spec == 'country-lag0' & ssp == 'ssp1') %>% mutate(GDPCapDamage = GDPCapDamage * 100)

#### PART 1: Table ED1 ####
cuts <- c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
tab.ed1 <- sapply(unique(cum.disc.df$discount),
                  function(x){
                    filter(cum.disc.df, discount == x) %$%
                      quantile(-dam/1e12, cuts)}) %>%
  t %>% data.frame() %>% rownames_to_column() %>% arrange(X50.)
names(tab.ed1) <- c('discount', 'p'%&%(100*cuts))

#### PART 2: Table ED2 ####

t.cap.mid <- c(0, 1.25, 2.5, 3.75, 5)
t.cap.end <- c(0, 2, 4, 6, 8, 10)
t.cum.mid <- c(0, 10, 20, 30, 40)
t.cum.end <- c(0, 10, 20, 50, 100, 150)

cap.mid <- merge(t.cap.mid, filter(cap.df, rcp != 'rcp26b')) %>% group_by(rcp, x) %>%
  summarize(prob = sum(GDPCapDamage>x)/length(boot)) %>%
  dcast(x ~ rcp)
cap.end <- merge(t.cap.end, filter(cap.df, rcp == 'rcp26b')) %>% group_by(rcp, x) %>%
  summarize(prob = sum(GDPCapDamage>x)/length(boot)) %>%
  dcast(x ~ rcp)

cum.mid <- merge(t.cum.mid, filter(cum.uni.df, rcp != 'rcp26b')) %>%
  group_by(rcp, x, discount, rcp) %>% summarize(prob = sum(-dam/1e12>x)/(length(boot))) %>%
  dcast(x ~ rcp + discount)
cum.end <- merge(t.cum.end, filter(cum.uni.df, rcp == 'rcp26b')) %>%
  group_by(rcp, x, discount) %>% summarize(prob = sum(-dam/1e12>x)/(length(boot))) %>%
  dcast(x ~ rcp + discount)

#### PART 3: Table ED3 ####
cuts <- c(0, 10, 20, 40, 100, 200, 350)
tab.ed3 <- sapply(unique(cum.disc.df$discount),
                  function(x){
                    filter(cum.disc.df, discount == x) %$%
                      ecdf(-dam/1e12)(cuts)}) %>% subtract(1, .) %>%
  t %>% data.frame() %>% rownames_to_column() %>% merge(data.frame(d = tab.ed1$discount), ., by.x = 'd', by.y = 'rowname')
names(tab.ed3) <- c('discount', 'usd'%&%cuts%&%'tn')

#### PART 4: TeX output ####

dir.create('tables')
tex.out <- function(tab, name){
  tab.out <- xtable(tab, align = 'l'%&%paste(replicate(length(tab), 'c'), collapse = ''))
  digits(tab.out) <- 2
  print(tab.out, file = 'tables/'%&%name%&%'.tex')
}

# Table ED1 #
tex.out(tab.ed1, 'TabED1')
# Table ED2 #
tex.out(cap.mid, 'TabED2-GDPcap-mid')
tex.out(cap.end, 'TabED2-GDPcap-end')
tex.out(cum.mid, 'TabED2-gGDPcum-mid')
tex.out(cum.end, 'TabED2-gGDPcum-end')
# Table ED3 #
tex.out(tab.ed3, 'TabED3')