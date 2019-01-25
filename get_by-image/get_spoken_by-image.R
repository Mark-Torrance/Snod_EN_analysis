library(tidyverse)
library(magrittr)
library(lme4)
source('./get_data/get_snodspeak.R')

# find subjects where > 10% missing data
missing <- snodspeak %>% 
  mutate(error = ifelse(sp_RT < 0, 'missing',0)) %>% 
  group_by(sp_subno, error) %>% 
  summarise(N = n()) %>% 
  spread(error,N) %>% 
  filter(missing >= 260*.1)

# remove subjects with too much missing data
Df <- snodspeak %>% 
  filter(!(sp_subno %in% missing$sp_subno))
(nrow(snodspeak) - nrow(Df)) / nrow(snodspeak)

# remove trials where response is not modal name
Df %<>% 
  filter(sp_is_mc_response == 1)
(nrow(snodspeak) - nrow(Df)) / nrow(snodspeak)

# remove fast and slow trials
Df %<>% 
  filter(sp_RT > 300, sp_RT < 5000)
(nrow(snodspeak) - nrow(Df)) / nrow(snodspeak)

# get RT by image (raw means)
RTs = Df %>% 
  #unite(image,image,sp_response) %>% 
  group_by(image) %>% 
  summarise(M = mean(sp_RT),
            SD = sd(sp_RT))

# get RT by image (MER model)
M <- lmer(sp_RT ~ 0+ image + (1|sp_subno), data = Df) 
RTs$model.M <- round(summary(M)$coef[,'Estimate'],1)

spoken_by_image <- left_join(RTs, unique(Df %>% 
                   select(image,sp_response,sp_name_H))) %>% 
  select(image,sp_mc_name = sp_response, sp_name_H, sp_RT = model.M)
  
rm(list=Filter(function(x) {!(x %in% c('spoken_by_image'))}, ls()))


  