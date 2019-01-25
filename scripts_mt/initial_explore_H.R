library(tidyverse)
library(magrittr)
source('get_by-image/get_spoken_by-image.R')
source('get_data/load_snodwrite.R')

write_by_image <- snod_bykey %>% 
  filter(ispwordinit == 1) %>% 
  group_by(image, mc_response) %>% 
  summarise(name_H = max(name_H)) %>% 
  rename(write_mc_response = mc_response,
         write_name_H = name_H)

Df = left_join(spoken_by_image,write_by_image)

# where modal names differ
Df %>% filter(!(sp_mc_name == write_mc_response))


Df %>% ggplot(aes(x = sp_name_H, y = write_name_H)) +
  geom_point() +
  lims(x = c(0,3), y = c(0,3))

ggsave('./plots/H_scatter.png', units = 'cm', height = 15, width = 15)

Df %>% select(image,sp_name_H,write_name_H) %>% 
  gather(DV,score,-image) %>% 
  ggplot(aes(score))+
  geom_histogram(bins = 20, colour = 'white') +
  facet_wrap(~DV)

with(Df, cor(write_name_H,sp_name_H))

summary(Df %>% select(image,sp_name_H,write_name_H) %>% 
  gather(DV,score,-image) %>%
  lm(score~DV, data = .))
