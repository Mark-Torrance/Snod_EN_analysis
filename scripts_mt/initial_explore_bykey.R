source('./get_data/load_snodwrite.R')

library(stringr)
# ----

Df <- snod_bykey %>%
  mutate(image = as.numeric(image)) %>% 
  filter(letter.id > 1, 
         #word.length < 11 & word.length > 3,
         #IKI < 500,
         is_mc_name,
         image == 23
         #word != mc_response
         )
  #mutate(H_hilo = ifelse(name_H < median(name_H),"low","high"),
  #       RT_hilo = ifelse(RT < median(RT),"low","high")) %>% 
  # group_by(word,letter.id, word.length) %>% 
  # summarise(IKI = mean(IKI)) %>%
  # group_by(letter.id,word.length) %>% 
  # mutate(agIKI = mean(IKI, na.rm = T))

Df %>% 
  ggplot(aes(x = letter.id, y = IKI)) +
  #geom_jitter(width = .1, alpha = .3, size = .2) +
  geom_line(aes(group = subno)) +
  geom_line(aes(group = word), 
            stat = 'summary', fun.y = "mean", na.rm = T, colour = "red") +
  geom_label(aes(label = str_to_lower(KE_KEY)), alpha = .8, size = 3) +
  # geom_point(stat = 'summary', fun.y = "mean", na.rm = T,
  #            size = 3, shape = 18) +
  #lims(y = c(75,275)) +
  facet_wrap(~word, scales = "free_y") 
  

