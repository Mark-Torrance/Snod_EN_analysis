library(tidyverse)
library(magrittr)

message("getting by_key data")

load("../SnodWrite_EN/snod_EN_keydata.Rdata")

# ----

snod_bykey %<>%  
  mutate(image = str_remove(image,'\\.gif'), 
         image = str_pad(image,3,'left','0')) %>% 
  rename(IKI = IKI_mod)

snod_bykey %<>% 
  filter(EVENT_TYPE == 'INSERT',pwd_isfluent) %>%
  group_by(subno,image) %>% 
  mutate(letter.id = 1,
         letter.id = cumsum(letter.id),
         word.length = max(letter.id),
         RT = max(ifelse(letter.id == 1, IKI,0)))


# ----
# get name codes

# for written production several different responses can represent
# the same name: acccordian, accordion, accordien and no single written
# word represents the most common name. Therefore need name codes.


message('getting snodwrite namecodes')

namecodes <- read_delim("../SnodWrite_EN/words_EN_wordcodes.txt",
                        "\t", escape_double = FALSE, trim_ws = F)

namecodes %<>%
  select(word, image = gif, namecode, spell_tag) %>% 
  mutate(image = str_pad(image,3,'left','0'),
         namecode = str_extract(namecode, "(?<=_)[^_]*$"),
         namecode = paste(image,namecode,sep = '_')) %>% 
  arrange(word)

# sort out codes for whitespace and # words that weren't coded and add to namecodes
# double checked that this works correctly 25.01.19. and it does
# viewing data in RStudio trims white space, which is confusing

snod_bykey_temp <- merge(snod_bykey, namecodes, by = c('image','word'), all.x = T, all.y = F)

Df <- as.tibble(snod_bykey_temp) %>% filter(is.na(namecode)) %>% 
  group_by(image,word) %>% 
  summarise(N = n()) %>% # just a way of getting responses
  select(orig.word = word, image) %>% 
  mutate(word = str_trim(orig.word,"both"),
         word = str_remove(word,"\\#")) %>% 
  left_join(namecodes,by = c('word','image')) %>% # lookup namecodes
  mutate(spell_tag = case_when(is.na(spell_tag) & !(str_detect(orig.word,"\\#"))~'WS',
                               !(is.na(spell_tag)) & !(str_detect(orig.word,"\\#"))~paste('WS',spell_tag),
                               is.na(spell_tag) & str_detect(orig.word,"\\#")~'SX')) %>% 
  select(word = orig.word,image,namecode,spell_tag)

namecodes <- bind_rows(namecodes, Df)


# and merge in

snod_bykey <- merge(snod_bykey, namecodes, by = c('image','word'), all.x = T, all.y = F)

# ----
# merge with by_image data

message("getting by_image data")

by_image <- read_delim("../SnodWrite_EN/byimage_EN.txt",
"\t", escape_double = FALSE, trim_ws = F) %>% 
  select(-c(mc_response_len:image.gif))

snod_bykey <- merge(snod_bykey, by_image, by = 'image', all.x = T, all.y = F) %>% 
  arrange(subno,image,letter.id) %>% 
  mutate(is_mc_name = ifelse(mc_namecode == namecode,1,0))

snod_bykey %<>% select(-EVENT_TYPE)


# ----
# add in ngraph frequencies

freq_letter <- read_delim("../SnodWrite_EN/BNC_letters.txt",
                       "\t", escape_double = FALSE, trim_ws = F, col_names = FALSE)
names(freq_letter) = c('KE_KEY','letter_frq','letter_cv')

freq_dig <- read_delim("../SnodWrite_EN/BNC_digs.txt",
                       "\t", escape_double = FALSE, trim_ws = F, col_names = FALSE)
names(freq_dig) = c('dig','dig_frq','dig_cv')

freq_trig <- read_delim("../SnodWrite_EN/BNC_trigs.txt",
                       "\t", escape_double = FALSE, trim_ws = F, col_names = FALSE)
names(freq_trig) = c('trig','trig_frq','trig_cv')

snod_bykey %<>% 
  group_by(subno,image) %>% 
  mutate(k1 = lead(KE_KEY,1),
         k2 = lead(KE_KEY,2),
         dig = ifelse(!is.na(k1),tolower(paste0(KE_KEY,k1)),NA),
         trig = ifelse(!(is.na(dig)|is.na(k2)),tolower(paste0(dig,k2)),NA)) %>% 
  left_join(freq_letter, by = 'KE_KEY') %>% 
  left_join(freq_dig, by = 'dig') %>% 
  left_join(freq_trig, by = 'trig') %>% 
  select(-k1,-k2)


# ---- 
# cleanup and save

rm(list=Filter(function(x) {c("data.frame") %in% class(get(x)) & 
                            !(x %in% c("snod_bykey"))}, ls()))

save(snod_bykey,file = "./data_local/snod_EN_keydata_2.Rdata")




