library(tidyverse)

source('./get_data/get_snodspeak_coding.R')

# ----
# message('getting snodspeak RT data')
fnames <- list.files('../SnodSpeak/SnodSpeak_response_data', 
                     pattern = 'RESULTSSnodSpeak', 
                     full.names = T,
                     recursive = T)

DfAll <- data.frame() #create somewhere to put stuff

# loop through all filenames
# {open data, do stuff, then add it to DfAll}
for(f in fnames){
  Df <- read.table(f, 
                   header = F, 
                   sep = "\t", 
                   comment.char = "", 
                   quote = "",
                   stringsAsFactors = FALSE,
                   skip = 1, fill = T)
  
  names(Df) <- c("subname", "block_number", "image_file", 
                 "image", "DisplayImageTime", "voicekey_time", 
                 "RT")
  
  DfAll = rbind(DfAll, Df)
}

snodspeak = DfAll %>% 
  filter(subname != 'SD301014') %>% # incomplete collection
  filter(!(subname %in% c("EML290115","JH20115" ))) %>% # misunderstood task 
  mutate(image = str_pad(image,3,'left','0')) %>% 
  select(subname,image,RT)

rm(Df,DfAll,f,fnames)

# ----
# merge with coding

snodspeak <- merge(snodspeak, snodspeak_coding, by = c('subname','image'), all.x = T)

names(snodspeak)[names(snodspeak) != 'image'] <- paste0('sp_',names(snodspeak)[names(snodspeak) != 'image'])

snodspeak <- merge(snodspeak, snodspeak_byimage, by = 'image', all.x = T) %>%
  mutate(sp_is_mc_response = +(sp_response == sp_mc_response),
         sp_subno = str_pad(sp_subno,2,'left','0')) %>%
  arrange(sp_subno,image) %>%
  select(-sp_mc_response, -sp_mc_namecount, -sp_subname)

rm(snodspeak_byimage,snodspeak_coding)
  

