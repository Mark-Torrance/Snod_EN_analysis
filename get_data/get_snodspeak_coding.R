library(tidyverse)
library(magrittr)

# ----
# get coding data

message('getting snodspeak coding data')

fnames <- list.files('../SnodSpeak/SnodSpeak_coding_data', 
                     pattern = 'RESULTSSnodSpeak_coding', 
                     full.names = T,
                     recursive = T)

DfAll <- data.frame() #create somewhere to put stuff

# loop through all filenames
# {open data, do stuff, then add it to DfAll}
for(f in fnames){
  
  Df <- read.table(textConnection(gsub("\t\t", "\t", readLines(f))), 
                   header = F, 
                   sep = "\t", 
                   comment.char = "", 
                   quote = "\"",
                   stringsAsFactors = FALSE,
                   skip = 1, fill = T)
  
  names(Df) <- c("session_name", "image", "wavfile", 
                 "subno", "subname", "response_value")
  #print(f)
  
  DfAll = rbind(DfAll, Df)
}


DfAll %<>% 
  mutate(code = str_pad(response_value,2,side = "left", pad = "0"),
         code = paste(image, code, sep = "_"))

snodspeak_coding = DfAll
rm(Df,DfAll,f,fnames)

# ----
# get coding datasources - just to create number-code to response key

message('getting snodspeak coding datasources')

fnames <- list.files('../SnodSpeak/coding_datasources', 
                     pattern = 'SPDatasource', 
                     full.names = T,
                     recursive = T)


DfAll <- data.frame()

for(f in fnames){
  Df <- read.table(textConnection(gsub("\t\t", "\t", readLines(f))), 
                   header = F, 
                   sep = "\t", 
                   comment.char = "", 
                   quote = "\"",
                   stringsAsFactors = FALSE,
                   skip = 1, fill = T)
  
  names(Df) <- c("image", "wavfile", "subno", "subname", 
                 "name1", "name2", "name3", "name4", "name5", 
                 "name6", "name7", "name8", "name9", "name10")
  
  #print(f)
  
  Df %<>% 
    select(-wavfile,-subno,-subname) %>%
    distinct(image, .keep_all = T) %>%
    gather(code,response,-image) %>%
    mutate(code = str_remove(code,"name"),
           code = str_pad(code,2,side = "left", pad = "0"),
           code = paste(image, code, sep = "_")) %>%
    arrange(code) %>%
    select(-image)
  
  DfAll = rbind(DfAll, Df)
}

response_key = DfAll
rm(Df,DfAll,f,fnames)

# ----
# add in (text of the) responses
snodspeak_coding <- merge(snodspeak_coding,response_key, 
              by = 'code', all.x = T, sort = F)



snodspeak_coding %<>%
  mutate(image = str_pad(image,3,'left','0')) %>% 
  arrange(image,subno) %>%
  filter(!subno %in% c(27,41)) %>% # misunderstood task
  filter(subname != 'SD301014') %>% # incomplete collection
  mutate(response_value = ifelse(is.na(response_value)
                                 ,'z',response_value),
         response = ifelse(is.na(response),response_value,response),
         ifelse(str_length(response) == 1, 'z', response)) %>% 
  select(subno,subname,image,response)

# check this
snodspeak_byimage <- snodspeak_coding %>%
  group_by(image) %>%
  mutate(N_tot = n()) %>%
  group_by(image,response) %>% 
  summarise(N = n(), N_tot = max(N_tot)) %>% 
  mutate(mc_name = ifelse(N == max(N),1,0),
         Nz = max(ifelse(response == 'z',N,0)),
         N_resp = N_tot - Nz,
         pc = N/N_resp,
         pcNR = Nz/(N_tot+Nz),
         H = pc*log2(1/pc)) %>%
  filter(response != 'z') %>% 
  group_by(image) %>% 
  mutate(H = sum(H)) %>% 
  filter(mc_name == 1) %>% 
  select(image, mc_response = response, mc_namecount = N, mc_name_pc = pc, 
         name_H = H, pcNR)

names(snodspeak_byimage)[-1] <- paste0("sp_",names(snodspeak_byimage)[-1])
  






