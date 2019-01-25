library(tidyverse)

# remove lots of variables from raw keystroke data and save as Rdata

snod_bykey <- read_delim("../SnodWrite_EN/EN_ko_all.txt",
                 "\t", escape_double = FALSE, trim_ws = FALSE) %>% 
  select("subno", "image", "keycode", "EVENT_TYPE", "EVENT_TIME", 
         "uplag_time", "uplag_keycount", "KE_KEY", "IKI_mod", "word", 
         "ispwordinit", "ispwordend", "pwd_isfluent")


save(snod_bykey,file = "../SnodWrite_EN/snod_EN_keydata.Rdata")
  

