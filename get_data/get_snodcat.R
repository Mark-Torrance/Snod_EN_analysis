library(tidyverse)


fnames <- list.files('../SnodCat/snodcatdata', 
                     pattern = 'RESULTSsnodcat', 
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
  
  names(Df) <- c("session_name", "block_number", "image_file", 
                 "image", "DisplayImageTime", "keypressed_time", 
                 "key_pressed", "RT")

  DfAll = rbind(DfAll, Df)
  }

snodcat = DfAll
rm(Df,DfAll,f,fnames)