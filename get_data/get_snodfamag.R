library(tidyverse)
library(magrittr)

# ----
# get coding data

message('getting snodspeak familiarity and agreement data')

get_famag = function(pth) {
  
  message(paste("getting",pth))
  
  fnames <- list.files(pth, 
                       pattern = '.csv', 
                       full.names = T,
                       recursive = T)
  
  DfAll <- data.frame() #create somewhere to put stuff
  
  # loop through all filenames
  # {open data, do stuff, then add it to DfAll}
  
  i = 1
  for(f in fnames){
  
    Df <- read.table(f, 
                     header = F, 
                     sep = ",", 
                     comment.char = "", 
                     quote = "\"",
                     stringsAsFactors = FALSE,
                     skip = 1, fill = T)
    
    names(Df) <- c("image.png","score","RT")
    
    Df$subno = str_pad(i,2,'left','0')
    i = i+1
    
    #print(f)
    
    DfAll = rbind(DfAll, Df)
  }
  
  image.key <- data.frame(image.png = unique(DfAll$image.png)) %>%
    arrange(image.png) %>% 
    mutate(image = 1,
           image = cumsum(image),
           image = str_pad(image,3,'left','0'))
  
  dat <- merge(DfAll,image.key,by = 'image.png', all.x = T) %>%
    select(image, subno, score) %>%
    group_by(image) %>% 
    summarise(M = mean(score))
  
  rm(Df,DfAll,f,fnames,i,image.key)
  
  return(dat)
}

snodfam <- get_famag('../SnodFamAg/fam output') %>% 
  rename(familiarity = M)
  
snodagree <- get_famag('../SnodFamAg/agree output') %>% 
  rename(agreement = M)

snod_famag = merge(snodfam,snodagree, by = 'image')

rm(snodagree,snodfam,get_famag)
