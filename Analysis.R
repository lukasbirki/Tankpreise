library(plyr)
library(readr)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

mydir = "data"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles

dat_csv = ldply(myfiles, read_csv)

write_csv(dat_csv,
          paste0("data/combined_data_", Sys.Date(),"_freib_e", ".csv"))


dat_csv  %>% 
  unique
  filter(id == "e7c28ca7-8f00-417a-8cd4-0e4a8be59e39")

