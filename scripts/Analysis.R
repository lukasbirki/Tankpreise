source("scripts/helpers.R")
readr::read_csv("data/merged_freiburg.csv") -> df


#Aggregating prices for every 30 min

df %>% 
  filter(`uuid` == "e1a15081-2556-9107-e040-0b0a3dfe563c") %>% 
  select(date, diesel:e10) %>% 
  zoo::read.zoo()-> df_1


seq.POSIXt(min(df$date), max(df$date), by = "30 min") %>% 
  as_tibble() %>% zoo::read.zoo(.) -> df_2


zoo::merge.zoo(df_1,df_2) %>% 
  zoo::fortify.zoo() %>% as.tibble %>% 
  fill(., names(.))-> s

#use case


df %>% 
  filter(`uuid` == "e1a15081-2556-9107-e040-0b0a3dfe563c") %>% 
  select(date, diesel:e10) %>% 
  zoo::read.zoo()-> df_1










df %>% 
  group_by(date15 = ceiling_date(date, '30 minutes')) %>%
  summarise(across(diesel:e10, mean, .names = "30_min_{col}"),
            n = n())

  


filter(`uuid` == "e1a15081-2556-9107-e040-0b0a3dfe563c") %>% 
  select(date, diesel:e10) -> v0


df %>% 
  tidyr::complete(date = seq.POSIXt(min(date), max(date), by = "hour")) %>% 
  filter(`uuid` == "e1a15081-2556-9107-e040-0b0a3dfe563c")  %>% 
  select(date, diesel:e10)

  
  
df %>% 
  complete(date = seq.POSIXt(min(date), max(date), by = "hour")) %>% 
  filter(`uuid` == "e1a15081-2556-9107-e040-0b0a3dfe563c") %>% 
  select(date, diesel:e10) -> v1

df %>% 
  complete(date = seq.POSIXt(min(date), max(date), by = "hour")) %>% 
  filter(`uuid` == "e1a15081-2556-9107-e040-0b0a3dfe563c") %>% 
  select(date, diesel:e10)-> v2

df %>% 
  filter(`uuid` == "e1a15081-2556-9107-e040-0b0a3dfe563c") %>% 
  complete(date = seq.POSIXt(min(date), max(date), by = "hour")) %>% 
  select(date, diesel:e10)-> v3
