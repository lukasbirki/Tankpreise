source("scripts/helpers.R")
readr::read_csv("data/merged_freiburg.csv") -> df


#Filling in prices for every 30 min

time_interval <- "30 mins"

df %>% 
  filter(`uuid` == "e1a15081-2556-9107-e040-0b0a3dfe563c") %>% 
  select(date, uuid, diesel:e10) %>% 
  zoo::read.zoo()-> df_1


seq.POSIXt(min(df$date), max(df$date), by = time_interval) %>% 
  as_tibble() %>% zoo::read.zoo(.) -> df_2


zoo::merge.zoo(df_1,df_2) %>% 
  zoo::fortify.zoo() %>% as_tibble %>% 
  mutate(across(c(diesel:e10), as.numeric)) %>% 
  fill(., names(.)) -> df_merged

#Aggregation over time (everyhour)

df_merged$tc <- cut(df_merged$Index, breaks = time_interval)  
df_merged$tc <- lubridate::ymd_hms(df_merged$tc)
count(df_merged, tc) #summary statistics for how many price changes per minute (1 = baseline, no change)

df_merged %>% 
  group_by(tc) %>% 
  dplyr::summarise(across(c(diesel:e10), mean,.names = "{col}_mean")) -> df

df$hms <- format(df$tc, format = "%H:%M:%S") #Ignoring date aspect https://learnr.wordpress.com/2010/02/25/ggplot2-plotting-dates-hours-and-minutes/
df$hms <- as.POSIXct(df$hms, format = "%H:%M:%S")


df %>% 
  group_by(hms) %>% 
  #group_by(hour = format(.$hms, format = "%H:%M")) %>% 
  dplyr::summarise(across(c(diesel_mean:e10_mean), mean, na.rm = T)) %>% 
  pivot_longer(cols = c(diesel_mean:e10_mean), values_to = "price", names_to = "fuel") %>% 
  ggplot(aes(x = hms, y = price, color = fuel))+
  geom_line()+
  geom_point()+
  scale_x_datetime("Daytime", date_labels = "%H", date_breaks = "1 hour") #https://bookdown.dongzhuoer.com/hadley/ggplot2-book/date-time.html
  

#Aggregation over time (every 10 min)

df_merged$tc <- cut(df_merged$Index, breaks = "10 mins")  
df_merged$tc <- lubridate::round_date(df_merged$Index, "15 mins")
count(df_merged, tc) #summary statistics for how many price changes per minute (1 = baseline, no change)

df_merged %>% 
  group_by(tc) %>% 
  dplyr::summarise(across(c(diesel:e10), mean,.names = "{col}_mean_hour")) -> df


df %>% 
  group_by(hour = as.numeric(format(.$tc, format = "%H"))) %>% 
  dplyr::summarise(across(c(diesel_mean_hour:e10_mean_hour), mean, na.rm = T)) %>% 
  pivot_longer(cols = c(diesel_mean_hour:e10_mean_hour), values_to = "price", names_to = "fuel") %>% 
  ggplot(aes(x = hour, y = price, color = fuel))+
  geom_line()+
  geom_point()










df_merged %>% 
  mutate(interval = lubridate::minute(Index) %/% 3) %>%
  group_by(interval) %>%
  summarise(count = n())


df_merged$dater <- as.Date(df_merged$date)
aggregate(energy$value, by=list(energy$Date), sum)

summarise_by_time()
summarize_b

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
