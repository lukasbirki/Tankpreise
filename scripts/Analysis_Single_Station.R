source("scripts/helpers.R")
readr::read_csv("data/merged_freiburg.csv") -> df


#Filling in prices for every 30 min

time_interval <- "30 mins"

df %>% 
  filter(`uuid` == "00060034-0011-4444-8888-acdc00000011") %>% 
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
  labs(title = element_text(paste("Fuel Prices every", time_interval)), x = "Hour", y = "Fuel Price")+
  scale_x_datetime(date_labels = "%H", date_breaks = "1 hour") #https://bookdown.dongzhuoer.com/hadley/ggplot2-book/date-time.html
  
ggsave(paste("plots/","Fuel Prices ", time_interval, ".png", sep = ""))

