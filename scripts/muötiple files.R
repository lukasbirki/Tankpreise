readr::read_csv("data/merged_freiburg.csv") -> df
time_interval <- "30 mins"

tankstellen <-unique(df$uuid)

seq.POSIXt(min(df$date), max(df$date), by = time_interval) %>% 
  as_tibble() %>% zoo::read.zoo(.) -> df_2

datalist = list()
index <-  1

for (i in tankstellen){
  df %>% 
    filter(`uuid` == i) %>% 
    select(date, uuid, diesel:e10) %>% 
    zoo::read.zoo() %>% 
    zoo::merge.zoo(.,df_2) %>% 
    zoo::fortify.zoo() %>% as_tibble %>% 
    mutate(across(c(diesel:e10), as.numeric)) %>% 
    fill(., names(.)) -> temp
  
  datalist[[i]] <- temp # add it to the list of dfs
  index <<- index + 1
}

final_data <- do.call(rbind, datalist)

#Aggregation over time (everyhour)

final_data$tc <- cut(final_data$Index, breaks = time_interval)  
final_data$tc <- lubridate::ymd_hms(final_data$tc)
count(final_data, tc, uuid) #summary statistics for how many price changes per minute (1 = baseline, no change)

final_data %>% 
  group_by(uuid, tc) %>% 
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
  labs(title = element_text(paste("Durchschn. Tankpreise der Freiburger Tankstellen je", time_interval, "inkl")), x = "Stunde", y = "Tankpreis")+
  scale_x_datetime(date_labels = "%H", date_breaks = "1 hour") #https://bookdown.dongzhuoer.com/hadley/ggplot2-book/date-time.html
