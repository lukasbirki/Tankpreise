source("scripts/helpers.R")

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

final_data <- do.call(rbind, datalist) %>% dplyr::rename(benzin = e5)
#write.csv(final_data, "data/df_freiburg.csv")


#Aggregation over time (everyhour)

final_data$tc <- cut(final_data$Index, breaks = time_interval)  
final_data$tc <- lubridate::ymd_hms(final_data$tc)
dplyr::count(final_data, uuid, tc) #summary statistics for how many price changes per minute (1 = baseline, no change)

final_data %>% 
  drop_na(diesel) %>% 
  dplyr::mutate(across(c(diesel:e10), as.numeric)) %>% 
  group_by(tc) %>% 
  dplyr::summarise(across(c(diesel:e10), mean,.names = "{col}_mean")) -> df

df$hms <- format(df$tc, format = "%H:%M:%S") #Ignoring date aspect https://learnr.wordpress.com/2010/02/25/ggplot2-plotting-dates-hours-and-minutes/
df$hms <- as.POSIXct(df$hms, format = "%H:%M:%S")


# #P1 Price per Hour
# df %>% 
#   group_by(hms) %>% 
#   #group_by(hour = format(.$hms, format = "%H:%M")) %>% 
#   dplyr::summarise(across(c(diesel_mean:e10_mean), mean, na.rm = T)) %>% 
#   pivot_longer(cols = c(diesel_mean:e10_mean), values_to = "price", names_to = "fuel") %>% 
#   ggplot(aes(x = hms, y = price, color = fuel))+
#   geom_line()+
#   geom_point()+
#   labs(title = paste("Durchschn. Tankpreise der Freiburger Tankstellen je", time_interval), x = "Stunde", y = "Tankpreis") +
#   scale_x_datetime(date_labels = "%H", date_breaks = "1 hour")+ #https://bookdown.dongzhuoer.com/hadley/ggplot2-book/date-time.html
#   gganimate::transition_reveal(along = hms) -> p1
# 
# 
# animate(p1, height = 500, width =1000)
# anim_save("Gapminder_example.gif")

#
liter <- 70

df %>% 
  group_by(hms) %>% 
  dplyr::summarise(across(c(diesel_mean:e10_mean), mean, na.rm = T)) %>% ungroup() %>% 
  mutate(mean_price_diesel = round(mean(diesel_mean)*liter, 2),
         price_diesel = round(diesel_mean*liter, 2),
         diff_diesel = round(mean_price_diesel-price_diesel, 2),
         dir = case_when(
           diff_diesel >= 0 ~ "up",
           diff_diesel < 0 ~ "down"
         )) ->df

df %>% 
  mutate(alpha = range01(diff_diesel)) %>% 
  ggplot(aes(x = hms))+
  geom_hline(aes(yintercept = 0))+
  geom_ribbon(aes(ymin = diff_diesel,  ymax = pmin(diff_diesel, 0), fill = "lower", alpha = alpha)) +
  geom_ribbon(aes(ymin = 0, ymax = pmin(diff_diesel, 0), fill = "higher",)) +
  geom_point(aes(y =diff_diesel) )+
  labs(title = 'Ø Preisentwicklung Freiburger Tankstellen im Tagesverlauf', 
       subtitle = "Ersparniss bei voller Tankfüllung (70l) zum Zeitpunkt {format(as.POSIXct(frame_along), format = '%H:%M')} Uhr ", x = "Uhrzeit", y = "Ersparniss",
       caption = "Data: Tankerkönig API | Visualisation: Lukas Birkenmaier") +
  scale_x_datetime(date_labels = "%H", date_breaks = "1 hour", limits = "")+ #https://bookdown.dongzhuoer.com/hadley/ggplot2-book/date-time.html
  scale_y_continuous(labels = scales::dollar_format(suffix = "€", prefix = ""), limits = c(-5,5), n.breaks = 6)+
  scale_fill_manual(name = "Gewinn / Verlust zum Tagesdurchschnitt", values = c("#a55353","#7ca553"), labels = c("Verlust", "Gewinn"))+
  theme(legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  transition_reveal(hms) -> p2


df %>% 
  ggplot(aes(x = hms))+
  geom_hline(aes(yintercept = 0))+
  geom_ribbon(aes(ymin = diff_diesel,  ymax = pmin(diff_diesel, 0), fill = "lower")) +
  geom_ribbon(aes(ymin = 0, ymax = pmin(diff_diesel, 0), fill = "higher")) +
  geom_point(aes(y =diff_diesel) )+
  labs(title = 'Ø Preisentwicklung Freiburger Tankstellen im Tagesverlauf', 
       subtitle = "Ersparniss bei voller Tankfüllung (70l) zum Zeitpunkt {format(as.POSIXct(frame_along), format = '%H:%M')} Uhr ", x = "Uhrzeit", y = "Ersparniss",
       caption = "Data: Tankerkönig API | Visualisation: Lukas Birkenmaier") +
  scale_x_datetime(date_labels = "%H", date_breaks = "1 hour")+ #https://bookdown.dongzhuoer.com/hadley/ggplot2-book/date-time.html
  scale_y_continuous(labels = scales::dollar_format(suffix = "€", prefix = ""), limits = c(-5,5), n.breaks = 6)+
  scale_fill_manual(name = "Differenz Tagesdurchschnitt:", values = c("#a55353","#7ca553"), labels = c("Verlust", "Gewinn"))+
  theme(legend.key = element_rect(fill = "transparent", colour = "transparent"),
          plot.title = element_text(face="bold"),
        legend.position = "bottom")+
  transition_reveal(hms) -> p2

animate(p2, height = 400, width =600)
anim_save("plots/example_Correlaid.gif")


df$diff_diesel[df$hms == df$hms[1]]
  
?transition_reveal