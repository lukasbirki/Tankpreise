library(plyr)
library(readr)
library(tidyverse)

#Merging Data ----

list_price <- list.files(path = "data/2022",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

df_price <- readr::read_csv(list_price, id = "file_name")
names(df_price)

df_stations<- readr::read_csv(file = "data/2022-01-01-stations.csv")

df_stations %>% 
  filter(city == "Freiburg" | city == "Freiburg (Breisgau") %>% 
  inner_join(.,df_price, by = c("uuid"="station_uuid")) ->  df_freiburg

names(df_freiburg)
unique(df_freiburg$street)

write.csv(df_freiburg, "data/merged_freiburg.csv")


# Analysis ----

#tba.