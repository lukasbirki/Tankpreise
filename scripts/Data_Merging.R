
#Merging Data ----

list_price <- list.files(path = "data/2022",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

df_price <- readr::read_csv(list_price, id = "file_name") %>% 
      dplyr::rename("benzin" = e5, "benzin_change" = e5change) 

names(df_price)

df_stations %>% 
  filter(city == "Freiburg" | city == "Freiburg (Breisgau") %>% 
  inner_join(.,df_price, by = c("uuid"="station_uuid")) %>% 
  filter(longitude != 0 & latitude <50) %>% 
  mutate(station_name = paste(brand, street)) ->  df_freiburg_csv

names(df_freiburg)
unique(df_freiburg$street)

write.csv(df_freiburg_csv, "data/merged_freiburg.csv")
write_csv(df_stations, "data/list_gas_stations_freiburg.csv")
