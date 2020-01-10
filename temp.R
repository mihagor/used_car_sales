# temp 

x18 <-read_csv2(paste0("scraped_car_ads/20191218_scraped_car_ad_data.csv"))
x19 <-read_csv2(paste0("scraped_car_ads/20191219_scraped_car_ad_data.csv"))

tmp <- x19 %>% distinct()

tmp <- bind_rows(x18, x19) %>% distinct()
write_csv2(tmp, path = paste0("scraped_car_ads/20191219_scraped_car_ad_data.csv"), col_names = TRUE)