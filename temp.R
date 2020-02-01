# temp 

x18 <-read_csv2(paste0("scraped_car_ads/20191218_scraped_car_ad_data.csv"))
x19 <-read_csv2(paste0("scraped_car_ads/20191219_scraped_car_ad_data.csv"))

tmp <- x19 %>% distinct()

tmp <- bind_rows(x18, x19) %>% distinct()
write_csv2(tmp, path = paste0("scraped_car_ads/20191219_scraped_car_ad_data.csv"), col_names = TRUE)


##############################################################################################################################
# add import date ... date 1st imported on 2020-01-05 

last_car_ads_table %<>%
      group_by(id_ad) %>%
      summarise(last(id_ad)) %>%
      select(id_ad) %>%
      mutate(name = "Datum uvoza", attribute = "value1", value = format(now(), "%Y%m%d")) %>%
      bind_rows(last_car_ads_table, .) %>%
      arrange(id_ad)


write_csv2(last_car_ads_table, path = paste0("scraped_car_ads/", format(Sys.Date(), "%Y%m%d"), "_scraped_car_ad_data.csv"), col_names = TRUE)


##############################################################################################################################

