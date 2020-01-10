# zadnji file z oglasi in search parametri 
car_ads_tables <- list.files(path = "scraped_car_ads/", pattern = "*.csv")
message("Last car ads table: ", interval(ymd(str_sub(max(car_ads_tables), 1, 8)), as_date(now())) %/% days(1), " days ago.")
last_car_ads_table <- read_csv2(paste0("scraped_car_ads/", max(car_ads_tables)))

merged_parameters_tables <- list.files(path = "merged_parameters/", pattern = "*.csv")
message("Last merged parameters table: ", interval(ymd(str_sub(max(merged_parameters_tables), 1, 8)), as_date(now())) %/% days(1), " days ago.")
last_merged_parameters_table <- read_csv2(paste0("merged_parameters/", max(merged_parameters_tables)))






last_car_ads_table %<>% 
      mutate(name = case_when(name %in% c("1.registracija", "1. registracija") ~ "prva_registracija",
                              name == "VIN / številka šasije" ~ "VIN", 
                              TRUE ~ str_replace_all(str_to_lower(name), " ", "_")
                              )
             )


last_car_ads_table %<>% 
      mutate(name = str_replace_all(name, "š", "s")) %>%
      mutate(name = str_replace_all(name, "č", "c")) %>%
      mutate(name = str_replace_all(name, "ž", "z"))


last_car_ads_table %<>% filter(!is.na(id_ad))

single_value_parameters <- 
      last_car_ads_table %>% filter(name %in% c("kratek_naziv_avtomobila",
                                                "dolg_naziv_avtomobila",
                                                "cena",
                                                "starost",
                                                "prva_registracija",
                                                "prevozenih_km",
                                                "tehnicni_pregled",
                                                "motor",  
                                                "gorivo", 
                                                "menjalnik", 
                                                "oblika", 
                                                "barva",  
                                                "VIN", 
                                                "zgodovina_vozila", 
                                                "kraj_ogleda", 
                                                "kombinirana_voznja", 
                                                "izvenmestna_voznja", 
                                                "mestna_voznja", 
                                                "emisijski_razred", 
                                                "emisija_co2")) %>%
      pivot_wider(id_cols = id_ad, names_from = name, values_from = value)



single_value_parameters %<>%
      separate(kratek_naziv_avtomobila, c("znamka", "model"), " ", extra = "merge") %>%
      mutate(prva_registracija = if_else(str_detect(prva_registracija, "^\\d\\d\\d\\d "), str_sub(prva_registracija, 6), prva_registracija)) %>% 
      mutate(prva_registracija = case_when(str_length(prva_registracija) > 5 ~ parse_date_time(prva_registracija, "m / Y"),
                                           TRUE ~ parse_date_time(prva_registracija, "Y"))) %>%
      mutate(tehnicni_pregled = parse_date_time(tehnicni_pregled, "m / Y")) %>% 
      separate(motor, c("motor_ccm", "motor_kw", "motor_km"), " ccm, | kW \\(", fill = "left") %>%
      mutate(motor_km = str_remove(motor_km, " KM\\)")) %>% 
      mutate(motor_kw = str_replace_all(motor_kw, "\\D", "")) %>%
      mutate_at(vars(kombinirana_voznja, izvenmestna_voznja, mestna_voznja, emisijski_razred, emisija_co2), ~ replace(., . == "ni podatka", NA)) %>%
      mutate_at(vars(kombinirana_voznja, izvenmestna_voznja, mestna_voznja, emisija_co2), ~ str_remove(., " litrov \\/ 100 km| g \\/ km")) %>% 
      mutate_at(vars(kombinirana_voznja, izvenmestna_voznja, mestna_voznja, emisija_co2), ~ str_replace(., ",", ".")) %>% 
      mutate(kraj_ogleda = replace(kraj_ogleda, kraj_ogleda == ",", NA)) %>% 
      mutate_at(vars(cena, prevozenih_km, motor_ccm, motor_kw, motor_km, kombinirana_voznja, izvenmestna_voznja, mestna_voznja, emisija_co2), ~as.numeric(.)) %>%
      mutate(menjalnik = str_remove(menjalnik, "/ tiptronic")) %>% 
      mutate_at(vars(znamka, model, starost, gorivo, menjalnik, oblika, barva, emisijski_razred), ~as_factor(.))

single_value_parameters %>%
      group_by(letnik = year(prva_registracija), model) %>%
      filter(model %in% c("Superb", "Octavia", "Berlingo")) %>%
      summarise(mean_price = mean(cena, na.rm = TRUE), n = n()) %>%
      arrange(model, letnik) %>%
      pivot_wider(id_cols = letnik, names_from = model, values_from = c(mean_price, n)) %>%
      mutate(delta_S_O = mean_price_Superb - mean_price_Octavia,
             delta_O_B = mean_price_Octavia - mean_price_Berlingo,
             delta_S_B = mean_price_Superb - mean_price_Berlingo) %>%
      print(n = 100)


single_value_parameters %>% filter(znamka == "Citroen") %>% distinct(model)
