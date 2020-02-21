## extract single and multi parameters from:
##     scraped car ads (2_web_scraping.R) and 
##     merged parameters table (6_merge_parameters.R) -- NOT ACTUAL ANYMORE




# zadnji file z oglasi in search parametri 
car_ads_tables <- list.files(path = "scraped_car_ads/", pattern = "*.csv")
message("Last car ads table: ", interval(ymd(str_sub(max(car_ads_tables), 1, 8)), as_date(now())) %/% days(1), " days ago.")
last_car_ads_table <- read_csv2(paste0("scraped_car_ads/", max(car_ads_tables)))


# Talisman dvojni prevoženi
#last_car_ads_table %>% filter(id_ad == 15065817)
# A4 dvojni prevoženi in prva_registracija
#last_car_ads_table %>% filter(id_ad == 15052997)




##### Initial data manipulation #####

## TEMP because of web scraping problems (duple prevozeni_km in duple prva_registracija)
last_car_ads_table %<>%
   group_by(id_ad, name) %>% 
   mutate(attribute = paste0("value", row_number())) %>% 
   filter(!(name == "Prevoženi km" & attribute == first(attribute) | name == "Prva registracija" & attribute == last(attribute))) %>%
   ungroup(name)




last_car_ads_table %<>% 
   mutate(name = str_replace_all(name, "š", "s")) %>%
   mutate(name = str_replace_all(name, "č", "c")) %>%
   mutate(name = str_replace_all(name, "ž", "z"))

last_car_ads_table %<>% 
      mutate(name = case_when(name %in% c("1.registracija", "1. registracija") ~ "prva_registracija",
                              name %in% c("Leto proizvodnje", "Letnik")        ~ "letnik_proizvodnje",
                              name == "VIN / stevilka sasije"                  ~ "VIN", 
                              name == "Prevozeni km"                           ~ "prevozenih_km",
                              name == "Pogonsko Gorivo"                        ~ "gorivo",
                              name == "Oblika karoserije"                      ~ "oblika",
                              TRUE ~ str_replace_all(str_to_lower(name), " ", "_")
                              )
             )


last_car_ads_table %<>% filter(!is.na(id_ad))

##### Single value parameters #####

single_parameters <- c("kratek_naziv_avtomobila",
                       "dolg_naziv_avtomobila",
                       "cena",
                       "starost",
                       "prva_registracija",
                       "letnik_proizvodnje",
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
                       "emisija_co2",
                       "datum_uvoza",
                       "status_zaloge",
                       "interna_stevilka",
                       "certifikat_vozila")


single_value_parameters <- 
      last_car_ads_table %>% filter(name %in% single_parameters) %>% 
      group_by(id_ad, name, attribute) %>% filter(row_number() == max(row_number())) %>% # duplikati so na prevoženih kilometrih - pravi podatek je zadnji podatek
      pivot_wider(id_cols = id_ad, names_from = name, values_from = value) %>% 
      select(id_ad, single_parameters)

single_value_parameters %<>% 
   mutate(letnik_proizvodnje = parse_date_time(letnik_proizvodnje, "Y"), quiet = TRUE) %>%
   mutate(datum_uvoza = ymd(datum_uvoza), quiet = TRUE) 


single_value_parameters %<>%
      separate(kratek_naziv_avtomobila, c("znamka", "model"), " ", extra = "merge") %>%
      mutate(prva_registracija = if_else(str_detect(prva_registracija, "^\\d\\d\\d\\d "), 
                                         str_sub(prva_registracija, 6), 
                                         prva_registracija)) %>% 
      mutate(prva_registracija = case_when(str_length(prva_registracija) > 5 ~ parse_date_time(prva_registracija, "m / Y"),
                                           TRUE ~ parse_date_time(prva_registracija, "Y"))) %>%
      mutate(prva_registracija = if_else(starost == "novo", floor_date(parse_date_time(Sys.Date(), "Y-m-d"), unit = "month"), prva_registracija)) %>%
      mutate(tehnicni_pregled = parse_date_time(tehnicni_pregled, "m / Y")) %>% 
      separate(motor, c("motor_ccm", "motor_kw", "motor_km"), " ccm, | kW \\(", fill = "left") %>%
      mutate(motor_km = str_remove(motor_km, " KM\\)")) %>% 
      mutate(motor_kw = str_replace_all(motor_kw, "\\D", "")) %>%
      mutate_at(vars(kombinirana_voznja, izvenmestna_voznja, mestna_voznja, emisijski_razred, emisija_co2), ~ replace(., . == "ni podatka", NA)) %>%
      mutate_at(vars(kombinirana_voznja, izvenmestna_voznja, mestna_voznja, emisija_co2), ~ str_remove(., " litrov \\/ 100 km| g \\/ km")) %>% 
      mutate_at(vars(kombinirana_voznja, izvenmestna_voznja, mestna_voznja, emisija_co2), ~ str_replace(., ",", ".")) %>% 
      mutate(kraj_ogleda = replace(kraj_ogleda, kraj_ogleda == ",", NA)) %>% 
      mutate(motor_km = round(parse_number(motor_km, locale = locale(grouping_mark = ".")), 0)) %>% 
      mutate_at(vars(cena, prevozenih_km, motor_ccm, motor_kw, kombinirana_voznja, izvenmestna_voznja, mestna_voznja, emisija_co2), ~as.numeric(.)) %>%
      mutate(menjalnik = str_remove(menjalnik, "/ tiptronic"))
      
sort_vars <- c("id_ad", "parameter_group", "attribute", "name", "value", "value_property")

single_value_parameters_long <- 
   single_value_parameters %>%
      mutate_all(~as.character(.)) %>%
      pivot_longer(cols = -id_ad, names_to = "name", values_to = "value") %>%
      ungroup() %>%
      mutate(id_ad = as.numeric(id_ad)) %>%
      mutate(parameter_group = "single") %>%
      mutate(attribute = name) %>%
      mutate(value_property = NA_character_) %>%
      select(!!sort_vars)


##### Multi value parameters ####

multi_value_parameters_long <- 
   last_car_ads_table %>% 
      select(-attribute) %>%
      mutate(name = if_else(name == "notranjost" & str_detect(value, "\\/|usnje"), "interier", name)) %>%
      filter(!name %in% c("opomba", "interier")) %>% 
      filter(!(name %in% single_parameters)) %>% 
      rename(attribute = name) %>%
      separate(value, into = c("name", "value_property"), sep = ":", extra = "merge") %>% 
      mutate(value_property = str_trim(value_property)) %>% 
      mutate(value_property = if_else(str_detect(name, ".*(?= x zračna vreča / Airbag)"), 
                                      str_extract(name, ".*(?= x zračna vreča / Airbag)"), 
                                      value_property)) %>%
      mutate(name = str_replace(name, "(\\d|\\d\\d) x zračna vreča / Airbag", "airbag")) %>%
      mutate(name = str_remove_all(name, "<strong>|</strong>")) %>%
      mutate(value = name) %>%
      mutate(parameter_group = "multi") %>%
      select(!!sort_vars)



##### Bind parameters ####
all_parameters <- bind_rows(single_value_parameters_long, multi_value_parameters_long)

write_csv2(all_parameters, path = paste0("data_manipulation/", format(Sys.Date(), "%Y%m%d"), "_data_manipulation.csv"), col_names = TRUE)



