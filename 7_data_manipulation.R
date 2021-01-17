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

tic("data manipulation for wide and long format")

##### DATA LONG #####
tic("data long...")
##### _Initial data manipulation #####
last_car_ads_table %<>% filter(!is.na(id_ad))

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

## TEMP because of web scraping problems (duple prevozeni_km in duple prva_registracija)
error_vars <- c("prevozenih_km", "prva_registracija")

last_car_ads_table %<>%
   group_by(id_ad, name) %>% 
   mutate(attribute = row_number())

error_vars_data <-
   last_car_ads_table %>%
   filter(name %in% error_vars) %>%
   ungroup(name) %>%
   filter(name == "prevozenih_km" & attribute == max(attribute) | name == "prva_registracija" & attribute == min(attribute))

non_errors_vars_data <-
   last_car_ads_table %>%
   filter(!(name %in% error_vars)) %>%
   ungroup(name)
   

last_car_ads_table <- bind_rows(error_vars_data, non_errors_vars_data)


##### _Single value parameters #####
tic("long single parameters")
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
      group_by(id_ad, name, attribute) %>% #filter(row_number() == max(row_number())) %>% # duplikati so na prevoženih kilometrih - pravi podatek je zadnji podatek
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

toc()

##### _Multi value parameters ####
tic("long multi parameters")

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

toc()

##### _Save ####
all_parameters <- bind_rows(single_value_parameters_long, multi_value_parameters_long)

write_csv2(all_parameters, path = paste0("data_long/", format(Sys.Date(), "%Y%m%d"), "_data_long.csv"), col_names = TRUE)

toc()













##### DATA  WIDE #####
tic("data wide")

##### _Single value parameters ####
tic("wide single parameters")

single <- 
   all_parameters %>% 
   filter(parameter_group == "single") %>%
   pivot_wider(id_cols = "id_ad", names_from = "attribute", values_from = "value")


# Data definition
numeric <- c("id_ad", "cena", "letnik_proizvodnje", "prevozenih_km", "motor_ccm", "motor_kw", "motor_km", 
             "kombinirana_voznja", "izvenmestna_voznja", "mestna_voznja", "emisija_co2")
date <- c("prva_registracija", "tehnicni_pregled", "datum_uvoza")
string <- names(data)[!(names(data) %in% c(numeric, date))]

## Check if all remaining really are strings
# single %>% 
#    select(!!string) %>%
#    glimpse()

single %<>%
   mutate_at(vars(numeric), ~as.numeric(.)) %>%
   mutate_at(vars(date), ~parse_date_time(., "Y-m-d")) %>%
   mutate(menjalnik = str_extract(menjalnik, "ročni menjalnik|avtomatski menjalnik"))

# One hot encode gorivo in menjalnik
gorivo <- single %>% distinct(gorivo) %>% mutate(gorivo = str_replace_all(tolower(gorivo), " ", "_")) %>% filter(!is.na(gorivo)) %>% pull(1)
menjalnik <- single %>% distinct(menjalnik) %>% mutate(menjalnik = str_replace_all(tolower(menjalnik), " ", "_")) %>% filter(!is.na(menjalnik)) %>% pull(1)

gorivo_wide <- 
   single %>%
   mutate(gorivo = str_replace_all(tolower(gorivo), " ", "_")) %>%
   pivot_wider(id_cols = "id_ad", names_from = "gorivo", values_from = "gorivo") %>%
   select(-`NA`) %>%
   mutate_at(vars(gorivo), ~if_else(is.na(.), 0, 1))

menjalnik_wide <- 
   single %>%
   mutate(menjalnik = str_replace_all(tolower(menjalnik), " ", "_")) %>%
   pivot_wider(id_cols = "id_ad", names_from = "menjalnik", values_from = "menjalnik") %>%
   mutate_at(vars(menjalnik), ~if_else(is.na(.), 0, 1))

single %<>%
   select(-gorivo, -menjalnik) %>%
   left_join(gorivo_wide, by = "id_ad") %>%
   left_join(menjalnik_wide, by = "id_ad")

toc()

##### _Multi value parameters ####
tic("wide multi parameters")

multi <- 
   all_parameters %>% 
   filter(parameter_group == "multi") %>%
   mutate(name = str_replace_all(name, "[^[:alnum:]]", "_")) %>%
   mutate(name = str_replace_all(name, c("_{2,}" = "_", "š" = "s", "č" = "c", "ž" = "z"))) %>%
   mutate(attribute = str_replace_all(attribute, c("š" = "s", "č" = "c", "ž" = "z"))) %>%
   mutate(name = str_remove(name, "_$")) %>% 
   mutate(name = tolower(name)) %>%
   mutate(dummy_value_property = case_when(attribute == "multimedia" ~ NA_character_,
                                           value == "lahka - ALU platišča" ~ NA_character_,
                                           TRUE ~ value_property)) %>%
   mutate(dummy_value_property = str_replace_all(dummy_value_property, "[^[:alnum:]]", "_")) %>%
   mutate(dummy_value_property = str_replace_all(dummy_value_property, c("_{2,}" = "_", "š" = "s", "č" = "c", "ž" = "z"))) %>%
   mutate(dummy_value_property = str_remove(dummy_value_property, "_$")) %>% 
   mutate(dummy_value_property = tolower(dummy_value_property)) %>%
   unite(name, attribute, name, dummy_value_property, sep = "_", remove = FALSE, na.rm = TRUE) %>%
   mutate(dummy_value_property = 1)


multi %<>%
   group_by(id_ad) %>%
   filter(!(duplicated(name))) %>% ## odstranimo duplikate na drsnih vratah (2x vnešena že v oglasu)
   pivot_wider(id_cols = "id_ad", names_from = "name", values_from = "dummy_value_property")

#multi %<>%
#   mutate_all(~replace_na(., 0))

# Whole data
data_wide <- 
   left_join(single, multi, by = "id_ad")

toc()

#### _Save ####
write_csv2(data_wide, path = paste0("data_wide/", format(Sys.Date(), "%Y%m%d"), "_data_wide.csv"), col_names = TRUE)


toc() # data wide

toc() # total

