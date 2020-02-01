## extract single and multi parameters from:
##     scraped car ads (2_web_scraping.R) and 
##     merged parameters table (6_merge_parameters.R)

# zadnji file z oglasi in search parametri 
car_ads_tables <- list.files(path = "scraped_car_ads/", pattern = "*.csv")
message("Last car ads table: ", interval(ymd(str_sub(max(car_ads_tables), 1, 8)), as_date(now())) %/% days(1), " days ago.")
last_car_ads_table <- read_csv2(paste0("scraped_car_ads/", max(car_ads_tables)))

merged_parameters_tables <- list.files(path = "merged_parameters/", pattern = "*.csv")
message("Last merged parameters table: ", interval(ymd(str_sub(max(merged_parameters_tables), 1, 8)), as_date(now())) %/% days(1), " days ago.")
last_merged_parameters_table <- read_csv2(paste0("merged_parameters/", max(merged_parameters_tables)))


##### Initial data manipulation #####

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
      pivot_wider(id_cols = id_ad, names_from = name, values_from = value) %>% 
      select(id_ad, single_parameters)

single_value_parameters %<>% 
   mutate(letnik_proizvodnje = parse_date_time(letnik_proizvodnje, "Y")) %>%
   mutate(datum_uvoza = ymd(datum_uvoza))


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




##### Multi new ad value parameters #####
message("Multi parametri: ",paste(last_car_ads_table %>% distinct(name) %>% filter(!(name %in% single_parameters)) %>% as_vector(), collapse = ", "))

## Parameters from make new ad 
multiple_parameters_list <- 
   last_merged_parameters_table %>% 
      filter(!(attribute %in% c("znamka", "model")))

multiple_parameters_list %<>% 
   arrange(attribute) %>% 
   rownames_to_column(var = "id_attr") %>%
   mutate(id_attr = as.numeric(id_attr)) %>%
   mutate(name = str_to_lower(name))

multiple_parameters_list %>%
   write_csv2(paste0("mdm/multiple_parameters_list/", format(Sys.Date(), "%Y%m%d"), "_multiple_parameters_list.csv"), col_names = TRUE)





##### Multi posted ads value parameters #####

## Parameters from posted ads
multi_value_parameters_import <- 
   last_car_ads_table %>% 
      filter(!(name %in% single_parameters)) %>%
      arrange(id_ad, name, attribute) %>% 
      filter(name != "opomba")


# seznam numeric in string parameters, where users could input own data in text format
numeric_values <- c("štev. sedežev",       
                    "sedeži",              
                    "klimatska naprava",   
                    "pomoč pri parkiranju",
                    "volan",               
                    "zunanja ogledala")

string_values <- 
   suppressWarnings(                          # warningi samo zaradi separate, ker ni vrednosti v vseh vrsticah
      multi_value_parameters_import %>% 
      select(-id_ad, -attribute) %>% 
      separate(value, into = c("value_name", "value_string"), sep = ":") %>% 
      filter(!is.na(value_string)) %>%
      select(-value_string) %>% 
      distinct() %>%
      filter(!(value_name %in% numeric_values)) %>%
      pull(2)
   )

num_str_values <- c(numeric_values, string_values)


multi_value_parameters_import %<>%
   select(name, value) %>%
   filter(!str_detect(value, paste(num_str_values, collapse = "|"))) %>% 
   distinct() %>%
   arrange(name) %>%
   rownames_to_column(var = "id_attr") %>%
   mutate(id_attr = as.numeric(id_attr) * 1000)


multi_value_parameters_import %>%
   write_csv2(paste0("mdm/multi_value_parameters_import/", format(Sys.Date(), "%Y%m%d"), "_multi_value_parameters_import.csv"), col_names = TRUE)



##### MDM - union of parameters from posted ads and make new ads #####

mdm_multi_parameters <-
   full_join(multi_value_parameters_import, multiple_parameters_list, by = "value", keep = TRUE) %>%
   arrange(name.x, attribute, value) %>%
   select(-attribute)
   

union_multi_parameters_path <- "mdm/union_multi_parameters/"

mdm_multi_parameters %>%
   write_csv2(paste0(union_multi_parameters_path, format(Sys.Date(), "%Y%m%d"), "_mdm_multi_parameters.csv"), col_names = TRUE)


## Checking for new parameters
union_multi_parameters <- list.files(path = union_multi_parameters_path, pattern = "*.csv")

last_union_multi_parameters <- read_csv2(paste0(union_multi_parameters_path, nth(union_multi_parameters, 1, order_by = desc(union_multi_parameters))))
previous_union_multi_parameters <- read_csv2(paste0(union_multi_parameters_path, nth(union_multi_parameters, 2, order_by = desc(union_multi_parameters))))      

## New parameters
new_multi_param <- last_union_multi_parameters %>% anti_join(previous_union_multi_parameters, by = 'value')
message(new_multi_param %>% glue::glue_data("New parameters\nParameter: {name.x} -> Vrednost: {value}\n\r"))



## Import last MDM

last_mdm <- max(list.files(path = "mdm/", pattern = "*.csv"))
last_mdm <- read_delim(file = paste0("mdm/", last_mdm), delim = ";")


napacni_id <-
   last_mdm %>% 
      mutate(id = if_else(id_attr.x != 999, id_attr.x, id_attr.y)) %>% 
      count(id) %>%
      filter(n > 2) 

message("Fix next ids:")   
napacni_id %>% glue::glue_data("\tid {id} -> count: {n}\n\r") %>% message(.)


## Export multi parameters

## 154 multiple attributes
last_mdm %<>% 
   mutate(id = if_else(id_attr.x != 999, id_attr.x, id_attr.y)) %>%
   mutate(src = if_else(is.na(name.x), "posted_ad", "new_ad")) %>%
   mutate(name.x = case_when(name.x == "info-multimedia" | name.y == "info-multimedia" ~ "multimedia",
                             is.na(name.x) ~ name.y, 
                             TRUE ~name.x)) %>%
   select(id, name = name.x, value, src) %>%
   filter(!str_detect(value, "zračna vreča / Airbag")) %>%
   filter(!(name == "notranjost" & str_detect(value, "\\/|usnje"))) %>%
   arrange(id, desc(src), name, value) %>% 
   filter(!duplicated(id))


last_mdm %>% 
   write_csv2(paste0("mdm/", format(Sys.Date(), "%Y%m%d"), "_mdm.csv"), col_names = TRUE)




scraped_car_ad_ids <- last_car_ads_table %>% select(id_ad) %>% distinct()

scraped_car_ad_ids %>% cross_df(last_mdm)
