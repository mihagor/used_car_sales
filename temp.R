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


##### MDM - Multi value parameters  #######################################################################################################################################################
### Multi new ad value parameters
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





### Multi posted ads value parameters

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



### MDM - union of parameters from posted ads and make new ads 

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

last_mdm <- max(list.files(path = paste0(union_multi_parameters_path, "joined_manually/"), pattern = "*.csv"))
last_mdm <- read_delim(file = paste0(union_multi_parameters_path, "joined_manually/", last_mdm), delim = ";")


napacni_id <-
   last_mdm %>% 
   mutate(id = if_else(id_attr.x != 999, id_attr.x, id_attr.y)) %>% 
   count(id) %>%
   filter(n > 2) 

message("Fix next ids:")   
napacni_id %>% glue::glue_data("\tid {id} -> count: {n}\n\r") %>% message(.)


## Export multi parameters

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



### Multi parameters in ads 

scraped_car_ad_ids <- last_car_ads_table %>% select(id_ad) %>% distinct()

all_multi_parameters <- 
   scraped_car_ad_ids %>% 
   crossing(last_mdm)

last_mdm


last_mdm %>% filter(name != "opomba") %>%
   filter(#str_detect(value, "zračna vreča / Airbag") |
      #name == "notranjost" & str_detect(value, "\\/|usnje") |
      str_detect(value, "klimatska naprava")
   )  %>% distinct(value)


last_car_ads_table %>%
   filter(str_detect(value, "klimatska naprava"))


#####  adhoc #####



## ONE HOT ENCODING -- factors to booleans 1 or 0
test <- 
   all_parameters %>% 
   filter(parameter_group == "multi" & attribute == "podvozje") %>%
   mutate_at(vars(attribute, name), ~as_factor(.)) %>%
   select(-parameter_group, -value_property, -value)
test <- as.data.table(test)   

one_hot(test)

temp_ads <- all_parameters %>% distinct(id_ad) %>% filter(row_number() < 5000) %>% pull(1) 
test <- all_parameters %>% group_by(id_ad) %>% filter(id_ad %in% temp_ads)

test <- test %>% filter(parameter_group == "single") %>% pivot_wider(id_cols = "id_ad", names_from = "name", values_from = "value")
test %>% filter(cena < 15000 & prevozenih_km < 130000) %>% View

keras::text_one_hot(test %>% ungroup %>% select(value), 5)



choosen_models <-
   all_parameters %>%  
   filter(name == "model" & value %in% c("Passat")) %>% #, "Talisman", "Superb", "Passat", "Berlingo", "Laguna", "Golf", "Mazda3"
   pull(1)

choosen_parameters <- c("model", "prva_registracija", "prevozenih_km", "cena", "datum_uvoza")

# all_parameters %>%
#    filter(id_ad %in% choosen_models & name %in% choosen_parameters) %>%
#    pivot_wider(id_cols = "id_ad", names_from = "name", values_from = "value") %>%
#    mutate_at(vars(cena, prevozenih_km), ~as.numeric(.)) %>%
#    mutate(prva_registracija = as.Date(prva_registracija)) %>%
#    group_by(model, prva_registracija_y = year(prva_registracija)) %>%
#    summarise(max_cena = max(cena, na.rm = TRUE), 
#              mean_cena = mean(cena, na.rm = TRUE), 
#              median_cena = median(cena, na.rm = TRUE), 
#              min_cena = min(cena, na.rm = TRUE),
#              max_prevozenih_km = max(prevozenih_km, na.rm = TRUE), 
#              mean_prevozenih_km = mean(prevozenih_km, na.rm = TRUE), 
#              median_prevozenih_km = median(prevozenih_km, na.rm = TRUE), 
#              min_prevozenih_km = min(prevozenih_km, na.rm = TRUE)
#    ) %>%
#    mutate_if(is.numeric, round)




choose <- 
   all_parameters %>%
   filter(id_ad %in% choosen_models & name %in% choosen_parameters) %>% # 
   pivot_wider(id_cols = "id_ad", names_from = "name", values_from = "value") %>%
   mutate_at(vars(cena, prevozenih_km), ~as.numeric(.)) %>%
   mutate(prva_registracija = as.Date(prva_registracija))
View(choose)   
## time series analysis
# ggplot(choose, aes(x = prva_registracija, y = cena)) +
#    geom_line(size = 0.75) +
#    scale_x_date(limits = c(as.Date("2010-01-01"), as.Date("2020-12-31"))) +
#    stat_smooth(method = "lm", color = "#FC4E07", fill = "#FC4E07") +
#    facet_grid(~model)






box_plot <- 
   choose %>% 
   filter(year(prva_registracija) > 2010 & !is.na(prva_registracija)) %>% 
   mutate(model = if_else(model == "Laguna", "Talisman", model))



p <- 
   ggplot(box_plot, aes(x = model, y = cena, color = model)) +
   geom_boxplot() +
   facet_grid(~year(prva_registracija)) +
   theme(axis.text.x = element_blank(),
         axis.ticks.x = element_blank())

ggplotly(p)


box_plot %>%
   group_by(prva_registracija = year(prva_registracija)) %>%
   count(model) %>%
   pivot_wider(id_cols = "prva_registracija", names_from = "model", values_from = "n")