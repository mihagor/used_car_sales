# avto.net car ad parameters for dimensions
here::here()

##### META SECTION #####

start_time <- Sys.time()
message("Starting avto.net car ad parameters web scraping ... ", start_time)
message("Loading packages ...")
library(pacman)
p_load(rvest, jsonlite, tidyverse, xopen, polite, robotstxt, curl, httr, magrittr, lubridate, cronR, shinyFiles, RCurl)

message("Working directory ...")
if (getwd() != "/Users/mihagornik/projects/avto_net/used_car_sales") {setwd("/Users/mihagornik/projects/avto_net/used_car_sales")}
getwd()

add_new_ad_tables <- list.files(path = "manual_add_new_ad/", pattern = "*.html")
message("Last car ads table: ", max(add_new_ad_tables))
last_add_new_ad_table <- read_html(paste0("manual_add_new_ad/", max(add_new_ad_tables)), encoding = "UTF-8")

# vnosni parametri


tables <- last_add_new_ad_table %>% 
      html_nodes("div.InsertAdBox") %>% 
      html_children() 



tables %>% xml_find_all(".//br") %>% xml_add_sibling("p", "\n") 
tables %>% xml_find_all(".//br") %>% xml_remove()

tables %<>% 
      html_text() %>% 
      as_tibble() %>% 
      separate(value, c("attribute", paste0("value", 1:55)), sep = "\n", fill = "right") %>% 
      mutate_all(~str_squish(.)) %>%
      mutate_all(~na_if(., "")) %>%
      pivot_longer(cols = starts_with("value")) %>%
      mutate(attribute = str_remove_all(attribute, ":")) %>%
      filter(value != attribute & !is.na(value) & !is.na(attribute)) %>%
      filter(attribute %in% c("Podvozje", "Varnost", "Notranjost", "Udobje", "Info-Multimedia", "Uporabnost"))
      
tables %<>% 
      group_by(attribute) %>% 
      mutate(name = paste0("value", seq_along(attribute))) %>%
      mutate(value = str_remove(value, ":$"))


write_csv2(tables, path = paste0("manual_add_new_ad/parameters/", format(Sys.Date(), "%Y%m%d"), "_new_ad_parameters.csv"), col_names = TRUE)


message("Izvoženi parametri za: ", paste(unique(tables$attribute), collapse = ", "))

