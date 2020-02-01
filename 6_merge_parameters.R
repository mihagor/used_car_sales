# merge parameters from search and making new ad

# search parameters
parameters_tables <- list.files(path = "scraped_parameters/", pattern = "*.csv")
message("Last search parameters table from ", interval(ymd(str_sub(max(parameters_tables), 1, 8)), as_date(now())) %/% days(1), " days ago.")
last_search_parameters_table <- read_csv2(paste0("scraped_parameters/", max(parameters_tables)))


new_ad_parameters_tables <- list.files(path = "manual_add_new_ad/parameters/", pattern = "*.csv")
message("Last new car ad parameters table: ", interval(ymd(str_sub(max(new_ad_parameters_tables), 1, 8)), as_date(now())) %/% days(1), " days ago.")
last_new_ad_parameters_table <- read_csv2(paste0("manual_add_new_ad/parameters/", max(new_ad_parameters_tables)))



all_parameters <- 
      last_search_parameters_table %>% 
            filter(attribute %in% c("Znamka", "Model")) %>%   # izločimo ostale parametre (letnik, ceno, ...
            bind_rows(last_new_ad_parameters_table)

all_parameters %<>%
      mutate(attribute = str_to_lower(attribute))
      

write_csv2(all_parameters, path = paste0("merged_parameters/", format(Sys.Date(), "%Y%m%d"), "_all_parameters.csv"), col_names = TRUE)

