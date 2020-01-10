here::here()
p_load(tidyverse, magrittr, lubridate, plotly)

# Zadnji file z oglasi in search parametri 
car_ads_tables <- list.files(path = "scraped_car_ads/", pattern = "*.csv")
message("Last car ads table: ", max(car_ads_tables))
last_car_ads_table <- read_csv2(paste0("scraped_car_ads/", max(car_ads_tables)))

parameters_tables <- list.files(path = "scraped_parameters/", pattern = "*.csv")
message("Last car ads table: ", max(parameters_tables))
last_parameters_table <- read_csv2(paste0("scraped_parameters/", max(parameters_tables)))


last_car_ads_table$name %>% unique()



last_car_ads_table %>% filter(id_ad == last_car_ads_table$id_ad[1]) %>% print(n = 100)



























test <- car_ad_data %>% filter(row_number() < 501)

car_attibutes <- test %>% select(name) %>% distinct() %>% filter(row_number() < 11)


test2 <- car_ad_data %>% 
      filter(name %in% as_vector(car_attibutes)) %>%
      select(-attribute) %>%
      pivot_wider(id_cols = id_ad, names_from = name, values_from = value)



car_ads <- test2 %>%
      separate(`Kratek naziv avtomobila`, c("Znamka", "Model"), " ", extra = "merge") %>% 
      mutate(Model = str_remove(Model, ":"))


car_ads %<>% 
      filter(Znamka %in% c("Škoda", "Citroen") & Model %in% c("Octavia", "Superb", "Berlingo")) %>% 
      mutate(`1. registracija` = str_sub(`1. registracija`, -4, -1)) %>%
      select(id_ad:Model, Cena, `1. registracija`, `Prevoženih km`, Motor, Menjalnik)

car_ads %<>% mutate(Cena = as.numeric(Cena), `1. registracija` = as.numeric(`1. registracija`), `Prevoženih km` = as.numeric(`Prevoženih km`))


p <- 
car_ads %>% 
      ggplot(aes(x =`1. registracija`, y = Cena, colour = Model)) +
      geom_point()

