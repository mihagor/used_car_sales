#rm(list = ls())

# Zadnji file z oglasi in search parametri 
parameters_tables <- list.files(path = "data_wide/", pattern = "*.csv")
message("Last car ads table: ", max(parameters_tables))
all_parameters <- read_delim(paste0("data_wide/", max(parameters_tables)), 
                             delim = ";")

message("Število uvoženih oglasov: ", all_parameters %>% distinct(id_ad) %>% nrow)


#all_parameters %>% filter(id_ad == 14934978) %>% filter(name == "prevozenih_km") %>% mutate(value = as.numeric(value))


data_wide %>% arrange(desc(datum_uvoza)) %>% View

all_parameters %>% filter(name == "letnik_proizvodnje") %>% count(value)


### Analysing sample

models <- c("Passat", "Talisman", "Superb", "Passat", "Berlingo", "Laguna", "Golf", "Mazda3")
# models <- c("Talisman")

sample_data <-
   data_wide %>%
   filter(model %in% models) %>%
   # mutate(model = if_else(model == "Laguna", "Talisman", model)) %>%
   mutate(prva_registracija = year(prva_registracija)) %>%
   select(
      id_ad,
      znamka,
      model,
      cena,
      prva_registracija,
      prevozenih_km,
      datum_uvoza
      )

glimpse(sample_data)

empty_parameters <-
   sample_data %>%
   summarise_if( ~ is.numeric(.), ~ sum(., na.rm = TRUE)) %>%
   pivot_longer(id_ad:stevilo_vrat_2_vr) %>%
   filter(value < 11) %>%
   pull(1)

sample_data %<>%
   select(-!!empty_parameters)




##### VISUALIZATION 

data_basic <-
   sample_data %>%
   group_by(id_ad) %>%
   select(id_ad:motor_km)

## starost vozila glede na prevožene kilometre 
ggplot(data_basic, aes(prva_registracija, prevozenih_km)) +
   geom_hex(bins = 20) +
   scale_x_continuous(breaks = seq(1990, 2020, 2)) +
   scale_y_continuous(labels = scales::comma, breaks = seq(0, 500000, 50000))
   

data_violin <- 
   data_basic %>% 
   ungroup() %>%
   filter(prva_registracija > 2004) %>%
   mutate_at(vars(znamka, model, prva_registracija), ~as_factor(.))

## cena glede na starost avtomobila 
ggplot(data_violin, aes(x = prva_registracija, y = cena, color = model)) +
   geom_violin(scale = "width", trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) +
   geom_smooth(aes(group = model), method = "loess")


ggplotly(
ggplot(data_violin, aes(x = prva_registracija, y = cena, color = model)) +
   geom_boxplot()
)


data_scatter <-
   data_basic %>%
   filter(prevozenih_km > 10000)

## cena glede na število prevoženih kilometrov 
ggplot(data_scatter, aes(x = prevozenih_km, y = cena, color = model)) +
   geom_point() +
   scale_x_continuous(labels = scales::comma, limits = c(0, 450000)) +
   scale_y_continuous(limits = c(0, 40000)) +
   geom_smooth(aes(group = model), method = "lm")







##### REGRESSION
#### Priprava na regresijo
desc_stat <- psych::describeBy(sample_data, mat = FALSE, group = "model")


desc_stat %<>%  
   as.list.data.frame() %>%         # describBy S4 objekt pretvorimo najprej v list.df
   map(~as.data.frame(.)) %>%       # nato list.df v df
   map(~rownames_to_column(., "parameter")) %>%
   imap_dfr(~as_tibble(.), .id = "model")

desc_stat %<>% 
   select(-c(vars, trimmed, mad, range)) %>%
   filter(!(parameter %in% c("znamka*", "model*", "id_ad"))) %>%  
   filter(mean > 0.05) %>%                                        # izberemo parametre, ki jih ima vsaj 5 % avtomobilov
   pivot_wider(names_from = model, values_from = c(n:se))


## Povprečja in % posameznih funkcionalnosti
desc_stat %>%
   mutate_at(vars(starts_with("mean_")), ~round(., 2)) %>%
   print(n = 500) %>% 
   select(parameter, starts_with("mean_")) %>%
   View()



regression_data <- 
   sample_data %>%
   ungroup() %>%
   filter(prevozenih_km > 10000) %>%
   filter(prva_registracija > 2004) %>%
   select(model:prevozenih_km, motor_km:avtomatski_menjalnik)

regression_data <- as_tibble(regression_data)
regression_data_s <- scale(regression_data %>% select(cena:motor_km))
regression_data_s <- regression_data %>% select(-c(cena:motor_km)) %>% bind_cols(as_tibble(regression_data_s)) %>% select(model, cena:motor_km, diesel_motor:avtomatski_menjalnik)


enter_method_data <- 
   sample_data %>%
   ungroup() %>%
   filter(prevozenih_km > 10000) %>%
   filter(prva_registracija > 2004) 

enter_vars <- paste0(colnames(enter_method_data)[-c(1:12)], collapse = " + ")


## Korelacije med spremenljivkami

numeric_vars <- enter_method_data %>% select(cena:avtomatski_menjalnik)
dummy_vars <-  enter_method_data %>% select(podvozje_lahka_alu_platisca:last_col())
dummy_vars_sort <- dummy_vars[names(dummy_vars) %>% order()]

cor_all_vars <- cor(enter_method_data[, -c(1:3)], use = "pairwise.complete.obs", method = "pearson")
cor_dummy_vars_sort <- cor(dummy_vars_sort, use = "pairwise.complete.obs", method = "pearson")


cor_tibble_all_vars <- 
   cor_all_vars %>% 
   as.data.frame() %>% 
   rownames_to_column("var") %>% 
   as_tibble()


ggcorr(enter_method_data[, -c(1:3)],
       method = c("pairwise.complete.obs", "pearson"),
       nbreaks = 5 
)



d <- dist(cor_dummy_vars_sort)
hclust_d <- hclust(d)

cor_dummy_vars_sort_order <- cor_dummy_vars_sort[hclust_d$order, hclust_d$order]
ggcorr(data = NULL, cor_matrix = cor_dummy_vars_sort_order,
       method = c("pairwise.complete.obs", "pearson"),
       nbreaks = 5 
)



## spremenljivke za multikolinearnost. 
## te spremenljivke korelirajo z več kot 20 drugimi spremenljivkami

multi_corr_vars <- 
   cor_dummy_vars_sort_order %>%
   as.data.frame() %>% 
   rownames_to_column("var") %>% 
   as_tibble() %>%
   mutate_all(~case_when(. < -0.8 ~ -2,
                         . >= -0.8 & .< -0.3 ~ -1,
                         . >= -0.3 & . < 0.3 ~ 0,
                         . >= 0.3 & . < 0.8 ~ 1,
                         . >= 0.8 ~ 2,
                         TRUE ~ NA_real_)) %>%
   pivot_longer(cols = c(var:last_col())) %>%
   filter(name != "var") %>%
   group_by(name) %>%
   count(value) %>%
   filter(value != 0 & n > 20) %>%
   pull(1)



cor_tibble_all_vars %>% select(!!multi_corr_vars) %>% filter_all(any_vars(. > 0.3)) %>% View("cor_tibble_all_vars")

glm <-
   lm(
      formula = cena ~ model * (prva_registracija + prevozenih_km + motor_km + diesel_motor + avtomatski_menjalnik),
      data = regression_data,
     # family = "gaussian"
   )

glm_s <-
   lm(
      formula = cena ~ model * (prva_registracija + prevozenih_km + motor_km + diesel_motor + avtomatski_menjalnik),
      data = regression_data_s,
      # family = "gaussian"
   )

glm_enter <-
   lm(
      formula = as.formula(paste0("cena ~ prva_registracija + prevozenih_km + motor_km + diesel_motor + avtomatski_menjalnik + ", enter_vars)),
      data = enter_method_data,
      # family = "gaussian"
   )



enframe(glm$coefficients) %>%
   mutate(value = round(value, 3)) %>%
   print(n = 30)

enframe(glm_enter$coefficients) %>%
   mutate(value = round(value, 3)) %>%
   print(n = 300)



summary(glm_enter)

# Porazdelitev residualov
par(mfrow = c(2,2))
plot(glm)

predict(glm_enter, model = "Talisman", prva_registracija = 2017, prevozenih_km = 120000, motor_ccm = 1600, motor_km = 131)






