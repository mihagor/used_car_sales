p_load(ggthemes, extrafont, showtext, markdown, rmarkdown)
p_load(rvest, jsonlite, tidyverse, xopen, polite, robotstxt, curl, httr, magrittr, lubridate, mailR, rmarkdown)

if (getwd() != "/Users/mihagornik/projects/avto_net/used_car_sales") {setwd("/Users/mihagornik/projects/avto_net/used_car_sales")}
getwd()

# font_import("Courier", prompt = FALSE)
font_add("Fira Code", 
         regular = "FiraCode-Retina.otf", 
         bold = "FiraCode-Bold.otf", 
         italic = "FiraCode-Light.otf")
showtext_auto()
#View(font_files())

##### Time series analysis #####
scraped_ads_day <- 
  all_scraped_car_ads %>% 
  filter(name == "Datum uvoza") %>%
  select(id_ad, value) %>% 
  transmute(day = as.Date(value, format = "%Y%m%d")) %>%
  group_by(day) %>%
  summarise(cnt_day = n()) %>%
  mutate(diff_between_days = cnt_day - lag(cnt_day, 1)) %>%
  mutate(diff_between_days_pct = diff_between_days / cnt_day)

scraped_ads_last_14_days <- 
  scraped_ads_day %>% 
  filter(between(day, Sys.Date() - ddays(14), Sys.Date()))

ggplot(scraped_ads_last_14_days, aes(x = day, y = cnt_day)) +
  geom_line(size = 1, colour = "#A4A4A4") +
  geom_point(size = 2, colour = "#5B5B5B") +
  geom_label(data = scraped_ads_last_14_days, aes(label = cnt_day), hjust = "inward", vjust = "outward", family = "Fira Code", size = 3) +
  scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Days",
       y = "Number of scraped ads",
       title = "Number of scraped ads in last 14 days",
       subtitle = "Per day") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "Fira Code"))

ggsave(file.path("dq_daily", "pictures", paste0(format(Sys.Date(), '%Y%m%d'), "_last_14days")), device = "png")


ggplot(scraped_ads_day, aes(x = day, y = cnt_day)) +
  geom_line(size = 1, colour = "#A4A4A4") +
  geom_point(size = 2, colour = "#5B5B5B") +
  geom_label(data = scraped_ads_day %>% filter(abs(diff_between_days_pct) > 1), aes(label = format(day, "%a")), hjust = "inward", vjust = "outward", family = "Fira Code", size = 3) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b/%U") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Weeks",
       y = "Number of scraped ads",
       title = "Number of scraped ads",
       subtitle = "Per week") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "Fira Code"))

ggsave(file.path("dq_daily", "pictures", paste0(format(Sys.Date(), '%Y%m%d'), "_all_time")), device = "png")
        
##### Brand analysis #####
scraped_car_brands <- 
  all_scraped_car_ads %>% 
  filter(name == "Kratek naziv avtomobila") %>%
  separate(value, into = c("brand", "model"), sep = " ", extra = "merge", remove = FALSE) %>%
  mutate(model = str_remove(model, ":"),
         value = str_remove(value, ":")) %>%
  select(value, brand, model) %>%
  mutate(timeframe = "all time")

scraped_car_brands_last_14days <-
  all_scraped_car_ads %>% 
  filter(name == "Kratek naziv avtomobila") %>% 
  filter(between(row_number(), nrow(.) - sum(scraped_ads_last_14_days$cnt_day) + 1, nrow(.))) %>%
  separate(value, into = c("brand", "model"), sep = " ", extra = "merge", remove = FALSE) %>%
  mutate(model = str_remove(model, ":"),
         value = str_remove(value, ":")) %>%
  select(value, brand, model) %>%
  mutate(timeframe = "last 14 days")

scraped_car_brands_for_plot <- 
  scraped_car_brands %>%
  bind_rows(scraped_car_brands_last_14days) %>% 
  mutate(value = fct_lump_n(value, n = 20)) %>%
  group_by(timeframe) %>%
  count(value) %>%
  filter(value != "Other") %>%
  arrange(desc(n))
  
  
ggplot(scraped_car_brands_for_plot, aes(x = fct_reorder(value, n), y = n)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#A4A4A4") + 
  geom_text(aes(label = n), family = "Fira Code", size = 2, nudge_y = 500) +
  labs(x = "Cars",
       y = "Number of scraped ads",
       title = "Number of scraped ads",
       subtitle = "Per car brand & model") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(family = "Fira Code")) +
  coord_flip() +
  facet_grid(~ timeframe)

ggsave(file.path("dq_daily", "pictures", paste0(format(Sys.Date(), '%Y%m%d'), "_brand")), device = "png")