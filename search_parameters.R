# avto.net search parameters for dimensions

##### META SECTION #####

start_time <- Sys.time()
message("Starting avto.net parameters web scraping ... ", start_time)
message("Loading packages ...")
library(pacman)
p_load(rvest, jsonlite, tidyverse, xopen, polite, robotstxt, curl, httr, magrittr, lubridate, cronR, shinyFiles)

message("Working directory ...")
if (getwd() != "/Users/mihagornik/projects/avto_net/used_car_sales") {setwd("/Users/mihagornik/projects/avto_net/used_car_sales")}
getwd()




##### URL SECTION #####
url_advanced_search <- "https://www.avto.net/Ads/search.asp?SID=10000"

message("Checking connection ...")
# check url connection
stopifnot(http_status(GET(url_advanced_search))$category == "Success")

ex_page <- read_html(url_advanced_search)

# html form
params <- ex_page %>% html_nodes(".Form195") %>% html_children()

# vnosni parametri
param_names <- params %>% html_attr("name")
param_names <- str_remove(param_names, "\\d")


# parametri in možne vrednosti 
params %<>% 
      map(html_children) %>%                    # vnosna maska (html form)
      set_names(param_names) %>%                # poimenujemo list z imeni vnosnega parametra
      map(~html_text(., trim = TRUE)) %>%       # pretvorimo xml v text
      imap_dfr(~tibble(id = .y, value = .x))    # imap potegne še name lista, ki ga stlačimo v id

# odstranimo dvojnike, ki jih dobimo iz parametrov izbire 2., 3. znamke
params %<>% distinct()




