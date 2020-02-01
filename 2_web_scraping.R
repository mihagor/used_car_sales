here::here()

# avto.net apliakcija za spremljanje cene karavanov in enoprostorcev
start_time <- Sys.time()
message("Starting avto.net web scraping ... ", start_time)
message("Loading packages ...")
library(pacman)
p_load(rvest, jsonlite, tidyverse, xopen, polite, robotstxt, curl, httr, magrittr, lubridate)

message("Working directory ...")
if (getwd() != "/Users/mihagornik/projects/avto_net/used_car_sales") {setwd("/Users/mihagornik/projects/avto_net/used_car_sales")}
getwd()
# No valid robots
# rt <- robotstxt(domain = "avto.net")


# oblika 13 = karavan
# oblika 14 = enoprostorec
# %2C = URL escape za presledek, 
# 17.12.2019 - 2657 zadetkov
# samo 21 page-ov prikaže naenkrat - scraping delamo na 20 pagih
page <- c(1:20)

# web scraping card ad ID in last 20 pages 
get_page_ad_ids <- function(oblika, ...) {
  
  page <- page
  
  scraped_ids <- c()
  
  for (i in page) {
    
    get_request <- 
      GET(url = "https://www.avto.net/Ads/results.asp?",
        query = list(cenaMin	     = 	'0', 
                     cenaMax       = 	'999999',
                     letnikMin	   = 	'0',
                     letnikMax	   = 	'2090',
                     bencin	       = 	'0',
                     starost2	     = 	'999',
                     oblika	       = 	oblika,
                     ccmMin	       = 	'0',
                     ccmMax	       = 	'99999',
                     kmMin	       = 	'0',
                     kmMax	       = 	'9999999',
                     kwMin	       = 	'0',
                     kwMax	       = 	'999',
                     lokacija	     = 	'0',
                     EQ1	         = 	'1000000000',
                     EQ2	         = 	'1000000000',
                     EQ3	         = 	'1000000000',
                     EQ4	         = 	'100000000',
                     EQ5	         = 	'1000000000',
                     EQ6	         = 	'1000000000',
                     EQ7	         = 	'1110100120',
                     EQ8	         = 	'1010000001',
                     EQ9	         = 	'1000000000',
                     KAT	         = 	'1010000000',
                     paketgarancije = 	'0',
                     zaloga	       = '10',
                     stran	       = 	i
        )
    )
    message(get_request$url)
    stop_for_status(get_request)
    
    ex_page <- content(get_request, encoding = "windows-1250")
    num_ads <- length(ex_page %>% html_nodes(".Adlink"))
    
    page_ids <- 
              ex_page %>% 
              html_nodes(".Adlink") %>% 
              html_attr("href") %>%
              str_extract("(?<=id\\=)[0-9]{8}(?=&display)") # pred digiti mora biti 'id=', za njimi pa '&display'
              
    if (length(page_ids) != num_ads) {
              stop("Number of extracted ids is not equal to number of ads!") 
    }
    
    scraped_ids <- c(scraped_ids, page_ids)
    
    if (i %% 10 == 0) {
              sleep_time <- 10
    } else {
              sleep_time <- 5
    }
    message(paste("Completed:", round(i/max(page) * 100, 0), "%", "\nIteration number:", i, "\nSleeping for", sleep_time, "seconds"))
    Sys.sleep(sleep_time)
  } 

  return(scraped_ids)
}

message("Scraping page ids ...")
ad_ids_karavan <- get_page_ad_ids("13")
ad_ids_enopros <- get_page_ad_ids("14")

combined_scraped_ids <- c(ad_ids_karavan, ad_ids_enopros)
new_scraped_car_ids <- combined_scraped_ids %>% unique(.)

message("Getting old ad ids ...")
old_scraped_car_ids <- read_csv2(paste0("scraped_car_ids/", max(list.files(path = "scraped_car_ids/", pattern = "*.csv")))) 
old_scraped_car_ids %<>% pull(value)

# all new car ads for further web scraping
new_car_ads <- new_scraped_car_ids[!(new_scraped_car_ids %in% old_scraped_car_ids)]

car_ad_data <- tibble()
counter <- 1

message(paste0("Number of new car ads: ", length(new_car_ads)))
message("Starting to scrape new car ads ...")
# web scraping car ad DATA for new ads
for (i in new_car_ads) {
          
  car_ad <- read_html(paste0("https://www.avto.net/Ads/details.asp?id=", i)) 

  if(length(car_ad %>% html_nodes(".OglasDataTitle")) == 0) {
    next
    
  } else {
    
    # Cena
    car_price <- car_ad %>% html_nodes(".OglasDataCenaTOP")
    if (length(car_price) > 0) {
      car_price <- car_price %>% html_text() %>% str_extract_all("[0-9]") %>% unlist() %>% paste0(collapse = "")
    } else {
      car_price <- car_ad %>% html_nodes(".OglasDataAkcijaCena") %>% html_text() %>% str_extract_all("[0-9]") %>% unlist() %>% paste0(collapse = "")    
    }
    names(car_price) <- "Cena"
    car_price <- enframe(car_price) %>% mutate(attribute = "value1") %>% select(name, attribute, value)
    
    
    # Naziv
    car_name_node <- car_ad %>% html_nodes(".OglasDataTitle")
    car_name <- car_name_node %>% as.character() %>% str_extract(("(?<=\\<h1\\>).*(?=\\<small\\>)")) %>% str_squish()
    car_name_full <- car_name_node %>% html_text() %>% str_squish()
    names(car_name) <- "Kratek naziv avtomobila"
    names(car_name_full) <- "Dolg naziv avtomobila"
    car_names <- enframe(c(car_name, car_name_full)) %>% mutate(attribute = "value1") %>% select(name, attribute, value)
    
    
    # Osnovni podatki
    car_ad_data_name <- car_ad %>% html_nodes(".OglasDataLeft") %>% html_text()
    car_ad_data_value <- car_ad %>% html_nodes(".OglasDataRight") %>% html_text()
    
    car_data <- bind_cols(name = car_ad_data_name, value = car_ad_data_value)
    car_data %<>% 
      mutate(value = str_squish(str_remove(value, "\r|\n|\t"))) %>%
      mutate(name = str_remove(name, ":")) %>%
      mutate(attribute = "value1") %>%
      select(name, attribute, value)
    
    
    # Podrobni podatki
    car_ad_properties_name <- car_ad %>% html_nodes(".OglasEQLeft") %>% html_text() %>% str_remove(":")
    car_ad_properties_value <- car_ad %>% html_nodes(".OglasEQRightWrapper") %>% as.character() 
    car_ad_properties <- bind_cols(name = car_ad_properties_name, value = car_ad_properties_value)
    
    suppressWarnings( # separate vrne error, kadar ni 30 vrednosti atributov
      car_ad_properties %<>% 
        mutate(value = str_squish(str_remove_all(value, '\r|\n|\t|<div class=\"OglasEQRightWrapper\">|</div>'))) %>% 
        separate(value, paste0("value", 0:30), sep = '<div class=\"OglasEQRight\">- ') %>% 
        mutate_all(str_trim) %>% 
        select(-value0) %>%
        pivot_longer(-name, names_to = "attribute", values_to = "value") %>% 
        filter(!is.na(value))
    )
    
    
    # Opomba
    car_ad_comment <- car_ad %>% html_nodes(".OglasEQtext")
    if (length(car_ad_comment) > 0) {
      car_ad_comment <- car_ad_comment %>% html_text() %>% str_remove_all("\r|\n|\t") %>% str_squish() 
    } else {
      car_ad_comment <- NA    
    }
    
    names(car_ad_comment) <- "Opomba"
    car_ad_comment <- enframe(car_ad_comment) %>% mutate(attribute = "value1") %>% select(name, attribute, value)
    
    scrape_date <- tibble(name = "Datum uvoza", attribute = "value1", value = format(now(), "%Y%m%d"))
    
    # final loop data
    car_ad_data_temp <- bind_rows(car_names, car_price, car_data, car_ad_properties, car_ad_comment, scrape_date) %>% mutate(id_ad = i) %>% select(id_ad, everything())
    
    car_ad_data <- bind_rows(car_ad_data, car_ad_data_temp)
    
  }
    
  
  sleep_time <- runif(1, min = 3, max = 17)
  message(paste("Completed:", round(counter/length(new_car_ads) * 100, 2), "%", "\nIteration number:", counter, "\nSleeping for", round(sleep_time, 0), "seconds"))
  Sys.sleep(sleep_time)
  
  counter <- counter + 1
}

message("Finished scraping car ads ...")

 # archive scraped car ad IDs
new_scraped_car_ad_ids <- car_ad_data %>% select(id_ad) %>% distinct() %>% pull()
all_scraped_car_ids <- c(old_scraped_car_ids, new_scraped_car_ad_ids)
all_scraped_car_ids %<>% unique(.)

message("Exporting car ads ids ...")
write_csv2(enframe(all_scraped_car_ids), path = paste0("scraped_car_ids/", format(Sys.Date(), "%Y%m%d"), "_scraped_car_ads.csv"), col_names = TRUE) 

# archive car ad data
old_scraped_car_ads <- read_csv2(paste0("scraped_car_ads/", max(list.files(path = "scraped_car_ads/", pattern = "*.csv"))))

car_ad_data %<>% mutate(id_ad = as.double(id_ad))
all_scraped_car_ads <- bind_rows(old_scraped_car_ads, car_ad_data)
all_scraped_car_ads %<>% distinct()

message("Exporting car ad data ...")
write_csv2(all_scraped_car_ads, path = paste0("scraped_car_ads/", format(Sys.Date(), "%Y%m%d"), "_scraped_car_ad_data.csv"), col_names = TRUE)

finish_time <- Sys.time()
message(glue::glue("Finished avto.net web scraping ... , {finish_time}\n Job took: {round(difftime(finish_time, start_time, units = 'min'))} minutes"))

