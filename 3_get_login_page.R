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


html <- read_html("manual_add_new_ad/20200103_new_ad.html")










##### URL SECTION #####
url_avtonet_login <- "https://www.avto.net/_2016mojavtonet/"

message("Checking connection ...")
# check url connection
stopifnot(http_status(GET(url_avtonet_login))$category == "Success")

handle_reset(handle)


login <- list(enaslov = "mihagornik3@gmail.com",
              geslo = "MotorolaV3",
              pravnoobvestilo = "1",
              LOGIN = "1")



response <- POST("https://www.avto.net/_2016mojavtonet/index.asp", 
                encode="form",
                body = login, 
                verbose(), 
                add_headers(`Accept` =          "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
                            `Accept-Encoding` = "gzip, deflate, br",
                            `Accept-Language` = "en-GB,en-US;q=0.9,en;q=0.8",
                            `Cache-Control` =   "max-age=0",
                            `Connection` =      "keep-alive",
                            `Content-Length` =  "74",
                            `Content-Type` =    "application/x-www-form-urlencoded",
                            `DNT` =             "1",
                            `Host` =            "www.avto.net",
                            `Origin` =          "https =//www.avto.net",
                            `Referer` =         "https =//www.avto.net/_2016mojavtonet/",
                            `Sec-Fetch-Mode` =  "navigate",
                            `Sec-Fetch-Site` =  "same-origin",
                            `Sec-Fetch-User` =  "?1",
                            `Upgrade-Insecure-Requests` = "1",
                            `User-Agent` =      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.94 Safari/537.36")
            )




content(response, "text", encoding = "windows-1250")

