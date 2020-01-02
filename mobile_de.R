library(httr)

username <- 'mihagornik3@gmail.com' # fake email
password <- '76P0paFnGW5t' # fake password

set_config(authenticate(username,password), override = TRUE)

GET("https://services.mobile.de/search-api/search?", authenticate(username,password))
