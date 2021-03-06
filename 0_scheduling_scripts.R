library(cronR, shinyFiles)

f <- file.path("/Users/mihagornik/projects/avto_net/used_car_sales/2_web_scraping.R")
cmd <- cron_rscript(f)

## Every day at 22:01
cron_add(cmd, frequency = 'daily', id = 'job1', description = "Web scraping avto.net for new car ads.", at = '03:00')


## Get all the jobs
cron_ls()

## Remove 1 scheduled job
cron_rm(id = "job2")

## Remove all scheduled jobs
cron_clear(ask = FALSE)