
### Twitter API setup ###

# load twitter library 
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)


get_timeline(user = 'alexandrekalil', n = 50)



## project's app name:
app_name <- 'harris-student'

## api key (example below is not a real key)
key <- 'niHqBfN2Pmf0WNwJAgPrnXqGe'

## api secret (example below is not a real key)
secret <- 'LAhdWN6Fg3YfH5Gi6vQeo36smg7BW6OzwB5RRHBuhlqPD7gcB0'

# create token named "twt-harris"
twt_harris <- create_token(
  app = app_name,
  consumer_key = key,
  consumer_secret = secret,
  access_token = '1351351867335462913-MD6Jp3bVnHU590qLgtSCDLqZ8xGt1D',
  access_secret = 'LJp0vG0hCd1cFKwP5EF4yIcZxlXxIMcRfflU1fsIHuiXJ')



