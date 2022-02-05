## Getting packages
library(tidyverse)
library(here)
library(lubridate)

## Getting the dat
country_ratings <- readxl::read_xlsx(path = "C:/Users/arthu/Downloads/Harris-School-of-PP/Winter quarter (2nd)/ap-psets/country_territory_ratings.xlsx", sheet = 2, col_names = T, skip = 1) %>% 
  .[,c(1, 68:ncol(.))]

## Pre-cleaning the data
col_names <- colnames(country_ratings)

possible_status <- str_squish(str_remove(unique(country_ratings$...70), c("Status|-")))[c(2,3,4)]
possible_values <- str_c(c(1:7, possible_status), collapse = "|")

## Cleaning data
cleaned_data <- country_ratings %>%
  rename(country = col_names[1]) %>% 
  gather(key, value, -country) %>%
  mutate(year = as.numeric(ifelse(str_detect(key, "\\..."), NA, key)),
         type = ifelse(!str_detect(value, "PR|CL|Status"), NA, value),
         value = ifelse(str_detect(value, possible_values), value, NA)) %>%
  fill(year, type) %>% 
  select(-key) %>%
  spread(type, value) %>% 
  filter(!is.na(country),
         !is.na(Status)) %>% 
  mutate(x_scale = floor(year/5)*5)

write_csv(x = cleaned_data, file = "data/tidy-data-fh.csv", col_names = TRUE)

## Obtaining UN data
data <- read.csv("C:/Users/arthu/Downloads/Harris-School-of-PP/Winter quarter (2nd)/ap-psets/un-data.csv")

new_names <- str_replace_all(colnames(data), pattern = "\\.", replacement = "_")

new_names[1] <- "Global_Code"
new_names[13] <- "Least_Developed_Countries_LDC"
new_names[14] <- "Land_Locked_Developing_Countries_LLDC"
new_names[15] <- "Small_Island_Developing_States_SIDS"

data_un <- setNames(object = data, nm = new_names)

write_csv(x = data_un, file = "data/tidy-data-un.csv", col_names = TRUE)
