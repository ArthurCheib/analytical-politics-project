## Getting packages
library(tidyverse)
library(here)
library(lubridate)

### TASK 1 ###

### Get the dataset from Freedom House's website + clean it + upload to the cloud (GitHub = online repository for code)

## Getting the data
country_ratings <- readxl::read_xlsx(path = "C:/Users/arthu/Downloads/Harris-School-of-PP/Winter quarter (2nd)/ap-psets/country_territory_ratings.xlsx", sheet = 2, col_names = T, skip = 1) %>% 
  .[,c(1, 68:ncol(.))]

## Pre-cleaning the data
col_names <- colnames(country_ratings)

possible_status <- str_squish(str_remove(unique(country_ratings$...70), c("Status|-")))[c(2,3,4)]
possible_values <- str_c(c(1:7, possible_status), collapse = "|")

## Cleaning Freedom House's dataset
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

# ---------------------------------------------------------------------------------------------------------------

### TASK 2 ###

### Get the dataset from UN's website + clean it + upload to the cloud (GitHub = online repository for code)

## Cleaning UN data
data <- read.csv("C:/Users/arthu/Downloads/Harris-School-of-PP/Winter quarter (2nd)/ap-psets/un-data.csv")

new_names <- str_replace_all(colnames(data), pattern = "\\.", replacement = "_")

new_names[1] <- "Global_Code"
new_names[13] <- "Least_Developed_Countries_LDC"
new_names[14] <- "Land_Locked_Developing_Countries_LLDC"
new_names[15] <- "Small_Island_Developing_States_SIDS"

data_un <- setNames(object = data, nm = new_names)

write_csv(x = data_un, file = "data/tidy-data-un.csv", col_names = TRUE)

# ---------------------------------------------------------------------------------------------------------------

### TASK 3 ###

### Get the UN + Freedom House datasets from the cloud + clean them + joined them + save them in the cloud one last time

## Cleaning and combining FH + UN data
url_file_fh <- "https://raw.githubusercontent.com/ArthurCheib/analytical-politics-project/main/data/tidy-data-fh.csv"
url_file_un <- "https://raw.githubusercontent.com/ArthurCheib/analytical-politics-project/main/data/tidy-data-un.csv"

## Getting the data from Freedom House after having cleaned it
dataset <- read.csv(url_file_fh) %>% 
  as_tibble()

## Getting the data from United Nations after having cleaned it
data_un <- read.csv(url_file_un) %>% 
  as_tibble() %>%
  mutate(is_ldc = case_when(Least_Developed_Countries_LDC != "" ~ 1,
                            TRUE ~ 0)) %>% 
  select(Region_Code, Region_Name, Country_or_Area, is_ldc) %>% 
  arrange(Country_or_Area, is_ldc) %>% 
  group_by(Country_or_Area) %>% 
  slice(1) %>% 
  ungroup()

## Countries names in the dataset that are missing
names_missing <- dataset %>% 
  count(country) %>% 
  select(-n) %>% 
  left_join(data_un, by = c("country" = "Country_or_Area")) %>% 
  filter(is.na(Region_Name)) %>%
  pull(country)

## Correcting the names + removing from the data countries that don't appear in both 2005 and 2020 years.
dataset_fh_un <- dataset %>% 
  mutate(country = fct_recode(country,
                              "Bolivia (Plurinational State of)" = names_missing[1],
                              "Brunei Darussalam" = names_missing[2],
                              "Congo" = names_missing[3],
                              "Democratic Republic of the Congo" = names_missing[4],
                              "CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire" = names_missing[5],
                              "Czechia" = names_missing[6],
                              "Iran (Islamic Republic of)" = names_missing[7],
                              "Lao People's Democratic Republic" = names_missing[9],
                              "Micronesia (Federated States of)" = names_missing[10],
                              "Republic of Moldova" = names_missing[11],
                              "Democratic People's Republic of Korea" = names_missing[12],
                              "Russian Federation" = names_missing[13],
                              "Serbia" = names_missing[14],
                              "Republic of Korea" = names_missing[15],
                              "Saint Kitts and Nevis" = names_missing[16],
                              "Saint Lucia" = names_missing[17],
                              "Saint Vincent and the Grenadines" = names_missing[18],
                              "Syrian Arab Republic" = names_missing[19],
                              "United Republic of Tanzania" = names_missing[21],
                              "Gambia" = names_missing[22],
                              "United Kingdom of Great Britain and Northern Ireland" = names_missing[23],
                              "United States of America" = names_missing[24],
                              "Venezuela (Bolivarian Republic of)" = names_missing[25],
                              "Viet Nam" = names_missing[26])) %>% 
  left_join(data_un, by = c("country" = "Country_or_Area")) %>%
  filter(!is.na(Region_Name),
         year %in% c(2005, 2020),
         !country %in% c("Montenegro", "South Sudan"))

write_csv(x = dataset_fh_un, file = "data/tidy-data-fh-un.csv", col_names = TRUE)
