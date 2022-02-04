## Loading the necessary packages and installing them, in case you (our TA) don't have it at your computer:
packages = c("tidyverse", "here", "paletteer", "readxl")

## Now load or install&load all
lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

rm(packages)

## Getting the data
data_dictionary <- readxl::read_xlsx(path = "C:/Users/arthu/Downloads/Harris-School-of-PP/Winter quarter (2nd)/ap-psets/country_territory_ratings.xlsx", sheet = 1)

country_ratings <- readxl::read_xlsx(path = "C:/Users/arthu/Downloads/Harris-School-of-PP/Winter quarter (2nd)/ap-psets/country_territory_ratings.xlsx", sheet = 2, col_names = T, skip = 1) %>% 
  .[,c(1, 68:ncol(.))]

## Cleaning the data
col_names <- colnames(country_ratings)

possible_status <- str_squish(str_remove(unique(country_ratings$...70), c("Status|-")))[c(2,3,4)]
possible_values <- str_c(c(1:7, possible_status), collapse = "|")

cleaned_data <- country_ratings %>%
  rename(country = col_names[1]) %>% 
  gather(key, value, -country) %>%
  mutate(year = as.numeric(ifelse(str_detect(key, "\\..."), NA, key)),
         type = ifelse(!str_detect(value, "PR|CL|Status"), NA, value),
         value = ifelse(str_detect(value, possible_values), value, NA)) %>%
  fill(year, type) %>% 
  select(-key) %>%
  spread(type, value) %>% 
  filter(!is.na(country))



