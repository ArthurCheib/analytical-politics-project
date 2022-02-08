## Trash code

dataset %>%
  mutate(avg = (CL +PR)/2,
         my_status = case_when(avg >= 1 & avg <= 2.5 & year < 2003 ~ "F",
                               avg >= 3 & avg < 5.5 & year < 2003 ~ "PF",
                               avg >= 5.5 & avg <= 7 & year < 2003 ~ "NF",
                               avg >= 1 & avg <= 2.5 & year >= 2003 ~ "F",
                               avg >= 3 & avg <= 5 & year >= 2003 ~ "PF",
                               avg >= 5.5 & avg <= 7 & year >= 2003 ~ "NF"),
         check = case_when(my_status != Status ~ 1,
                           TRUE ~ 0)) %>% view

### Countries which first appearance in the INDEX happened after 1995:
dataset %>% 
  group_by(country) %>% 
  slice(1) %>% 
  filter(year != 1995) %>% 
  arrange(year)


### Useless fct_recode to facilitate the join
mutate(country = fct_recode(country, "Antigua & Barbuda" = "Antigua and Barbuda",
                            "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
                            "Cape Verde" = "Cabo Verde",
                            "Congo - Brazzaville" = "Congo (Brazzaville)",
                            "Congo - Kinshasa" = "Congo (Kinshasa)",
                            "Côte d’Ivoire" = "Cote d'Ivoire",
                            "Czechia" = "Czech Republic",
                            "Micronesia (Federated States of)" = "Micronesia",
                            "Myanmar (Burma)" = "Myanmar",
                            "São Tomé & Príncipe" = "Sao Tome and Principe",
                            "Serbia" = "Serbia and Montenegro",
                            "St. Kitts & Nevis" = "St. Kitts and Nevis",
                            "St. Vincent & Grenadines" = "St. Vincent and the Grenadines",
                            "Trinidad & Tobago" = "Trinidad and Tobago",
                            "Gambia" = "The Gambia"))

## Getting countrycode dataset
code_reference <- countrycode::codelist %>% 
  select(country.name.en, un.region.code, un.region.name) %>% 
  filter(!is.na(un.region.name)) %>% 
  mutate(un.region.code = as.numeric(un.region.code))

## Previous code for question (a) graph
ggplot(aes(x = year, y = percentage, fill = Status)) +
  geom_area() +
  theme_economist() +
  labs(title = "Freedom in the World Index - 1995 to 2020",
       subtitle = "What is the percentual evolution of freedom in the World?",
       y = "Percent (%)",
       x = "") +
  theme(legend.position = "bottom",
        plot.title = element_text( size = 16)) +
  scale_fill_manual(values = pal)

## Unces. calc
dataset_fh_un_grouped %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(year >= 2014 & year <= 2020) %>% 
  group_by(better_worse) %>% 
  summarize(avg_change = mean(how_much))