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