library(here)
library(osfr)
library(tidyverse)
library(kableExtra)
library(modelsummary)
library(readxl)
library(marginaleffects)

options(knitr.kable.NA = "\\")
tx <- tibble(
  p = factor(rep(c("New recruitment",
                   "Wave 1 households", "Wave 2 households",
                   "Total recruitment"),2), levels = c("New recruitment",
                                                       "Wave 1 households", "Wave 2 households",
                                                       "Total recruitment")),
  g = c(1,1,1,1,2,2,2,2),
  s1 = c(977,NA,NA,977, 0,NA,NA,0),
  s2 = c(196,866,NA, 1062, 300, 0,NA, 300),
  s4 = c(68, 780, 162, 1010, 52, 0, 248, 300)
)

tx2 <- tx %>% pivot_wider(names_from = g,
                          values_from = c(s1, s2, s4), 
                          names_vary = "slowest") %>%
  mutate(s3_2 = c(0,0,246,246)) %>%
  relocate(p,s1_1,s2_1,s4_1,s1_2,s2_2,s3_2,s4_2)

kable(tx2, 
      col.names = c("Sample", "Wave 1", "Wave 2", "Wave 4", 
                    "Wave 1", "Wave 2", "Wave 3", "Wave 4"),
      "latex", booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, 
                     "Overall" = 3, "Indoor" = 4))

ds <- read_csv(here("data-clean", 
  "BHET_master_data_05Mar2024.csv"),
  col_select = c("wave", "gender_health", "age_health",
      "smoking", "waist_circ", "height", 
      "weight", "lived_with_smoker")) %>%
  filter(wave != 3) %>%
  mutate(wave = as.factor(wave),
         bmi = weight / height^2,
    female = if_else(gender_health == 2 & !is.na(gender_health),
                     100, 0),
    csmoke = if_else(smoking==1 & !is.na(smoking), 100, 0),
    asmoke = if_else(smoking < 3, 100, 
      if_else(smoking==3 & lived_with_smoker %in% 
      c(2,3), 100, 0))) %>%
  mutate(across(c(female, csmoke, asmoke), as.factor))
    
infer::chisq_test(ds, wave ~ female)

d %>% filter(wave != 3) %>% #& !is.na(gender_health)) %>% 
  mutate(`Female` = if_else(gender_health==2, 100, 0)) %>% 
  group_by(wave) %>% 
  summarize(n = n(), prop = mean(`Female`), 
            np = round(n * prop/100, 0)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = wave, 
              values_from = c(np, prop), 
              names_vary = "slowest")

chisq_test(ds, wave ~ `Female`)

