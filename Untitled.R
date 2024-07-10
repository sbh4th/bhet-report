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
      "weight", "lived_with_smoker", "ID_VILLAGE")) %>%
  filter(wave != 3) %>%
  mutate(wave = as.factor(wave),
         bmi = (weight / (height/100)^2),
    female = if_else(gender_health == 2 & !is.na(gender_health),
                     1, 0),
    csmoke = if_else(smoking==1 & !is.na(smoking), 1, 0),
    asmoke = if_else(smoking < 3, 1, 
      if_else(smoking==3 & lived_with_smoker %in% 
      c(2,3), 1, 0))) %>%
  mutate(across(c(female, csmoke, asmoke), as.integer))

ds %>%
  across()


    
infer::chisq_test(ds, wave ~ female)

ds %>%
  infer::specify(bmi ~ wave) %>%
  fit() %>%
  get_p_value(direction = "two-sided")


  

F_hat <- ds %>% 
    observe(age_health ~ wave, stat = "F")

null_dist_theory <- ds %>%
  specify(age_health ~ wave) %>%
  hypothesize(null = "independence") %>%
  assume(distribution = "F") %>%
  get_p_value(obs_stat = F_hat, direction = "two-sided")

estimate_means <- function(
    descvar) {
  
  if (descvar=="integer") {
    cmeans_descvar <- ds %>% filter(wave != 3) %>%
      group_by(wave) %>%
      summarize(n = n(), 
                prop = round(mean(descvar, na.rm=T) * 100, 1),
                np = round(n * prop/100, 0)) %>% 
      select(-n) %>% 
      pivot_wider(names_from = wave, 
                  values_from = c(np, prop), 
                  names_vary = "slowest") %>%
      mutate(char = "descvar",
             w1 = paste0(np_1, " (", prop_1, ")"),
             w2 = paste0(np_2, " (", prop_2, ")"),
             w4 = paste0(np_4, " (", prop_4, ")")) %>%
      select(char, w1, w2, w4)

  } else {
    cmeans_descvar <- ds %>% filter(wave != 3) %>%
      group_by(wave) %>%
      summarize(vmean = round(mean(descvar, na.rm=T), 1),
                vsd = round(sd(descvar, na.rm=T), 1)) %>% 
      pivot_wider(names_from = wave, 
                  values_from = c(vmean, vsd), 
                  names_vary = "slowest") %>%
      mutate(char = "descvar",
             w1 = paste0(vmean_1, " (", vsd_1, ")"),
             w2 = paste0(vmean_2, " (", vsd_2, ")"),
             w4 = paste0(vmean_4, " (", vsd_4, ")")) %>%
      select(char, w1, w2, w4)
  
  }
  return(cmeans_descvar)
}

estimate_means(bmi)


cprops <- ds %>% filter(wave != 3) %>%
  group_by(wave) %>%
  summarize(n = n(), 
    prop = round(mean(female, na.rm=T) * 100, 1),
    np = round(n * prop/100, 0)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = wave, 
              values_from = c(np, prop), 
              names_vary = "slowest") %>%
  mutate(char = "female",
         w1 = paste0(np_1, " (", prop_1, ")"),
         w2 = paste0(np_2, " (", prop_2, ")"),
         w4 = paste0(np_4, " (", prop_4, ")")) %>%
  select(char, w1, w2, w4)


ctests <- ds %>%
  mutate(female = as.factor(female)) %>%
  infer::chisq_test(wave ~ female) %>%
  select(-chisq_df) %>%
  mutate(across(c('statistic', 'p_value'), round, 3))

cmeanst <- cmeans %>% bind_cols(ctests)



colnames(cmeanst) <- c("Characteristic", 
  "Wave 1 (2018-19) N=1003", "Wave 2 (2019-20) N=1110",
  "Wave 4 (2021-22) N=1028", "Statistic", "p-value")

tt(cmeanst) |>
  # format_tt(replace = "-") |>
  group_tt(
    j = list(
      " " = 1,
      "Estimates" = 2:4,
      "Test for Equality" = 5:6))


cmeans <- ds %>% filter(wave != 3) %>%
  group_by(wave) %>%
  summarize(vmean = round(mean(bmi, na.rm=T), 1),
            vsd = round(sd(bmi, na.rm=T), 1)) %>% 
  pivot_wider(names_from = wave, 
              values_from = c(vmean, vsd), 
              names_vary = "slowest") %>%
  mutate(char = "bmi",
         w1 = paste0(vmean_1, " (", vsd_1, ")"),
         w2 = paste0(vmean_2, " (", vsd_2, ")"),
         w4 = paste0(vmean_4, " (", vsd_4, ")")) %>%
  select(char, w1, w2, w4)
cmeans
