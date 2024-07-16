library(here)
library(osfr)
library(tidyverse)
library(kableExtra)
library(modelsummary)
library(readxl)
library(marginaleffects)
library(tinytable)
library(tidymodels)

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

dst <- read_csv(here("data-clean", 
  "BHET_master_data_05Mar2024.csv"),
  col_select = c("wave", "gender_health", "age_health",
      "smoking", "waist_circ", "height", "ptc_id",
      "weight", "lived_with_smoker", "ID_VILLAGE")) %>%
  filter(wave != 3) %>%
  add_count(ptc_id) %>%
  mutate(wave = as.factor(n),
    bmi = (weight / (height/100)^2),
    female = if_else(gender_health == 2 & !is.na(gender_health),
      1, 0),
    csmoke = if_else(smoking==1 & !is.na(smoking), 1, 0),
    asmoke = if_else(smoking < 3, 1, 
      if_else(smoking==3 & lived_with_smoker %in% 
      c(2,3), 1, 0))) %>%
  mutate(across(c(female, csmoke, asmoke), as.integer))


infer::chisq_test(dst, nwaves ~ female)

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

cmeanst <- F_hat %>% 
  bind_cols(null_dist_theory) %>%
  rename("statistic" = stat) %>%
  mutate(across(c('statistic', 'p_value'), 
                ~ sprintf('%.3f', .x)))
  




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

cmeanst <- cprops %>% bind_cols(ctests)



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
    vsd = sprintf('%.1f', sd(bmi, na.rm=T))) %>%
  pivot_wider(names_from = wave, 
              values_from = c(vmean, vsd), 
              names_vary = "slowest") %>%
  mutate(char = "bmi",
         w1 = paste0(vmean_1, " (", vsd_1, ")"),
         w2 = paste0(vmean_2, " (", vsd_2, ")"),
         w4 = paste0(vmean_4, " (", vsd_4, ")")) %>%
  select(char, w1, w2, w4)


F_hat <- ds %>% 
    observe(age_health ~ wave, stat = "F")

null_dist_theory <- ds %>%
  specify(bmi ~ wave) %>%
  hypothesize(null = "independence") %>%
  assume(distribution = "F") %>%
  get_p_value(obs_stat = F_hat, direction = "two-sided")

cmeanss <- F_hat %>% 
  bind_cols(null_dist_theory) %>%
  rename("statistic" = stat) %>%
  mutate(across(c('statistic', 'p_value'), 
                ~ sprintf('%.3f', .x)))

cmeanst <- cmeans %>% bind_cols(cmeanss)


ds <- read_csv(here("data-clean", 
                    "BHET_master_data_05Mar2024.csv"),
  col_select = c("wave", "gender_health", "age_health",
    "smoking", "waist_circ", "height", "ptc_id", 
    "weight", "lived_with_smoker", "ID_VILLAGE")) %>%
  filter(wave != 3) %>%
  mutate(catvar = recode_factor(wave, 
    `1` = "1", `2` = "2", `4` = "3"),
    bmi = (weight / (height/100)^2),
    female = if_else(gender_health == 2 & !is.na(gender_health),
      1, 0),
    csmoke = if_else(smoking==1 & !is.na(smoking), 1, 0),
    asmoke = if_else(smoking < 3, 1, 
      if_else(smoking==3 & lived_with_smoker %in% 
                                    c(2,3), 1, 0))) %>%
  mutate(across(c(female, csmoke, asmoke), as.integer))



# Combined function to process columns
process_column <- function(ds, column_name) {
  column_type <- typeof(ds[[column_name]])
  
  if (column_type == "integer") {
    cprops <- ds %>% filter(wave != 3) %>%
      group_by(catvar) %>%
      summarize(
        n = n(), 
        prop = mean(.data[[column_name]], na.rm=TRUE),
        np = round(n * prop, 0),
        pr = sprintf('%.1f', prop * 100)) %>% 
      select(catvar, np, pr) %>% 
      pivot_wider(names_from = catvar, 
                  values_from = c(np, pr), 
                  names_vary = "slowest") %>%
      mutate(char = column_name,
             w1 = paste0(np_1, " (", pr_1, ")"),
             w2 = paste0(np_2, " (", pr_2, ")"),
             w3 = paste0(np_3, " (", pr_3, ")")) %>%
      select(char, w1, w2, w3)
    
    cprop_stats <- ds %>%
      mutate(!!sym(column_name) := as.factor(.data[[column_name]])) %>%
      infer::chisq_test(as.formula(paste("catvar ~", column_name))) %>%
      select(-chisq_df) %>%
      mutate(across(c('statistic', 'p_value'), 
                    ~ sprintf('%.3f', .x)))
    
    ctable <- cprops %>% bind_cols(cprop_stats)
    
  } else if (column_type == "double") {
    cmeans <- ds %>% filter(wave != 3) %>%
      group_by(catvar) %>%
      summarize(
        vmean = sprintf('%.1f', mean(.data[[column_name]], 
                                     na.rm=TRUE)),
        vsd = sprintf('%.1f', sd(.data[[column_name]], 
                                 na.rm=TRUE))) %>%
      pivot_wider(names_from = catvar, 
                  values_from = c(vmean, vsd), 
                  names_vary = "slowest") %>%
      mutate(char = column_name,
             w1 = paste0(vmean_1, " (", vsd_1, ")"),
             w2 = paste0(vmean_2, " (", vsd_2, ")"),
             w3 = paste0(vmean_3, " (", vsd_3, ")")) %>%
      select(char, w1, w2, w3)
    
    formula <- as.formula(paste(column_name, "~ catvar"))
    
    F_hat <- ds %>% 
      infer::observe(formula, stat = "F")
    
    null_dist_theory <- ds %>%
      infer::specify(formula) %>%
      hypothesize(null = "independence") %>%
      assume(distribution = "F") %>%
      get_p_value(obs_stat = F_hat, direction = "two-sided")
    
    cmeans_stats <- F_hat %>% 
      bind_cols(null_dist_theory) %>%
      rename("statistic" = stat) %>%
      mutate(across(c('statistic', 'p_value'), 
                    ~ sprintf('%.3f', .x)))
    
    ctable <- cmeans %>% bind_cols(cmeans_stats)
  }
  
  return(ctable)
}

# Combine results for all columns
process_columns <- function(ds, columns) {
  result <- bind_rows(lapply(columns, function(col) 
    process_column(ds, col)))
  return(result)
}

# Columns of interest
columns_of_interest <- c("female", "csmoke", "asmoke", 
                         "bmi", "age_health", "waist_circ")

# Apply the function to the data set
result <- process_columns(ds, columns_of_interest)



ds <- ds %>%
  select(-catvar) %>%
  filter(wave != 3) %>%
  add_count(ptc_id) %>%
  mutate(catvar = recode_factor(n, 
    `1` = "1", `2` = "2", `3` = "3")) %>%
  group_by(catvar) %>% tally()

t3 <- result %>%
  mutate(char = c(
    "Female, n (%)", 
    "Current smoker, n (%)",
    "Any smoke exposure, n (%)", 
    "Age in years, Mean (SD)",
    "BMI (kg/m2), Mean (SD)", 
    "Waist circumference (cm), Mean (SD)")) %>%
  rename("Characteristic" = char)

colnames(t3) <- c("Characteristic", 
                  "1 Wave N=365", "2 Waves N=886",
                  "3 Waves N=1890", "Statistic", "p-value")

tt(t3,
   caption = "Demographic and health characteristics of participants who contributed to different numbers of study waves.",
   width = c(.4, .15, .15, .15, .1, .1),
   notes = list(a = list(i=0, j=5,
                         text = "Chi-square test for categorical and F-test for continuous characteristics."))) |>
  # theme_tt("multipage") |>
  group_tt(
    j = list(
      " " = 1,
      "Estimates" = 2:4,
      "Test for Equality" = 5:6)) |>
  format_tt(escape = TRUE) |>
  style_tt(j = 1:4, align = "llll")







d <- read_csv(here("data-clean", 
  "BHET_master_data_05Mar2024.csv"),
  col_select = c("wave", "gender_health", "age_health",
    "smoking", "waist_circ", "height", "ptc_id", 
    "weight", "lived_with_smoker", "ID_VILLAGE")) %>%
filter(wave != 3) %>%                                                         # no participant-level data in year 3
  mutate(ETS = ifelse(smoking == 1, 1, NA),                                     # generate smoking variable
         ETS = ifelse(smoking == 2, 2, ETS),
         ETS = ifelse(lived_with_smoker %in% c(2,3) & is.na(ETS), 3, ETS),
         ETS = ifelse(lived_with_smoker == 1 & is.na(ETS), 4, ETS),
         ETS = factor(ETS)) %>%
  # dichotomize into smoke vs. no smoke exposure (1=smoke/0=no smoke)
  mutate(ETS_binary = ifelse(ETS %in% c(1,2,3), 1, 0)) %>%
  mutate(BMI = weight/(height/100)^2) %>%
  group_by(ptc_id) %>%
  mutate(n_obs = n(),
         count = 1:n()) %>%
  ungroup() %>%