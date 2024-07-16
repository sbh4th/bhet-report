#' ------
#' Project: BHET
#' Script purpose: HEI report descriptive stats
#' Date created: April 12, 2024
#' Last Update: July 16, 2024
#' Author: Talia Sternbach
#' ------

# Set-up: Load packages, functions, data =======================================
library(tidyverse); library(haven); library(readxl)

# Load master dataset
master <- read_dta("Master-data/Processed-data/BHET_master_data_10Jul2024.dta")

# Participant characteristics by # of observations =============================
desc_by_obs <- master %>% 
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
  select(hh_id, ptc_id, ID_VILLAGE, wave, gender_health, ETS, ETS_binary,
         age_health, age_CORRECTED, waist_circ, BMI, n_obs, count) %>%
  filter(count == 1) %>%
  group_by(n_obs) %>%
  summarise(n = n(),
            no_female = sum(gender_health == 2, na.rm = T),
            perc_female = (no_female/n)*100,
            mean_age = mean(age_CORRECTED, na.rm = T),
            sd_age = sd(age_CORRECTED, na.rm = T),
            no_current_smok = sum(ETS == 1, na.rm = T),
            perc_current_smok = (no_current_smok/n)*100,
            no_smok = sum(ETS_binary == 1, na.rm = T),
            perc_smok = (no_smok/n)*100,
            mean_waist = mean(waist_circ, na.rm = T),
            sd_waist = sd(waist_circ, na.rm = T),
            mean_BMI = mean(BMI, na.rm = T),
            sd_BMI = sd(BMI, na.rm = T)) %>%
  ungroup()

desc_by_obs_table <- desc_by_obs %>%
  rename(`Number of observations` = n_obs,
         N = n) %>%
  mutate(`Female, N (%)` = paste0(no_female, " (", round(perc_female,1), ")"),
         `Age, Mean (SD), years` = paste0(round(mean_age, 1), " (", round(sd_age,1), ")"),
         `Current smoker, N (%)` = paste0(no_current_smok, " (", round(perc_current_smok,1), ")"),
         `Any smoke exposure, N (%)` = paste0(no_smok, " (", round(perc_smok,1), ")"),
         `BMI, Mean (SD), kg/m2` = paste0(round(mean_BMI, 1), " (", round(sd_BMI,1), ")"),
         `Waist circumference, Mean (SD), cm` = paste0(round(mean_waist, 1), " (", round(sd_waist,1), ")")) %>%
  select(`Number of observations`, N, `Female, N (%)`, `Age, Mean (SD), years`,
         `Current smoker, N (%)`, `Any smoke exposure, N (%)`, 
         `BMI, Mean (SD), kg/m2`, `Waist circumference, Mean (SD), cm`)

desc_by_obs_table_t <- data.frame(t(desc_by_obs_table[-1]))
colnames(desc_by_obs_table_t) <- c("1 Measure", "2 Measures", "3 Measures")
desc_by_obs_table_t$characteristic <- rownames(desc_by_obs_table_t)
desc_by_obs_format <- desc_by_obs_table_t %>%
  select(characteristic, `1 Measure`, `2 Measures`, `3 Measures`)

writexl::write_xlsx(desc_by_obs_format, "Tables/ptc-descriptives-by-n-obs.xlsx")

# Participant characteristics by study year ====================================
char_by_year <- master %>% 
  filter(wave != 3) %>%                                                         # no participant-level data in year 3
  mutate(ETS = ifelse(smoking == 1, 1, NA),                                     # generate smoking variable
         ETS = ifelse(smoking == 2, 2, ETS),
         ETS = ifelse(lived_with_smoker %in% c(2,3) & is.na(ETS), 3, ETS),
         ETS = ifelse(lived_with_smoker == 1 & is.na(ETS), 4, ETS),
         ETS = factor(ETS)) %>%
  # dichotomize into smoke vs. no smoke exposure (1=smoke/0=no smoke)
  mutate(ETS_binary = ifelse(ETS %in% c(1,2,3), 1, 0)) %>%
  mutate(BMI = weight/(height/100)^2) %>%
  select(hh_id, ptc_id, ID_VILLAGE, wave, gender_health, ETS, ETS_binary,
         age_health, age_CORRECTED, waist_circ, BMI) %>%
  group_by(wave) %>%
  summarise(n = n(),
            no_female = sum(gender_health == 2, na.rm = T),
            perc_female = (no_female/n)*100,
            mean_age = mean(age_CORRECTED, na.rm = T),
            sd_age = sd(age_CORRECTED, na.rm = T),
            no_current_smok = sum(ETS == 1, na.rm = T),
            perc_current_smok = (no_current_smok/n)*100,
            no_smok = sum(ETS_binary == 1, na.rm = T),
            perc_smok = (no_smok/n)*100,
            mean_waist = mean(waist_circ, na.rm = T),
            sd_waist = sd(waist_circ, na.rm = T),
            mean_BMI = mean(BMI, na.rm = T),
            sd_BMI = sd(BMI, na.rm = T)) %>%
  ungroup()

char_by_year_table <- char_by_year %>%
  rename(N = n) %>%
  mutate(`Female, N (%)` = paste0(no_female, " (", round(perc_female,1), ")"),
         `Age, Mean (SD), years` = paste0(round(mean_age, 1), " (", round(sd_age,1), ")"),
         `Current smoker, N (%)` = paste0(no_current_smok, " (", round(perc_current_smok,1), ")"),
         `Any smoke exposure, N (%)` = paste0(no_smok, " (", round(perc_smok,1), ")"),
         `BMI, Mean (SD), kg/m2` = paste0(round(mean_BMI, 1), " (", round(sd_BMI,1), ")"),
         `Waist circumference, Mean (SD), cm` = paste0(round(mean_waist, 1), " (", round(sd_waist,1), ")")) %>%
  select(N, `Female, N (%)`, `Age, Mean (SD), years`,
         `Current smoker, N (%)`, `Any smoke exposure, N (%)`, 
         `BMI, Mean (SD), kg/m2`, `Waist circumference, Mean (SD), cm`)

char_by_year_table_t <- data.frame(t(char_by_year_table))
colnames(char_by_year_table_t) <- c("Wave 1 (2018-19)", "Wave 2 (2019-20)", "Wave 4 (2021-22)")
char_by_year_table_t$characteristic <- rownames(char_by_year_table_t)
char_by_year_format <- char_by_year_table_t %>%
  select(characteristic, `Wave 1 (2018-19)`, `Wave 2 (2019-20)`, `Wave 4 (2021-22)`)

writexl::write_xlsx(char_by_year_format, "Tables/ptc-descriptives-by-wave.xlsx")


