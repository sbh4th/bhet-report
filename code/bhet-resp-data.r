#  program:  bhet-resp-analysis.R
#  task:     estimate models for respiratory outcomes
#  input:    bhet-master
#  output:   
#  project:  BHET
#  author:   sam harper \ 2024-10-04


##  0 Load needed packages ----
library(here)
library(tidyverse)
library(haven)
library(modeldb)
library(modelr)
library(osfr)
library(modelsummary)


## download data files from OSF (data-clean component)
# dir.create("data-clean")
# bhet_project <- osf_retrieve_node("vxur5")
# bhet_project %>%
#   osf_ls_files("Master Dataset (Seasons 1-4)",
#                pattern = "csv") %>%
#   osf_download(path = here("data-clean"),
#     conflicts = "overwrite")
#
## set path to upload model fits to OSF
## code component
#u2s_fits <- osf_retrieve_node("cv9qg")



## 1 Read in dataset, limit to resp vars ----
d <- read_csv(here("data-clean", 
                   "BHET_master_data_22Jul2024.csv"), 
  col_select= c(hh_id, ptc_id, wave, ID_VILLAGE, ID_COUNTY, 
                ban_status_2019, ban_status_2020, 
                ban_status_2021, ban_status_no, 
                ban_status_composite,
                freq_cough, freq_phlegm,
                freq_wheezing, freq_breath,
                freq_no_chest, height, weight, 
                age_health, gender_health,
                smoking, temp, PM25_exp_remove, 
                PM25conc_exposureugm3,
                lived_with_smoker, years_with_smoker,
                height, weight, occupation,
                freq_farming, freq_exercising,
                freq_drink))

# import wealth index
dwi <- read_csv(here("data-clean", 
  "BHET_PCA_11Oct2023.csv"))

dwi <- dwi %>%
  group_by(wave) %>%
  mutate(wq = as.integer(cut(wealth_index, 
    quantile(wealth_index, 
      probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T), 
    labels = c(1:4), include.lowest = T))) %>%
  ungroup() %>%
  add_dummy_variables(wq, 
    values=c(1:4), remove_original = F) 

# add wealth index to main dataset
d1 <- d %>% 
  left_join(dwi, by = c("hh_id", "ptc_id", "wave"))


# define main outcomes and covariates
d2 <- d1 %>%
  mutate(
    # total symptoms
    cresp = rowSums(across(c(freq_breath, freq_cough, 
      freq_phlegm, freq_wheezing, freq_no_chest))),
    
    # separate symptoms
    cough = if_else(freq_cough < 3, 1, 0),
    phlegm = if_else(freq_phlegm < 3, 1, 0),
    wheeze = if_else(freq_wheezing < 3, 1, 0),
    breath = if_else(freq_breath < 3, 1, 0),
    nochest = if_else(freq_no_chest < 3, 1, 0),
    resp = if_else(
    freq_cough < 3 |
    freq_phlegm < 3 |
    freq_wheezing < 3 |
    freq_breath < 3 |
    freq_no_chest < 3, 1, 0),
    
    # covariates
    male = if_else(gender_health == 1, 1, 0),
    # csmoke = if_else(smoking == 1, 1, 0),
    # fsmoke = if_else(smoking == 2, 1, 0),
    bmi = weight / (height/100)^2,
    
    occ = case_when(
     occupation == 1 ~ "Agriculture",
     occupation %in% c(2,11,12) ~ "Manual",
     occupation %in% c(3:5) ~ "Non-manual",
     occupation %in% c(6:10) ~ "Other"),
   # add dummies for occupation
    occ_2 = if_else(
      occ=="Manual", 1, 0),
    occ_3 = if_else(
      occ=="Non-manual", 1, 0),
    occ_4 = if_else(
      occ=="Other", 1, 0),
    
   ets = case_when(
      smoking == 1 ~ "Current smoker",
      smoking == 2 ~ "Former smoker",
      smoking == 3 & lived_with_smoker %in% c(2,3) ~ 
        "Never smoker lived with smoker",
      smoking == 3 & lived_with_smoker == 1 ~ 
        "No smoking exposure"),
    # add dummies for smoking
    ets_2 = if_else(
      ets=="Former smoker", 1, 0),
    ets_3 = if_else(
      ets=="Never smoker lived with smoker", 1, 0),
    ets_4 = if_else(
      ets=="No smoking exposure", 1, 0),
    
   drink = case_when(
      freq_drink == 1 ~ "Never",
      freq_drink %in% c(2:5) ~ "Occasional",
      freq_drink %in% c(6:8) ~ "Regular",
      freq_drink == 9 ~ "Everyday"),
    # add dummies for drinking
    drink_2 = if_else(
      drink=="Occasional", 1, 0),
    drink_3 = if_else(
      drink=="Regular", 1, 0),
    drink_4 = if_else(
      drink=="Everyday", 1, 0),
    
    farm = case_when(
      freq_farming == 1 ~ "Never",
      freq_farming %in% c(2:3) ~ "Occasional",
      freq_farming == 4 ~ "Regular",
      freq_farming == 5 ~ "Everyday"),
    # add dummies for farming
    farm_2 = if_else(
      farm=="Occasional", 1, 0),
    farm_3 = if_else(
      farm=="Regular", 1, 0),
    farm_4 = if_else(
      farm=="Everyday", 1, 0)) %>%
    
    # treatment related
    mutate(year = if_else(wave==1, 2018, 
      if_else(wave==2, 2019,
        if_else(wave==4, 2021, 0))),
    cohort_year = if_else(
      ban_status_composite==1, 2019, 
      if_else(ban_status_composite==2, 2020, 
              if_else(ban_status_composite==3, 2021, 2022))),
    treat = ifelse(year >= cohort_year, 1, 0),
    cohort_year = ifelse(cohort_year == 2022,-Inf, 
                         cohort_year),
    ppm25 = case_when(
      PM25conc_exposureugm3 <= 0 ~ NA,
      PM25_exp_remove == 1 ~ PM25conc_exposureugm3,
      PM25_exp_remove == 0 ~ NA,
      PM25_exp_remove == -1 ~ NA)) %>%
    rename(district = ID_COUNTY) %>%
  
  # add dummies for district
  add_dummy_variables(district,
  values = c(1,2,3,4),
  remove_original = T) %>%
  # relabel last cohort year 
  # treatment cohort dummies
  add_dummy_variables(cohort_year, 
    values=c(-Inf,2019,2020,2021), 
    remove_original = F) %>%
  # wave dummies
  add_dummy_variables(year, 
    values=c(2018,2019,2021), remove_original = F) %>%
  # create unique continuous village Id
  group_by(ID_VILLAGE) %>%
  mutate(v_id = cur_group_id()) %>%
  ungroup()


write_rds(d2, here("data-clean", "bhet-resp-data.rds"))

