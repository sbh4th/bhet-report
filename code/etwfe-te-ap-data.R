#  program:  etwfe-te-ap-data.R
#  task:     prepare datasets for air pollution outcomes
#  input:    BHET_master_air_pollution.csv
#  output:   d_personal.rds, d_bc.rds, d_ind_24h.rds
#            d_ind_seasonal.rds, d_is_s3_add.rds
#  project:  BHET
#  author:   sam harper \ 2024-10-05


## 0 Load needed packages ----
library(here)
library(tidyverse)
library(osfr)
library(fastDummies)
library(modeldb)

## 1 read in existing air pollution data from OSF ----
# bring in air pollution data

# read in data for Aim 1
# aim_1 <- osf_retrieve_node("qsmbr")
# aim_1 %>%
#   osf_ls_files("Air pollution",
#     pattern = "BHET_master_air_pollution.csv") %>%
#   osf_download(path = here("data-clean"),
#                conflicts = "overwrite")

# read in air pollution data from OSF
# restrict to specific variables
ap_data <- read_csv(here("data-clean", 
  "BHET_master_data_04Oct2024.csv"),
  col_select = c(ptc_id, hh_id, ID_VILLAGE, ID_COUNTY,
    wave, PM25conc_exposureugm3, p_usable_pm, 
    BCconc_exposureugm3, p_usable_bc,
    indoor_filter_type, indoor_filter_id, 
    house_area, PM25_indoor_24h_sensor,
    PM25_indoor_seasonal_hs,
    n_percent_indoor_seasonal_hs,
    ban_status_composite, smoking, lived_with_smoker, 
    hh_num, gender_health, age_health)) %>%
  
  # create year and cohort_year variables
  mutate(year = if_else(wave==1, 2018, 
      if_else(wave==2, 2019,
      if_else(wave==3, 2020,
      if_else(wave==4, 2021, 0)))),
    cohort_year = if_else(
      ban_status_composite==1, 2019, 
      if_else(ban_status_composite==2, 2020, 
              if_else(ban_status_composite==3, 2021, 2022))),
    treat = ifelse(year >= cohort_year, 1, 0),
    # relabel last cohort year 
    cohort_year = ifelse(cohort_year == 2022,-Inf, 
                         cohort_year),
    male = if_else(gender_health == 1, 1, 0),
    # tobacco exposures
    ets = case_when(
      smoking == 1 ~ "Current smoker",
      smoking == 2 ~ "Former smoker",
      smoking == 3 & lived_with_smoker %in% c(2,3) ~ 
        "Never smoker lived with smoker",
      smoking == 3 & lived_with_smoker == 1 ~ 
        "No smoking exposure"),
    # add dummies for smoking
    ets_former = if_else(
      ets=="Former smoker", 1, 0),
    ets_lived = if_else(
      ets=="Never smoker lived with smoker", 1, 0),
    ets_none = if_else(
      ets=="No smoking exposure", 1, 0)) %>%
  
  # treatment cohort dummies
  add_dummy_variables(cohort_year, 
    values=c(-Inf,2019,2020,2021), 
    remove_original = F) %>%
  # wave dummies
  add_dummy_variables(year, 
    values=c(2018,2019,2021), remove_original = F) %>%
  
  group_by(ID_VILLAGE) %>%
  mutate(v_id = cur_group_id()) %>%
  ungroup()

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
ap_data <- ap_data %>% 
  left_join(dwi, by = c("hh_id", "ptc_id", "wave"))

# bring in meteorological covariates
meteo_data <- read_csv(here("data-clean", 
  "met-data.csv")) 

## 2 create datasets for each outcome ----

# personal exposure for PM2.5
d_personal <- ap_data %>%
  inner_join(meteo_data, 
    by = c("ptc_id", "hh_id", "wave")) %>%
  # rename outcomes
  mutate(pe = case_when(
    PM25conc_exposureugm3 <= 0 ~ NA,
    p_usable_pm == 1 ~ PM25conc_exposureugm3,
    p_usable_pm == 0 ~ NA),
    bc = case_when(
      BCconc_exposureugm3 <= 0 ~ NA,
      p_usable_bc == 1 ~ BCconc_exposureugm3,
      p_usable_bc == 0 ~ NA)) %>%
  # rename meteorological variables
  # for consistency across analyses
  rename(out_temp = out_temp_24h,
         out_dew = out_dew_24h,
         out_rh = rh_24h) %>%
  # drop seasonal meteo data
  select(-out_temp_season,
         -out_dew_season,
         -rh_season)

# write to data-clean folder
write_rds(d_personal, file = here("data-clean",
 "ap-data-personal.rds"))

# indoor exposure (daily)
d_ind_24h <- ap_data %>% 
  # add meterological data
  inner_join(meteo_data, 
    by = c("ptc_id", "hh_id", "wave")) %>%
  # keep usable values of indoor PM
  filter(!is.na(PM25_indoor_24h_sensor)) %>%
  # rename outcome and meteo vars
  rename(i24 = PM25_indoor_24h_sensor,
         out_temp = out_temp_24h,
         out_dew = out_dew_24h,
         out_rh = rh_24h) %>%
  # drop 24h meteo data
  select(-out_temp_season,
         -out_dew_season,
         -rh_season)

# write to data-clean folder
write_rds(d_ind_24h, file = here("data-clean",
 "ap-data-i24h.rds"))

# indoor exposure (seasonal)
d_ind_seasonal <- ap_data %>% 
    # add meterological data
  inner_join(meteo_data, 
    by = c("ptc_id", "hh_id", "wave")) %>%
  # keep usable values of indoor PM
  filter(n_percent_indoor_seasonal_hs >= 0.2) %>%
  # name outcome and meteo vars
  rename(is = PM25_indoor_seasonal_hs,
         out_temp = out_temp_season,
         out_dew = out_dew_season,
         out_rh = rh_season) %>%
  # drop 24h meteo data
  select(-out_temp_24h,
         -out_dew_24h,
         -rh_24h)

# write to data-clean folder
write_rds(d_ind_seasonal, file = here("data-clean",
 "ap-data-iseason.rds"))

# indoor exposure (seasonal) including season 3
# bring in season 3 seasonal data from OSF
# aim_1 %>%
#   osf_ls_files("Air pollution",
#     pattern = "indoor_pm_S3.csv") %>%
#   osf_download(path = here("data-clean"),
#                conflicts = "overwrite")

# read in season 3 data
d_ind_s3 <- read_csv(here("data-clean", 
  "indoor_pm_S3.csv")) %>%
  select(hh_id, wave, ID_VILLAGE, 
    coal_ban_time, pm2.5_indoor_seasonal_hs, 
    N_percent_indoor_seasonal_hs) %>%
  mutate(wave = 3,
    ban_status_composite = case_when(
      coal_ban_time == "2019" ~ 1, 
      coal_ban_time == "2020" ~ 2, 
      coal_ban_time == "2021" ~ 3, 
      coal_ban_time == "Not Yet" ~ 0)) %>%
  rename(n_percent_indoor_seasonal_hs = 
           N_percent_indoor_seasonal_hs,
         is = pm2.5_indoor_seasonal_hs) %>%
  select(hh_id, wave, ID_VILLAGE, 
    ban_status_composite, is, 
    n_percent_indoor_seasonal_hs)

# add to indoor seasonal
d_is_s3_add <- d_ind_seasonal %>%
  select(hh_id, wave, ID_VILLAGE,
    ban_status_composite,
    is, 
    n_percent_indoor_seasonal_hs) %>%
  bind_rows(d_ind_s3) %>%
  filter(n_percent_indoor_seasonal_hs >= 0.2) %>%
  mutate(year = if_else(wave==2, 2019, 
      if_else(wave== 3, 2020,
      if_else(wave== 4, 2021, 0))),
    cohort_year = if_else(
      ban_status_composite==1, 2019, 
      if_else(ban_status_composite==2, 2020, 
              if_else(ban_status_composite==3, 2021, 2022))),
    treat = ifelse(year >= cohort_year, 1, 0),
    cohort_year = ifelse(cohort_year == 2022,-Inf, 
                         cohort_year)) %>%
  # relabel last cohort year 
  # treatment cohort dummies
  add_dummy_variables(cohort_year, 
    values=c(-Inf,2019,2020,2021), 
    remove_original = F) %>%
  # wave dummies
  add_dummy_variables(year, 
    values=c(2018,2019,2020,2021), 
    remove_original = F) %>%
  
  group_by(ID_VILLAGE) %>%
  mutate(v_id = cur_group_id()) %>%
  ungroup()

# write to data-clean folder
write_rds(d_is_s3_add, file = here("data-clean",
 "ap-data-iseason-s3.rds"))


