#  program:  etwfe-te-ap-data.R
#  task:     prepare datasets for air pollution outcomes
#  input:    BHET_master_air_pollution.csv
#  output:   d_personal.rds, d_bc.rds, d_ind_24h.rds
#            d_ind_seasonal.rds, d_is_s3_add.rds
#  project:  BHET
#  author:   sam harper \ 2024-04-15


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
  "BHET_master_data_22Jul2024.csv"), # BHET_master_data_22Jul2024.csv
  col_select = c(ptc_id, hh_id, ID_VILLAGE, wave, 
    PM25conc_exposureugm3, PM25_exp_remove, 
    BCconc_exposureugm3, BC_exp_remove,
    usable_indoor_filter, BC_indoor_remove,
    indoor_filter_type, indoor_filter_id, 
    house_area, PM25_indoor_24h_sensor,
    PM25_indoor_seasonal_hs,
    n_percent_indoor_seasonal_hs,
    ban_status_composite, smoking, lived_with_smoker, 
    outdoor_temp_24h, outdoor_dew_24h, hh_num)) 
%>%
  
  # create year and cohort_year variables
  mutate(year = if_else(wave=="S1", 2018, 
      if_else(wave=="S2", 2019,
        if_else(wave=="S4", 2021, 0))),
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
    values=c(2018,2019,2021), remove_original = F) %>%
  
  group_by(ID_VILLAGE) %>%
  mutate(v_id = cur_group_id()) %>%
  ungroup()


## 2 create datasets for each outcome ----

# personal exposure for PM2.5
d_personal <- ap_data %>%
  filter(PM25_exp_remove == 1) %>%
  # smoking variable for consistency across outcomes
  mutate(smoking = ptc_smoking) %>%
  # drop households with duplicate values
  distinct(hh_id, wave, PM25conc_exposureugm3, 
           .keep_all= TRUE)

# write to data-clean folder
write_rds(d_personal, file = here("data-clean",
 "ap-data-personal.rds"))

# personal exposure for black carbon
d_bc <- ap_data %>%
  # smoking variable for consistency across outcomes
  mutate(smoking = ptc_smoking) %>%
  filter(bc_exp_remove == 1)

# write to data-clean folder
write_rds(d_bc, file = here("data-clean",
 "ap-data-bc.rds"))

# indoor exposure (daily)
d_ind_24h <- ap_data %>% 
  # keep usable values of indoor PM
  filter(!is.na(pm2.5_indoor_sensor_24h)) %>%
  # recode smoking variable to reflect household status
  mutate(smoking = if_else(hh_smoking=="Smoking", 
                      "Smoker", "Non-smoker")) %>%
  # drop households with duplicate values
  distinct(hh_id, wave, pm2.5_indoor_sensor_24h, 
           .keep_all= TRUE)

# write to data-clean folder
write_rds(d_ind_24h, file = here("data-clean",
 "ap-data-i24h.rds"))

# indoor exposure (seasonal)
d_ind_seasonal <- ap_data %>% 
  filter(N_percent_indoor_seasonal_hs >= 0.2) %>%
  # recode smoking variable to reflect household status
  mutate(smoking = if_else(hh_smoking=="Smoking", 
                      "Smoker", "Non-smoker")) %>%
  distinct(hh_id, wave, pm2.5_indoor_seasonal_hs, 
           .keep_all= TRUE)

# write to data-clean folder
write_rds(d_ind_seasonal, file = here("data-clean",
 "ap-data-iseason.rds"))

# indoor exposure (seasonal) including season 3
# bring in season 3 seasonal data from OSF
aim_1 %>%
  osf_ls_files("Air pollution",
    pattern = "indoor_pm_S3.csv") %>%
  osf_download(path = here("data-clean"),
               conflicts = "overwrite")

# read in season 3 data
d_ind_s3 <- read_csv(here("data-clean", 
  "indoor_pm_S3.csv")) %>%
  select(hh_id, wave, ID_VILLAGE, 
    coal_ban_time, pm2.5_indoor_seasonal_hs, 
    N_percent_indoor_seasonal_hs)

# add to indoor seasonal
d_is_s3_add <- d_ind_seasonal %>%
  select(hh_id, wave, ID_VILLAGE,
    coal_ban_time,
    pm2.5_indoor_seasonal_hs, 
    N_percent_indoor_seasonal_hs) %>%
  bind_rows(d_ind_s3) %>%
  filter(N_percent_indoor_seasonal_hs >= 0.2) %>%
  mutate(year = if_else(wave=="S1", 2018, 
      if_else(wave=="S2", 2019,
      if_else(wave=="S3", 2020,
      if_else(wave=="S4", 2021, 0)))),
    ban_status_composite = case_when(
      coal_ban_time == 2019 ~ 1, 
      coal_ban_time == 2020 ~ 2, 
      coal_ban_time == 2021 ~ 3, 
      coal_ban_time == "Not Yet" ~ 0),
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


