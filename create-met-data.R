#  program:  create-met-data.R
#  task:     create meteorological covariates
#  input:    seasonal and daily temp / humidity .csv files
#  output:   met-data.csv
#  project:  BHET
#  author:   sam harper \ 2024-09-23

## 0 Load needed packages ----
library(here)
library(tidyverse)
library(osfr)

## 1 Download data from OSF to data-clean ----

# daily
met <- osf_retrieve_file("https://osf.io/57gs8") %>% 
  osf_download(path = here("data-clean"), 
               conflicts = "overwrite")


# seasonal
mets <- osf_retrieve_file("https://osf.io/pky4f") %>% 
  osf_download(path = here("data-clean"), 
               conflicts = "overwrite")

## 2 Create data ----

# rename daily variables
daily <- read_csv(here("data-clean", 
  "outdoor_temp_24h_household-level_28dec23.csv")) %>%
  rename(out_temp_24h = outdoor_temp_idw,
         out_dew_24h = outdoor_dew_idw,
         rh_24h = RH)

# rename seasonal variables
seasonal <- read_csv(here("data-clean", 
  "outdoor_temp_seasonal_household-level_28dec23.csv")) %>%
  rename(out_temp_season = outdoor_temp_idw,
         out_dew_season = outdoor_dew_idw,
         rh_season = outdoor_RH)

# merge
meteo <- daily %>% 
  left_join(seasonal, 
    by = c("ptc_key", "wave", "hh_id", "ptc_id"))

## 3 Save data ----
write_csv(meteo, 
  here("data-clean", "met-data.csv"))

