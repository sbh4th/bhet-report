#  program:  quantile-bayes.R
#  task:     estimate qreg models for temp
#  input:    
#  output:   
#  project:  BHET
#  author:   sam harper \ 2025-09-05


## 0 Load needed packages ----
library(here)
library(readxl)
library(tidyverse)
library(osfr)
library(marginaleffects)
library(modelsummary)
library(brms)
library(bayesplot)
library(tidybayes)
library(cmdstanr)
options(mc.cores = 4,
        brms.backend = "cmdstanr")

d <- read_csv(here("data-clean", 
  "BHET_master_data_04Oct2024.csv"),
  col_select = c(ptc_id, hh_id, ID_VILLAGE, ID_COUNTY,
                 wave, PM25conc_exposureugm3, p_usable_pm, 
                 BCconc_exposureugm3, p_usable_bc,
                 indoor_filter_type, indoor_filter_id, 
                 house_area, PM25_indoor_24h_sensor,
                 PM25_indoor_seasonal_hs,
                 n_percent_indoor_seasonal_hs,
                 ban_status_composite, smoking, lived_with_smoker, 
                 hh_num, gender_health, age_health,
                 temp))

# 01/05 Household-level indoor temperature data --------------------------------
ltt_node <- osf_retrieve_node("https://osf.io/wc9ta/")

hh_indtemp_dl <- osf_ls_files(ltt_node) %>%
  filter(name == "HH_indoor_temperature_ALL_SEASONS.xlsx") %>% 
  osf_download(path = here("data-clean"), 
    conflicts = "overwrite")

hh_lt_temp <- read_xlsx(here("data-clean", 
  "HH_indoor_temperature_ALL_SEASONS.xlsx"))

dt <- d %>%  
  left_join(hh_lt_temp, 
    by = c("ID_VILLAGE", "hh_id", "wave")) %>%
  # create year and cohort_year variables
  mutate(
    year = if_else(wave==1, 2018, 
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
    cohort_year_2019 = ifelse(cohort_year==2019,1,0),
    cohort_year_2020 = ifelse(cohort_year==2020,1,0),
    cohort_year_2021 = ifelse(cohort_year==2021,1,0),
    year_2019 = ifelse(year==2019,1,0),
    year_2020 = ifelse(year==2020,1,0),
    year_2021 = ifelse(year==2021,1,0)) %>%
  
  group_by(ID_VILLAGE) %>%
  mutate(v_id = cur_group_id()) %>%
  ungroup() %>%
  drop_na(temp)

# simple gaussian brms model
bt_gaussian <-
  brm(data = dt, 
      family = gaussian(),
      temp ~ 1 + (1 | v_id) +
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,        
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 84665)
  
bt_gaussian_post <- marginaleffects::predictions(
  bt_gaussian,
  re_formula = NULL,
  newdata = subset(dt, treat==1),
  variables = "treat")

bt_gaussian_post |>
  get_draws() |>
  ggplot(aes(x = draw, group = treat, color = treat)) +
  geom_density() + theme_classic()

# Quantile: 90th
bc_q90 <- 
  brm(bf(temp ~ 1 + (1 | v_id) +
           treat:cohort_year_2019:year_2019 + 
           treat:cohort_year_2019:year_2021 +
           treat:cohort_year_2020:year_2021 +
           treat:cohort_year_2021:year_2021 +
           cohort_year_2019 + cohort_year_2020 +
           cohort_year_2021 + year_2019 + year_2021, 
           quantile = 0.9), 
           data = dt, 
           family = asym_laplace(),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 5646)

bc_90_post <- marginaleffects::predictions(
  bc_q90,
  re_formula = NULL,
  newdata = subset(dt, treat==1),
  variables = "treat") |>
  get_draws() |>
  ggplot(aes(x = draw, group = treat, color = treat)) +
  geom_density() + theme_classic()

# Quantile: 10th
bc_q10 <- 
  brm(bf(temp ~ 1 + (1 | v_id) +
           treat:cohort_year_2019:year_2019 + 
           treat:cohort_year_2019:year_2021 +
           treat:cohort_year_2020:year_2021 +
           treat:cohort_year_2021:year_2021 +
           cohort_year_2019 + cohort_year_2020 +
           cohort_year_2021 + year_2019 + year_2021, 
         quantile = 0.1), 
      data = dt, 
      family = asym_laplace(),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 78)

bc_10_post <- marginaleffects::predictions(
  bc_q10,
  newdata = subset(dt, treat==1),
  variables = "treat") |>
  get_draws() |>
  mutate(quantile = "Upper 10th") |>
  select(draw, treat, quantile)

bc_90_post <- marginaleffects::predictions(
  bc_q90,
  newdata = subset(dt, treat==1),
  variables = "treat") |>
  get_draws() |>
  mutate(quantile = "Lower 10th") |>
  select(draw, treat, quantile)

plot <- bc_90_post %>%
  bind_rows(bc_10_post) %>%
  ggplot(aes(x = draw, group = treat,
         fill = treat, color=treat)) +
  geom_density(alpha=0.2) + facet_wrap(vars(quantile), nrow=2) +
  theme_classic() + labs(x = "Temp (C)")

