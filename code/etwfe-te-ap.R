# Code to estimate ETWFE models for air pollution outcomes
library(tidyverse)
library(fixest)
library(marginaleffects)
library(kableExtra)
library(fastDummies)
library(modeldb)


# bring in air pollution data
ap_data <- read_csv(here("data-clean", 
  "BHET_master_air_pollution.csv"),
  col_select = c(ptc_id, hh_id, ID_VILLAGE, wave, 
    PM25conc_exposureugm3, PM25_exp_remove, 
    bc_exp_conc, bc_exp_remove,
    usable_indoor_filter, bc_indoor_remove,
    indoor_filter_type, indoor_filter_id, 
    house_area, pm2.5_indoor_sensor_24h,
    pm2.5_indoor_seasonal_hs,
    N_percent_indoor_seasonal_hs, year,
    ban_status_composite, ptc_smoking, 
    outdoor_temp_24h, outdoor_dew_24h, hh_num)) %>%
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

# data frames
d_personal <- ap_data %>%
  filter(PM25_exp_remove == 1) 

d_bc <- ap_data %>%
  filter(bc_exp_remove == 1)

d_ind_24h <- ap_data %>% 
  # keep usable values of indoor PM
  filter(!is.na(pm2.5_indoor_sensor_24h)) %>%
  # drop households with duplicate values
  distinct(hh_id, wave, pm2.5_indoor_sensor_24h, 
           .keep_all= TRUE)

d_ind_seasonal <- ap_data %>% 
  filter(N_percent_indoor_seasonal_hs >= 0.2) %>% 
  distinct(hh_id, wave, pm2.5_indoor_seasonal_hs, 
           .keep_all= TRUE)

# Function to estimate models
estimate_etwfe <- function(outcome_vars, predictor_vars) {
  models <- outcome_vars %>%
    set_names() %>%
    map(~ {
      formula <- as.formula(paste(.x, "~", 
        paste(predictor_vars, collapse = " + ")))
      glm(formula, data = dresp, family = "binomial")
    })
  
  return(models)
}

# Function to estimate models
estimate_etwfe <- function(outcome, predictors, data) {
  models <- predictors %>%
    set_names() %>%
    map(~ {
      formula <- as.formula(paste(outcome, "~", 
        paste(.x, collapse = " + ")))
      glm(formula, data = data, cluster = ~v_id)
    })
  
  return(models)
}


# basic DiD specification
rhs_did <- c("treat:cohort_year_2019:year_2019",
  "treat:cohort_year_2019:year_2021", 
  "treat:cohort_year_2020:year_2021",
  "treat:cohort_year_2021:year_2021", "cohort_year_2019",
  "cohort_year_2020", "cohort_year_2021",
  "year_2019", "year_2021")

# DiD plus covariates
rhs_dida <- c(rhs_did, "hh_num", "ptc_smoking", 
  "outdoor_temp_24h", "outdoor_dew_24h")

# gather estimates across models

# personal PM
# basic ETWFE model
did_ppm <- fixest::feols(
  as.formula(paste("PM25conc_exposureugm3 ~ ", 
    paste(rhs_did, collapse= "+"))), 
  data = d_personal, cluster = ~v_id)

me_ppm <- slopes(
  did_ppm,
  newdata = subset(d_personal, treat==1),
  variables = "treat",
  by = "treat"
  )

# adjusted ETWFE model
did_ppm <- fixest::feols(
  as.formula(paste("PM25conc_exposureugm3 ~ ", 
    paste(rhs_dida, collapse= "+"))), 
  data = d_personal, cluster = ~v_id)

# black carbon
# basic ETWFE model
did_pbc <- fixest::feols(
  as.formula(paste("bc_exp_conc ~ ", 
    paste(rhs_did, collapse= "+"))), 
  data = d_personal, cluster = ~v_id)

# adjusted ETWFE model
did_ppm <- fixest::feols(
  as.formula(paste("bc_exp_conc ~ ", 
    paste(rhs_dida, collapse= "+"))), 
  data = d_personal, cluster = ~v_id)


# basic DiD
logit_did <- estimate_logit_did(b_out, rhs_did)
# with covariates
logit_dida <- estimate_logit_did(b_out, rhs_dida)

# estimate marginal effects (simple average)
# basic DiD
logit_me <- lapply(logit_did, marginaleffects::slopes, 
  newdata = subset(dresp, treat==1), 
  variables = "treat", by = "treat",
  # make sure to use cluster-robust SEs
  vcov = ~v_id)

# adjusted DiD
logit_mea <- lapply(logit_dida, marginaleffects::slopes, 
  newdata = subset(dresp, treat==1), 
  variables = "treat", by = "treat",
  # make sure to use cluster-robust SEs
  vcov = ~v_id)

# grab estimates and SEs from DiD results
did_t1 <- logit_me %>% {
  tibble(
    # table = 1,
    outcome = names(logit_me),
    est = map_dbl(., "estimate"),
    stderror = map_dbl(., "std.error"),
    ll = est - 1.96 * stderror,
    ul = est + 1.96 * stderror,
    ci = paste("(", round(ll, 2), ",",
               round(ul, 2), ")")
  )
}

# grab estimates and SEs from adjusted DiD results
did_t2 <- logit_mea %>% {
  tibble(
    # table = 2,
    # outcome = names(logit_me),
    esta = map_dbl(., "estimate"),
    stderrora = map_dbl(., "std.error"),
    lla = esta - 1.96 * stderrora,
    ula = esta + 1.96 * stderrora,
    cia = paste("(", round(lla, 2), ",",
               round(ula, 2), ")")
  )
}

didt <- cbind(did_t1, did_t2) %>%
  dplyr::select(outcome, est, ll, ul, esta, lla, ula) 

didt$outcome = c("Any respiratory symptom", "Coughing",
  "Phlegm", "Wheezing attacks", "Trouble breathing",
  "Chest trouble")

# write results table to dataset
write_rds(didt, file = here("data-clean", 
  "resp-did.rds"))

kable(didt, digits = 2, 
  col.names = c("Frequency of:", "Estimate", 
  "LL", "UL", "Estimate", "LL", "UL")) %>% #, "latex", booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, 
                     "Basic DiD" = 3, "Adjusted DiD" = 3))









# Function to estimate generalized linear regression model for two different sets of predictors
estimate_etwfe <- function(
    lhs, rhs1, rhs2, data, treatvar) {
  
  # Create model formula for model 1
  fml1 <- as.formula(paste(lhs, "~", 
    paste(rhs1, collapse = " + ")))
  
  # Fit model for the first set of predictors
  etwfe1 <- fixest::feols(fml = fml1, 
    data = data, cluster = ~v_id)
  
  # get marginal effects
  me_etwfe1 <- marginaleffects::slopes(
      etwfe1, newdata = subset(data, treat==1),
      variables = "treat", by = "treat")
    
  # test for heterogeneity across treatment cohorts
  me_het1 <- marginaleffects::slopes(
      etwfe1, newdata = subset(data, treat==1),
      variables = "treat", 
      by = c("cohort_year", "year"),
      hypothesis = c("b1 - b2 = 0", "b1 - b3 = 0", 
                     "b1 - b4 = 0"))
    
  # get test and p-value from joint test
  me_jt1 <- hypotheses(me_het1, joint = TRUE)

  # Create model formula for model 1
  fml2 <- as.formula(paste(lhs, "~", 
    paste(rhs2, collapse = " + ")))  
  
  # Fit model for the second set of predictors
  etwfe2 <- fixest::feols(fml = fml2, 
    data = data, cluster = ~v_id)
  
  # get marginal effects
    me_etwfe2 <- marginaleffects::slopes(
      etwfe2, newdata = subset(data, treat==1),
      variables = "treat", by = "treat")
  
  # test for heterogeneity across treatment cohorts
  me_het2 <- marginaleffects::slopes(
      etwfe2, newdata = subset(data, treat==1),
      variables = "treat", 
      by = c("cohort_year", "year"),
      hypothesis = c("b1 - b2 = 0", "b1 - b3 = 0", 
                     "b1 - b4 = 0"))
    
  # get test and p-value from joint test
  me_jt2 <- hypotheses(me_het2, joint = TRUE)
  
  # Return a list containing both models
  return(list(e1 = etwfe1, me_1 = me_etwfe1, 
              e2 = etwfe2, me_2 = me_etwfe2,
              ht1 = me_jt1, ht2 = me_jt2))
}

# ETWFE and adjusted ETWFE models for personal PM
m_ppm <- estimate_etwfe(lhs="PM25conc_exposureugm3", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=d_personal)

m_pbc <- estimate_etwfe(lhs="bc_exp_conc", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=d_bc)



