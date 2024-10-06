#  program:  resp-outcomes-mediation.R
#  task:     mediation models for respiratory outcomes
#  input:    bhet-master
#  output:   
#  project:  BHET
#  author:   sam harper \ 2024-10-05

# packages to load
pkgs <- c('here', 'tidyverse', 'modelsummary', 
          'fixest', 'marginaleffects',
          'kableExtra', 'patchwork')

#load all packages at once
lapply(pkgs, library, character.only=TRUE)

## 1 Read in dataset, limit to resp vars ----
dresp <- read_rds(here("data-clean", 
  "bhet-resp-data.rds"))

# limit to sample with mediators and measures of PM
dr_med <- dresp %>%
  filter(p_usable_pm == 1) %>% 
  drop_na(ppm25, temp) %>%
  mutate(ppm25 = ppm25 / 10,
         ctemp = temp - mean(temp))



## 2 Estimate DD across several binary outcomes ----

# Function to estimate GLM for multiple outcomes
estimate_logit_med <- function(outcome_vars, predictor_vars) {
  models <- outcome_vars %>%
    set_names() %>%
    map(~ {
      formula <- as.formula(paste(.x, "~", 
        paste(predictor_vars, collapse = " + ")))
      glm(formula, data = dr_med, family = "binomial")
    })
  
  return(models)
}

# binary outcomes
b_out <- c("resp", "cough", "phlegm", "wheeze",
              "breath", "nochest")

# basic DiD specification
rhs_ct <- c("treat:cohort_year_2019:year_2019",
  "treat:cohort_year_2019:year_2021", 
  "treat:cohort_year_2020:year_2021",
  "treat:cohort_year_2021:year_2021")

rhs_fe <- c("cohort_year_2019", "cohort_year_2020", 
  "cohort_year_2021", "year_2019", "year_2021")

# rhs_cov <- c("age_health", "male", 
#   "csmoke", "fsmoke")

rhs_cov <- c("age_health", "male", 
  "ets_2", "ets_3", "ets_4",
  "occ_2", "occ_3", "occ_4",
  "drink_2", "drink_3", "drink_4",
  "farm_2", "farm_3", "farm_4")

rhs_meda <- c(rhs_ct, rhs_fe, rhs_cov)

rhs_meda1 <- c(rhs_ct, "ppm25 * cohort_year_2019",
  "ppm25 * cohort_year_2020", "ppm25 * cohort_year_2021",
  "ppm25 * year_2019", "ppm25 * year_2021", rhs_cov)

rhs_meda2 <- c(rhs_ct, "temp * cohort_year_2019",
  "temp * cohort_year_2020", "temp * cohort_year_2021", 
  "temp * year_2019", "temp * year_2021", rhs_cov)

rhs_meda3 <- c(rhs_meda, "ppm25 * cohort_year_2019",
  "ppm25 * cohort_year_2020", "ppm25 * cohort_year_2021",
  "ppm25 * year_2019", "ppm25 * year_2021", 
  "temp * cohort_year_2019", "temp * cohort_year_2020", 
  "temp * cohort_year_2021", "temp * year_2019", 
  "temp * year_2021", rhs_cov)

# gather estimates across models
# adjusted ETWFE model
logit_meda <- estimate_logit_med(b_out, rhs_meda)

# mediated by personal PM2.5
logit_meda1 <- estimate_logit_med(b_out, rhs_meda1)

# mediated by point temperature
logit_meda2 <- estimate_logit_med(b_out, rhs_meda2)

# mediated by both PM2.5 and point temp
logit_meda3 <- estimate_logit_med(b_out, rhs_meda3)

# estimate marginal effects (simple average)
# adjusted ETWFE
logit_me_meda <- lapply(
  logit_meda, marginaleffects::slopes, 
  newdata = subset(dr_med, treat==1), 
  variables = "treat", by = "treat",
  # make sure to use cluster-robust SEs
  vcov = ~v_id)



## 3 Estimate CDE across outcomes ----

# marginal CDE for PM2.5
# first get the value of PM2.5 for untreated
# at baseline
mv_pm25 <- dr_med %>% 
  filter(wave==1 & cohort_year==-Inf) %>% 
  summarize(mean_ppm = mean(ppm25, na.rm = T))

# create dataset setting value of mediator
cde_pm <- 
    subset(dr_med, treat == 1) %>%
    mutate(ppm25 = mv_pm25)

logit_me_meda1 <- lapply(
  logit_meda1, 
  marginaleffects::slopes, 
  newdata = cde_pm, 
  variables = "treat", by = "treat",
  # make sure to use cluster-robust SEs
  vcov = ~v_id)

# value of point temp for untreated
# at baseline
mv_temp <- dr_med %>% 
  filter(wave==1 & cohort_year==-Inf) %>% 
  summarize(mean_temp = mean(temp, na.rm = T))

# create dataset setting value of mediator
cde_temp <- 
    subset(dr_med, treat == 1) %>%
    mutate(ctemp = mv_temp)

# marginal effect for CDE
logit_me_meda2 <- lapply(
  logit_meda2, 
  marginaleffects::slopes, 
  newdata = cde_temp, 
  variables = "treat", 
  by = "treat",
  # make sure to use cluster-robust SEs
  vcov = ~v_id)

# set values for both mediators
# create dataset setting value of mediator
cde_mm <- 
    subset(dr_med, treat == 1) %>%
    mutate(ppm25 = mv_pm25,
           temp = mv_temp)

# marginal effect for CDE
logit_me_meda3 <- lapply(
  logit_meda3, 
  marginaleffects::slopes, 
  newdata = cde_mm, 
  variables = "treat", 
  by = "treat",
  # make sure to use cluster-robust SEs
  vcov = ~v_id)


# grab estimates and SEs from total effect
cde_t1 <- logit_me_meda %>% {
  tibble(
    # table = 1,
    outcome = names(logit_me_meda),
    est = map_dbl(., "estimate") * 100,
    stderror = map_dbl(., "std.error") * 100,
    ll = est - 1.96 * stderror,
    ul = est + 1.96 * stderror,
    ci = paste("(", sprintf("%.1f", ll), ", ",
      sprintf("%.1f", ul), ")", sep="")
  ) %>%
    select(outcome, est, ci)
}

# grab estimates and SEs from mediation by PM
cde_t2 <- logit_me_meda1 %>% {
  tibble(
    est1 = map_dbl(., "estimate") * 100,
    stderror1 = map_dbl(., "std.error") * 100,
    ll1 = est1 - 1.96 * stderror1,
    ul1 = est1 + 1.96 * stderror1,
    ci1 = paste("(", sprintf("%.1f", ll1), ", ",
      sprintf("%.1f", ul1), ")", sep="")
  ) %>%
    select(est1, ci1)
}

# grab estimates and SEs from mediation by temp
cde_t3 <- logit_me_meda2 %>% {
  tibble(
    est2 = map_dbl(., "estimate") * 100,
    stderror2 = map_dbl(., "std.error") * 100,
    ll2 = est2 - 1.96 * stderror2,
    ul2 = est2 + 1.96 * stderror2,
    ci2 = paste("(", sprintf("%.1f", ll2), ", ",
      sprintf("%.1f", ul2), ")", sep="")
  )%>%
    select(est2, ci2)
}

# grab estimates and SEs from mediation by both PM/temp 
cde_t4 <- logit_me_meda3 %>% {
  tibble(
    est3 = map_dbl(., "estimate") * 100,
    stderror3 = map_dbl(., "std.error") * 100,
    ll3 = est3 - 1.96 * stderror3,
    ul3 = est3 + 1.96 * stderror3,
    ci3 = paste("(", sprintf("%.1f", ll3), ", ",
      sprintf("%.1f", ul3), ")", sep="")
  ) %>%
    select(est3, ci3)
}

cdet <- cbind(cde_t1, cde_t2, cde_t3, cde_t4) %>%
  mutate(outcome = c("Any symptom", "Coughing",
    "Phlegm", "Wheezing attacks", "Trouble breathing",
    "Chest trouble"))
  
# write results table to dataset
write_rds(cdet, file = here("outputs", 
  "resp-cdes.rds"))

