#  program:  resp-outcomes-mediation.R
#  task:     mediation models for respiratory outcomes
#  input:    bhet-master
#  output:   
#  project:  BHET
#  author:   sam harper \ 2024-10-04

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
  filter(PM25_exp_remove == 1) %>% 
  drop_na(ppm25, temp) %>%
  mutate(ppm25 = ppm25 / 10)



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
    mutate(temp = mv_temp)

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
    ci = paste("(", sprintf("%.2f", ll), ", ",
      sprintf("%.2f", ul), ")", sep="")
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
    ci1 = paste("(", sprintf("%.2f", ll1), ", ",
      sprintf("%.2f", ul1), ")", sep="")
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
    ci2 = paste("(", sprintf("%.2f", ll2), ", ",
      sprintf("%.2f", ul2), ")", sep="")
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
    ci3 = paste("(", sprintf("%.2f", ll3), ", ",
      sprintf("%.2f", ul3), ")", sep="")
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

kable(cdet, digits = 2, 
  col.names = c("", "ATT", "(95%CI)",
  "ATT", "(95%CI)", "ATT", "(95%CI)", "ATT", "(95%CI)")) %>%
  kable_styling() %>%
  add_header_above(c(" " = 3, 
  "Indoor PM" = 2, "Indoor Temp" = 2, "PM + Temp" = 2)) %>%
  add_header_above(c(" " = 1, 
    "Adjusted Total Effect" = 2,
    "CDE Mediated By:" = 6))

# table of marginal effects
modelsummary(list("Any symptom" = logit_me$resp,
  "Cough" = logit_me$cough, "Phlegm" = logit_me$phlegm,
  "Wheezing" = logit_me$wheeze,
  "Shortness of breath" = logit_me$breath,
  "Chest trouble" = logit_me$nochest,
  "Any symptom" = logit_mea$resp,
  "Cough" = logit_mea$cough, "Phlegm" = logit_mea$phlegm,
  "Wheezing" = logit_mea$wheeze,
  "Shortness of breath" = logit_mea$breath,
  "Chest trouble" = logit_mea$nochest),
  group = model ~ term + statistic,
  statistic = 'conf.int')

didtable <- modelsummary(list("Any" = logit_me$resp, "Cough" = logit_me$cough), group = model ~ term + statistic,
  statistic = 'conf.int')

modelsummary(list("Any" = logit_me$resp, "Cough" = logit_me$cough), group =  ~ "" + statistic,
  statistic = 'conf.int')

didatable <- modelsummary(list("Any" = logit_mea$resp, "Cough" = logit_mea$cough), group = model ~ term + statistic,
  statistic = 'conf.int')

data_tables <- data.frame(good_table = didtable, 
                          bad_table = didatable)



# any respiratory outcome
resp_het_any <- bind_rows(logit_mea$resp, 
                          logit_mea_het$resp) %>% 
  mutate(outcome = "resp") %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2019` = "2019",
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", sprintf('%.2f', conf.low), ", ",
    sprintf('%.2f', conf.high), ")", sep="")) %>%
  select(-conf.low, -conf.high) 


# write table to data
write_rds(resp_het_any, file = here("outputs", 
  "resp-het-resp.rds"))

# heterogeneity table for any respiratory outcome

# cough
resp_het_cough <- bind_rows(logit_mea$cough, 
                          logit_mea_het$cough) %>% 
  mutate(outcome = "cough") %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2019` = "2019",
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", sprintf('%.2f', conf.low), ", ",
    sprintf('%.2f', conf.high), ")", sep="")) %>%
  select(-conf.low, -conf.high) 

# write table to data
write_rds(resp_het_cough, file = here("outputs", 
  "resp-het-cough.rds"))

# phlegm
resp_het_phlegm <- bind_rows(logit_mea$phlegm, 
                          logit_mea_het$phlegm) %>% 
  mutate(outcome = "phlegm") %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2019` = "2019",
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", sprintf('%.2f', conf.low), ", ",
    sprintf('%.2f', conf.high), ")", sep="")) %>%
  select(-conf.low, -conf.high) 

# write table to data
write_rds(resp_het_phlegm, file = here("outputs", 
  "resp-het-phlegm.rds"))


# wheeze
resp_het_wheeze <- bind_rows(logit_mea$wheeze, 
                          logit_mea_het$wheeze) %>% 
  mutate(outcome = "wheeze") %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2019` = "2019",
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", sprintf('%.2f', conf.low), ", ",
    sprintf('%.2f', conf.high), ")", sep="")) %>%
  select(-conf.low, -conf.high) 

# write table to data
write_rds(resp_het_wheeze, file = here("outputs", 
  "resp-het-wheeze.rds"))

# shortness of breah
resp_het_breath <- bind_rows(logit_mea$breath, 
                          logit_mea_het$breath) %>% 
  mutate(outcome = "breath") %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2019` = "2019",
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", sprintf('%.2f', conf.low), ", ",
    sprintf('%.2f', conf.high), ")", sep="")) %>%
  select(-conf.low, -conf.high) 

# write table to data
write_rds(resp_het_breath, file = here("outputs", 
  "resp-het-breath.rds"))

# no chest trouble
resp_het_nochest <- bind_rows(logit_mea$nochest, 
                          logit_mea_het$nochest) %>% 
  mutate(outcome = "chest") %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2019` = "2019",
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", sprintf('%.2f', conf.low), ", ",
    sprintf('%.2f', conf.high), ")", sep="")) %>%
  select(-conf.low, -conf.high) 

# write table to data
write_rds(resp_het_nochest, file = here("outputs", 
  "resp-het-nochest.rds"))