#  program:  etwfe-pe-bc-analysis.R
#  task:     estimate ETWFE models for personal exposures
#  input:    various .rds files
#  output:   various tables
#  project:  BHET
#  author:   sam harper \ 2024-09-24


## 0 Load needed packages ----
library(here)
library(tidyverse)
library(osfr)
library(fixest)
library(marginaleffects)
library(modelsummary)

# function to pull out marginal predictions
# and effects into a `modelsummary` object
att_table <- function(
    preds, meffs) {
  
  # extract predictions, CI, SE from posterior
  bmp <- data.frame(
  term = preds$treat,
  estimate = preds$estimate,
  conf.low = preds$conf.low,
  conf.high = preds$conf.high,
  std.error = abs(preds$conf.high - 
    preds$conf.low) / (2 * 1.96)
  )
  
  bti <- data.frame(
    term = 2,
    estimate = meffs$estimate,
    conf.low = meffs$conf.low,
    conf.high = meffs$conf.high,
    std.error = abs(meffs$conf.high - 
      meffs$conf.low) / (2 * 1.96)
  )

  bta <- bind_rows(bmp,bti) %>%
    mutate(term = recode_factor(term,
      `0` = "Untreated", `1` = "Treated",
      `2` = "Difference"))
  
  gl <- data.frame()
  
  atts <- list(tidy = bta, glance = gl)
  class(atts) <- "modelsummary_list"
  
  return(atts)
}


## 1 read in clean air pollution datasets ----

# personal PM2.5 and BC
d_p <- read_rds(here("data-clean",
  "ap-data-personal.rds"))



## 2 Model function ----
# function to estimate models, gather marginal effects
# and test for heterogeneity
estimate_etwfe <- function(
    lhs, rhs1, rhs2, data) {
  
  # Create model formula for model 1
  fml1 <- as.formula(paste(lhs, "~", 
    paste(rhs1, collapse = " + ")))
  
  # Fit model for the first set of predictors
  # glm with gamma distribution and log link
  etwfe1 <- fixest::feglm(fml = fml1, 
    data = data, cluster = ~v_id,
    family = Gamma(link = "log"))
  
  # get marginal effects
  me_etwfe1 <- marginaleffects::slopes(
      etwfe1, newdata = subset(data, treat==1),
      variables = "treat", by = "treat")
    
  # estimates across group-time treatment cohorts
  me_het1 <- marginaleffects::slopes(
      etwfe1, newdata = subset(data, treat==1),
      variables = "treat", 
      by = c("cohort_year", "year"))
  
  # now test for heterogeneity
  # for estimates using all 4 waves
  if (nrow(me_het1)==4) {
    # estimate the model contrast
    me_het1_c <- hypotheses(me_het1, hypothesis = 
      c("b1 - b2 = 0", "b1 - b3 = 0", 
                     "b1 - b4 = 0"))
    # get test and p-value from joint test
    me_jt1 <- hypotheses(me_het1_c, joint = TRUE)
  # for estimates using 3 waves
  } else {
    me_het1_c <- hypotheses(me_het1, hypothesis = 
      c("b1 - b2 = 0"))
    me_jt1 <- hypotheses(me_het1_c, joint = TRUE)
  }

  # Create model formula for adjusted model
  fml2 <- as.formula(paste(lhs, "~", 
    paste(rhs2, collapse = " + ")))  
  
  # Fit model for adjusted ETWFE
  etwfe2 <- fixest::feglm(fml = fml2, 
    data = data, cluster = ~v_id,
    family = Gamma(link = "log"))
  
  # get marginal effects
    me_etwfe2 <- marginaleffects::slopes(
      etwfe2, newdata = subset(data, treat==1),
      variables = "treat", by = "treat")
  
  # allow for heterogeneity across treatment cohorts
  me_het2 <- marginaleffects::slopes(
      etwfe2, newdata = subset(data, treat==1),
      variables = "treat", 
      by = c("cohort_year", "year"))
    
  # now test for heterogeneity
  # for estimates using all 4 waves
  if (nrow(me_het2)==4) {
    # estimate the model contrast
    me_het2_c <- hypotheses(me_het2, hypothesis = 
      c("b1 - b2 = 0", "b1 - b3 = 0", 
                     "b1 - b4 = 0"))
  # get test and p-value from joint test
    me_jt2 <- hypotheses(me_het2_c, joint = TRUE)
  # for estimates using 3 waves
  } else {
    me_het2_c <- hypotheses(me_het2, hypothesis = 
      c("b1 - b2 = 0"))
    me_jt2 <- hypotheses(me_het2_c, joint = TRUE)
  }
  
  # Return a list containing both models
  return(list(e1 = etwfe1, me_1 = me_etwfe1,
              meh_1 = me_het1, ht1 = me_jt1, 
              e2 = etwfe2, me_2 = me_etwfe2,
              meh_2 = me_het2, ht2 = me_jt2))
}

# covariates for basic ETWFE specification
rhs_did <- c("treat:cohort_year_2019:year_2019",
  "treat:cohort_year_2019:year_2021", 
  "treat:cohort_year_2020:year_2021",
  "treat:cohort_year_2021:year_2021",
  "cohort_year_2020", "cohort_year_2021",
  "year_2021", "year_2019", "cohort_year_2019")

# ETWFE plus covariates
rhs_dida <- c(rhs_did, "hh_num", "ets_former",
  "ets_lived", "ets_none", "out_temp_24h",
  "out_dew_24h")


## 3 Estimate models for all AP outcomes ----
# personal PM
m_ppm <- estimate_etwfe(lhs="pe", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=d_p)

# black carbon
m_pbc <- estimate_etwfe(lhs="bc_exp_conc", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=d_bc)

# write model results to output folder
write_rds(m_ppm, file = here("outputs/models",
  "m_ppm.rds"))

write_rds(m_pbc, file = here("outputs/models",
  "m_pbc.rds"))


## 3 Generate table of results for all AP outcomes ----

# tables of results by outcome
ap_table_p <- bind_rows(m_ppm$me_1, m_ppm$me_2) %>% 
  mutate(category = "Personal", 
         outcome = "PM2.5")

ap_table_bc <- bind_rows(m_pbc$me_1, m_pbc$me_2) %>% 
  mutate(category = "Personal", 
         outcome = "Black carbon")

# put all subtables together
ap_table1 <- bind_rows(ap_table_p, ap_table_bc,
  ap_table_i24, ap_table_is) %>%
  mutate(model = rep(c(1,2),times=4)) %>%
  select(model, estimate, conf.low, 
    conf.high, category, outcome) %>%
  relocate(category, outcome) %>%
  mutate(ci = paste("(", round(conf.low, 2), ", ",
    round(conf.high, 2), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = model, values_from = 
    c(estimate, ci), names_vary = "slowest")

# now grab outdoor estimates
# download from OSF
aim_1 <- osf_retrieve_node("qsmbr")
aim_1 %>%
  osf_ls_files("Air pollution",
    pattern = "DID_air_pollution.csv") %>%
  osf_download(path = here("data-clean"),
               conflicts = "overwrite")

ap_table2 <- read_csv(here("data-clean", 
  "DID_air_pollution.csv")) %>%
  filter(`Category` == "Outdoor" & `Effect` !=
           "DiD with S3") %>%
  rename_at(c("Category", "Pollutant", "Estimate"), 
    .funs = tolower) %>%
  rename("outcome" = `pollutant`) %>%
  mutate(model = rep(c(1,2), times = 2),
    ci = paste("(", `CI_low`, ", ",
    `CI_upper`, ")", sep="")) %>%
  select(-Effect, -CI_low, -CI_upper) %>%
  relocate(category, outcome) %>%
  pivot_wider(names_from = model, values_from = 
    c(estimate, ci), names_vary = "slowest")

ap_table <- bind_rows(ap_table1, ap_table2)

# write table to output folder
write_rds(ap_table, file = here("outputs", 
  "ap-etwfe-table.rds"))

# html table for design
# kable(ap_table, digits = 2,
#   col.names = c(" ", " ", "Marginal effect", "(95% CI)", 
#    "Marginal effect", "(95% CI)"), #"latex", booktabs = T,
#   linesep = "") %>%
#   kable_styling(full_width = F) %>%
#   collapse_rows(columns = 1:2, valign = "top") %>% 
#   pack_rows("Air pollution", 1, 4) %>%
#   add_header_above(c(" " = 2, 
#                     "DiD" = 2, "Adjusted DiD" = 2))


## 4 Table of heterogeneous treatment effects ----

# tables of results by outcome for personal
ap_table_ph <- bind_rows(m_ppm$me_2, m_ppm$meh_2) %>% 
  mutate(outcome = "PM2.5")

ap_table_bch <- bind_rows(m_pbc$me_2, m_pbc$meh_2) %>% 
  mutate(outcome = "Black carbon")

aph_table <- bind_rows(ap_table_ph, ap_table_bch) %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2019` = "2019",
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", round(conf.low, 2), ", ",
    round(conf.high, 2), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = outcome, values_from = 
    c(estimate, ci), names_vary = "slowest")

# write table to data
write_rds(aph_table, file = here("outputs", 
  "ap-het-table.rds"))

# tables of results by outcome for indoor
ap_table_i24h <- bind_rows(m_i24$me_2, m_i24$meh_2) %>% 
  mutate(outcome = "Daily")

ap_table_ish <- bind_rows(m_is$me_2, m_is$meh_2) %>% 
  mutate(outcome = "Seasonal")

aph_table_i <- bind_rows(ap_table_i24h, ap_table_ish) %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", round(conf.low, 2), ", ",
    round(conf.high, 2), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = outcome, values_from = 
    c(estimate, ci), names_vary = "slowest")

# write table to data
write_rds(aph_table_i, file = here("outputs", 
  "ap-het-table-indoor.rds"))


## 5 Impact of S3 air pollution data ----

# read in indoor seasonal data plus season 3
d_is3 <- read_rds(here("data-clean",
  "ap-data-iseason-s3.rds"))

# ETWFE model estimates including Season 3
m_is_s3 <- fixest::feols(
  pm2.5_indoor_seasonal_hs ~ treat:cohort_year_2020:year_2020 + 
    treat:cohort_year_2020:year_2021 +
    treat:cohort_year_2021:year_2021 + cohort_year_2020 + 
    cohort_year_2021 + year_2021, 
  cluster = ~v_id, data=subset(d_is3, 
    cohort_year_2019!=1))

# overall marginal effect
m_is_s3_me <- slopes(
  m_is_s3,
  newdata = subset(d_is3, 
    cohort_year_2019!=1 & treat==1),
  variables = "treat", by = "treat")

# heterogeneous ATTs
m_is_s3_meh <- slopes(
  m_is_s3,
  newdata = subset(d_is3, 
    cohort_year_2019!=1 & treat==1),
  variables = "treat",
  by = c("cohort_year", "year"))
  

# estimates without Season 3
m_is_nos3 <- fixest::feols(
  pm2.5_indoor_seasonal_hs ~ treat:cohort_year_2020:year_2020 + 
    treat:cohort_year_2020:year_2021 +
    treat:cohort_year_2021:year_2021 + cohort_year_2020 + 
    cohort_year_2021 + year_2021, 
  cluster = ~v_id, data=subset(d_is3, 
    cohort_year_2019!=1 & wave!="S3"))

# overall marginal effect
m_is_nos3_me <- slopes(
  m_is_nos3,
  newdata = subset(d_is3, 
    cohort_year_2019!=1 & wave!="S3" & treat==1),
  variables = "treat", by = "treat")

# heterogeneous ATTs
m_is_nos3_meh <- slopes(
  m_is_nos3,
  newdata = subset(d_is3, 
    cohort_year_2019!=1 & treat==1),
  variables = "treat",
  by = c("cohort_year", "year"))

# Put together the table
ap_ind_s3 <- bind_rows(m_is_s3_me , m_is_s3_meh) %>% 
  mutate(outcome = "wS3")

ap_ind_nos3 <- bind_rows(m_is_nos3_me, m_is_nos3_meh) %>% 
  mutate(outcome = "woS3")

ap_is3_table <- bind_rows(ap_ind_s3, ap_ind_nos3) %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2019` = "2019",
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", round(conf.low, 2), ", ",
    round(conf.high, 2), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = outcome, values_from = 
    c(estimate, ci), names_vary = "slowest")

# write table to data
write_rds(ap_is3_table, file = here("outputs", 
  "ap-is3_table.rds"))


## 6 Impact of removing fixed effects ----

# basic ETWFE model

pe_etwfe <- fixest::feols(
  PM25conc_exposureugm3 ~ 
    treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + factor(smoking) + outdoor_temp_24h + 
    outdoor_dew_24h | cohort_year + year, 
  data = d_p, cluster = ~v_id)

pe_etwfe_me <- slopes(pe_etwfe, 
  newdata = subset(d_p, treat==1), 
  variables = "treat", by = "treat")

write_rds(pe_etwfe_me, file = here("outputs/models", 
  "pe_etwfe_me.rds"))

# no year FE
pe_etwfe_ny <- fixest::feols(
  PM25conc_exposureugm3 ~ 
    treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + factor(smoking) + outdoor_temp_24h + 
    outdoor_dew_24h | cohort_year, 
  data = d_p, cluster = ~v_id)

pe_etwfe_ny_me <- slopes(pe_etwfe_ny, 
  newdata = subset(d_p, treat==1), 
  variables = "treat", by = "treat")

write_rds(pe_etwfe_ny_me, file = here("outputs/models", 
  "pe_etwfe_ny_me.rds"))

# no group FE
pe_etwfe_ng <- fixest::feols(
  PM25conc_exposureugm3 ~ 
    treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + factor(smoking) + outdoor_temp_24h + 
    outdoor_dew_24h | year, 
  data = d_p, cluster = ~v_id)

pe_etwfe_ng_me <- slopes(pe_etwfe_ng, 
  newdata = subset(d_p, treat==1), 
  variables = "treat", by = "treat")

write_rds(pe_etwfe_ng_me, file = here("outputs/models", 
  "pe_etwfe_ng_me.rds"))

# no group or year FE
pe_etwfe_nfe <- fixest::feols(
  PM25conc_exposureugm3 ~ 
    treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + factor(smoking) + outdoor_temp_24h + 
    outdoor_dew_24h, 
  data = d_p, cluster = ~v_id)

pe_etwfe_nfe_me <- slopes(pe_etwfe_nfe, 
  newdata = subset(d_p, treat==1), 
  variables = "treat", by = "treat")

# put the results together in a table
write_rds(pe_etwfe_nfe_me, file = here("outputs/models", 
  "pe_etwfe_nfe_me.rds"))



## distribution tests

d_personal_pred <- d_personal %>% 
  filter(row_number() 
  %in% obs(pe_gamma_feglm)) %>%
  # add predicted E(y)
  add_predictions(pe_gamma_feglm,
    var = "pred_link", type = "link") %>%
  # add predicted E(y) on the response scale
  add_predictions(pe_gamma_feglm,
    var = "pred_response", type = "response") %>%
  add_residuals(pe_gamma_feglm) %>%
  mutate(
    resid_response = (pe - pred_response)^2,
    log_resid_response = log(resid_response),
    log_pred_response = log(pred_response),
    log_scale_resid = log(pe) - pred_link)

d_personal_park <- feglm(
  resid_response ~ pred_link,
  family=Gamma(link = "log"), 
  data = d_personal_pred, cluster = ~v_id)

avg_comparisons(d_personal_park, 
  var = "pred_link", type = "link", 
  hypothesis = c("b1 - 0 = 0",
                 "b1 - 1 = 0", 
                 "b1 - 2 = 0",
                 "b1 - 3 = 0"))


# OLS full model
pe_adj_ols <- feols(
  pe ~ treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 +
    treat:cohort_year_2020:year_2021 +
    treat:cohort_year_2021:year_2021 +
    cohort_year_2019 + cohort_year_2020 +
    cohort_year_2021 + year_2019 + year_2021 +
    hh_num + ets_2 + ets_3 + ets_4 +
    outdoor_temp_24h + outdoor_dew_24h,
    data = d_personal, cluster = ~v_id)

d_personal_pred <- d_personal %>% 
  filter(row_number() 
  %in% obs(pe_adj_ols)) %>%
  # add predicted E(y)
  add_predictions(pe_adj_ols) %>%
  add_residuals(pe_adj_ols)

d_personal_pred %>%
  ggplot(aes(x = pred, y = resid)) +
  geom_point(shape = 1) + theme_classic() +
  geom_smooth(method = 'loess', se = T)

# logged outcome model
# OLS full model
pe_adj_ols_logy <- feols(
  logpe ~ treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 +
    treat:cohort_year_2020:year_2021 +
    treat:cohort_year_2021:year_2021 +
    cohort_year_2019 + cohort_year_2020 +
    cohort_year_2021 + year_2019 + year_2021 +
    hh_num + ets_2 + ets_3 + ets_4 +
    outdoor_temp_24h + outdoor_dew_24h,
    data = d_personal, cluster = ~v_id)

d_personal_pred <- d_personal %>% 
  filter(row_number() 
  %in% obs(pe_adj_ols_logy)) %>%
  add_predictions(pe_adj_ols_logy) %>%
  add_residuals(pe_adj_ols_logy)

d_personal_pred %>%
  ggplot(aes(x = pred, y = resid)) +
  geom_point(shape = 1) + theme_classic() +
  geom_smooth(method = 'loess', se = T)


pe_adj_logn <- feglm(
  pe ~ treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 +
    treat:cohort_year_2020:year_2021 +
    treat:cohort_year_2021:year_2021 +
    cohort_year_2019 + cohort_year_2020 +
    cohort_year_2021 + year_2019 + year_2021 +
    hh_num + ets_2 + ets_3 + ets_4 +
    outdoor_temp_24h + outdoor_dew_24h,
    data = d_personal, cluster = ~v_id,
    family = gaussian(link = "log"))

d_personal_pred <- d_personal %>% 
  filter(row_number() 
  %in% obs(pe_adj_logn)) %>%
  add_predictions(pe_adj_logn) %>%
  add_residuals(pe_adj_logn)

d_personal_pred %>%
  ggplot(aes(x = pred, y = resid)) +
  geom_point(shape = 1) + theme_classic() +
  geom_smooth(method = 'loess', se = T)


pe_adj_gamma <- feglm(
  pe ~ treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 +
    treat:cohort_year_2020:year_2021 +
    treat:cohort_year_2021:year_2021 +
    cohort_year_2019 + cohort_year_2020 +
    cohort_year_2021 + year_2019 + year_2021 +
    hh_num + ets_2 + ets_3 + ets_4 +
    outdoor_temp_24h + outdoor_dew_24h,
    data = d_personal, cluster = ~v_id,
    family = Gamma(link = "log"))

d_personal_pred <- d_personal %>% 
  filter(row_number() 
  %in% obs(pe_adj_gamma)) %>%
  add_predictions(pe_adj_gamma) %>%
  add_residuals(pe_adj_gamma)

d_personal_pred %>%
  ggplot(aes(x = pred, y = resid)) +
  geom_point(shape = 1) + theme_classic() +
  geom_smooth(method = 'loess', se = T)

pe_adj_gamma <- feglm(
  pe ~ treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 +
    treat:cohort_year_2020:year_2021 +
    treat:cohort_year_2021:year_2021 +
    cohort_year_2019 + cohort_year_2020 +
    cohort_year_2021 + year_2019 + year_2021 +
    hh_num + ets_2 + ets_3 + ets_4 +
    outdoor_temp_24h + outdoor_dew_24h,
    data = d_personal, cluster = ~v_id,
    family = Gamma(link = "log"))

d_personal_pred <- d_personal %>% 
  filter(row_number() 
  %in% obs(pe_adj_gamma)) %>%
  add_predictions(pe_adj_gamma) %>%
  add_residuals(pe_adj_gamma)

d_personal_pred %>%
  ggplot(aes(x = pred, y = resid)) +
  geom_point(shape = 1) + theme_classic() +
  geom_smooth(method = 'loess', se = T)



