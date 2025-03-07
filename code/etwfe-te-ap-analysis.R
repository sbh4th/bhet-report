#  program:  etwfe-te-ap-analysis.R
#  task:     estimate ETWFE models for air pollution
#  input:    various .rds files
#  output:   various tables
#  project:  BHET
#  author:   sam harper \ 2024-10-05


## 0 Load needed packages ----
library(here)
library(tidyverse)
library(osfr)
library(fixest)
library(splines)
library(marginaleffects)
library(modelsummary)
library(tinytable)
library(readxl)


## 1 read in clean air pollution datasets ----

# personal PM2.5 and black carbon
d_p <- read_rds(here("data-clean",
  "ap-data-personal.rds")) %>%
  # limit to complete cases
  drop_na(pe, hh_num, ets, wq, out_temp, out_dew)

d_bc <- read_rds(here("data-clean",
  "ap-data-personal.rds")) %>%
  # limit to complete cases
  drop_na(bc, hh_num, ets, wq, out_temp, out_dew)

# indoor daily
d_i24 <- read_rds(here("data-clean",
  "ap-data-i24h.rds")) %>%
  # limit to complete cases
  drop_na(i24, hh_num, ets, wq, out_temp, out_dew) %>%
  # restrict to cohorts treated after 2019
  # since no data in 2018
  filter(cohort_year_2019!=1)

# indoor seasonal
d_is <- read_rds(here("data-clean",
  "ap-data-iseason.rds")) %>%
  # limit to complete cases
  drop_na(is, hh_num, ets, wq, out_temp, out_dew, out_rh) %>%
  # restrict to cohorts treated after 2019
  # since no data in 2018
  filter(cohort_year_2019!=1)


## 2 define a function to estimate the and adjusted 
## ETWFE models, estimate the marginal effects 
## (including heterogeneity) and put results in a list ----

estimate_etwfe <- function(
    lhs, rhs1, rhs2, data) {
  
  # Create model formula for model 1
  fml1 <- as.formula(paste(lhs, "~", 
    paste(rhs1, collapse = " + ")))
  
  # Fit gamma model for the first set of predictors
  # (see personal-models.qmd for details)
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
# personal PM and BC
rhs_did <- c("treat:cohort_year_2019:year_2019",
  "treat:cohort_year_2019:year_2021", 
  "treat:cohort_year_2020:year_2021",
  "treat:cohort_year_2021:year_2021",
  "cohort_year_2019", "cohort_year_2020", 
  "cohort_year_2021", "year_2019",
  "year_2021")

# indoor daily and seasonal
# drop 2019 cohort and year dummies
rhs_didi <- c("treat:cohort_year_2020:year_2021",
  "treat:cohort_year_2021:year_2021", 
  "cohort_year_2020", "cohort_year_2021", 
  "year_2021")

# ETWFE plus covariates
rhs_dida <- c(rhs_did, "hh_num", "ets_former",
  "ets_lived", "ets_none", 
  "ns(out_temp, df=2)", 
  "ns(out_dew, df=2)")

# ETWFE plus covariates
rhs_didia <- c(rhs_didi, "hh_num", "ets_former",
  "ets_lived", "ets_none",
  "ns(out_temp, df=2)", 
  "ns(out_dew, df=2)")


## 3 Estimate models for all AP outcomes ----
# personal PM
m_ppm <- estimate_etwfe(lhs="pe", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=d_p)

# ETWFE and adjusted ETWFE models for black carbon
m_pbc <- estimate_etwfe(lhs="bc", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=d_bc)

# ETWFE and adjusted ETWFE models for indoor daily
m_i24 <- estimate_etwfe(lhs="i24", 
  rhs1=rhs_didi, rhs2=rhs_didia, 
  data=d_i24)

# ETWFE and adjusted ETWFE models for indoor seasonal
m_is <- estimate_etwfe(lhs="is", 
  rhs1=rhs_didi, rhs2=rhs_didia, 
  data=d_is)

# write model results to output folder
write_rds(m_ppm, file = here("outputs/models",
  "m_ppm.rds"))

write_rds(m_pbc, file = here("outputs/models",
  "m_pbc.rds"))

write_rds(m_i24, file = here("outputs/models",
  "m_i24.rds"))

write_rds(m_is, file = here("outputs/models",
  "m_is.rds"))


## 3 Generate table of results for all AP outcomes ----

# tables of results by outcome
ap_table_p <- bind_rows(m_ppm$me_1, m_ppm$me_2) %>% 
  mutate(category = "Personal", 
         outcome = "PM2.5",
         nobs = m_ppm$e1$nobs)

ap_table_bc <- bind_rows(m_pbc$me_1, m_pbc$me_2) %>% 
  mutate(category = "Personal", 
         outcome = "Black carbon",
         nobs = m_pbc$e1$nobs)

ap_table_i24 <- bind_rows(m_i24$me_1, m_i24$me_2) %>% 
  mutate(category = "Indoor", 
         outcome = "24-hr PM2.5",
         nobs = m_i24$e1$nobs)

ap_table_is <- bind_rows(m_is$me_1, m_is$me_2) %>% 
  mutate(category = "Indoor", 
         outcome = "Seasonal PM2.5",
         nobs = m_is$e1$nobs)

# put all subtables together
ap_table1 <- bind_rows(ap_table_p, ap_table_bc,
  ap_table_i24, ap_table_is) %>%
  mutate(model = rep(c(1,2),times=4)) %>%
  select(model, nobs, estimate, conf.low, 
    conf.high, category, outcome) %>%
  relocate(category, outcome) %>%
  mutate(ci = paste("(", sprintf("%.1f", conf.low), ", ",
    sprintf("%.1f", conf.high), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = model, values_from = 
    c(estimate, ci), names_vary = "slowest")

# now grab outdoor estimates
# download from OSF
# aim_1 <- osf_retrieve_node("qsmbr")
# aim_1 %>%
#   osf_ls_files("Air pollution",
#     pattern = "DID_air_pollution.csv") %>%
#   osf_download(path = here("data-clean"),
#                conflicts = "overwrite")

ap_table2 <- read_csv(here("outputs", 
  "DID_air_pollution_gamma.csv")) %>%
  filter(`Category` == "Outdoor" & `Sample` ==
           "No S3") %>%
  mutate(Pollutant = c("24-hr PM2.5",
    "24-hr PM2.5", 
    "Seasonal PM2.5",
    "Seasonal PM2.5")) %>%
  rename_at(c("Category", "Pollutant", "Estimate"), 
    .funs = tolower) %>%
  rename("outcome" = `pollutant`) %>%
  mutate(model = rep(c(1,2), times = 2),
    ci = paste("(", sprintf("%.1f", `CI_low`), ", ",
    sprintf("%.1f", `CI_upper`), ")", sep=""), 
    nobs = c(11174, 11174, 139, 139)) %>%
  select(-Effect, -CI_low, -CI_upper, -Sample) %>%
  relocate(category, outcome) %>%
  pivot_wider(names_from = model, values_from = 
    c(estimate, ci), names_vary = "slowest")

ap_table <- bind_rows(ap_table1, ap_table2)

# write table to output folder
write_rds(ap_table, file = here("outputs", 
  "ap-etwfe-table.rds"))

ap_table <- read_rds(here("outputs", 
  "ap-etwfe-table.rds")) 

# temperature results
stemp_table <- read_xlsx(here("outputs",
  "marginal_temp_results.xlsx")) %>%
  mutate(
    # Extract mean
    estimate = as.double(str_extract(att, "^[^\\(]+")),                 # Extract lower bound
    ci_lower = as.double(str_extract(att, "(?<=\\().+?(?=,)")),         # Extract upper bound
    ci_upper = as.double(str_extract(att, "(?<=, ).+?(?=\\))")),
    # rename model for reshape
    model = case_when(
      `Covariate adjustment` == "DiD" ~ 1,
      `Covariate adjustment` != "DiD" ~ 2,),
    nobs = as.integer(`N observations`)) %>%
  mutate(ci = paste("(", sprintf("%.1f", ci_lower), ", ",
      sprintf("%.1f", ci_upper), ")", sep="")) %>%
  select(-`Covariate adjustment`, -att, -ci_lower,
         -ci_upper, -`N observations`) %>%
  # reshape to wide
  pivot_wider(names_from = model, 
    values_from = c(estimate, ci)) %>%
  mutate(order = c(2:7, 1)) %>%
  arrange(order) %>%
  mutate(category = c("Point", rep("Seasonal", times = 6)),
    outcome = c("Mean", "Mean (all)", "Mean (daytime)", 
      "Mean (heating season)", "Mean (daytime heating season)",
      "Min. (all)", "Min. (heating season)")) %>%
  select(-`Outcome metric`, -order) %>%
  relocate(category, outcome, nobs, estimate_1,
           ci_1, estimate_2, ci_2)

# join tables
m_table <- bind_rows(ap_table, stemp_table)

colnames(m_table) <- c(" ", " ", "Obs", "ATT", "(95% CI)", 
  "ATT", "(95% CI)")

tt(m_table,
   digits = 2,
  #width = c(3.5, 3, 1, 0.5, 2, 0.5, 2, 0.5, 2, 0.5, 2),
  notes = list("Note: ATT = Average Treatment Effect on the Treated, DiD = Difference-in-Differences, ETWFE = Extended Two-Way Fixed Effects.",
     a = list(i=0, j=6,
     text = "ETWFE models for air pollution outcomes were adjusted for household size, smoking, outdoor temperature, and outdoor humidity. Temperature models not additionally adjusted."),
     b = list(i=3, j = 2, 
     text = "The indoor 24-hr PM2.5 concentration was determined over the time period concurrent with when the personal PM2.5 concentration was determined."))) %>%
  group_tt(
    j = list("DiD" = 4:5, 
             "Adjusted DiD" = 6:7),
    i = list("Air pollution" = 1, 
             "Indoor temperature" = 7)) %>%
  style_tt(i = c(1, 8), align = "l", bold=T) %>%
  style_tt(
    i = c(2, 4, 6), j = 1, 
    rowspan = 2, alignv = "t") %>%
  style_tt(
    i = 10, j = 1, rowspan = 6, alignv = "t") %>%
  style_tt(j = 1:7, align = "llccccc") %>%
  format_tt(escape = TRUE) %>%
  format_tt(j=c(4,6), sprintf = "%.1f") 


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
  mutate(ci = paste("(", sprintf("%.1f", conf.low), ", ",
    sprintf("%.1f", conf.high), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = outcome, values_from = 
    c(estimate, ci), names_vary = "slowest")

# write table to data
write_rds(aph_table, file = here("outputs", 
  "ap-het-table.rds"))

# tables of results by outcome for indoor
ap_table_i24h <- bind_rows(m_i24$me_2, m_i24$meh_2) %>% 
  mutate(outcome = "24-hr PM2.5")

ap_table_ish <- bind_rows(m_is$me_2, m_is$meh_2) %>% 
  mutate(outcome = "Seasonal PM2.5")

aph_table_i <- bind_rows(ap_table_i24h, ap_table_ish) %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", sprintf("%.1f", conf.low), ", ",
    sprintf("%.1f", conf.high), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = outcome, values_from = 
    c(estimate, ci), names_vary = "slowest")

# write table to data
write_rds(aph_table_i, file = here("outputs", 
  "ap-het-table-indoor.rds"))


## 5 Impact of S3 air pollution data ----

# read in indoor seasonal data plus season 3
d_is3 <- read_rds(here("data-clean",
  "ap-data-iseason-s3.rds")) %>%
  # restrict to cohorts treated after 2019
  # since no data in 2018
  filter(cohort_year_2019!=1)

# ETWFE model estimates including Season 3
m_is_s3 <- fixest::feglm(
  is ~ treat:cohort_year_2020:year_2020 + 
    treat:cohort_year_2020:year_2021 +
    treat:cohort_year_2021:year_2021 + cohort_year_2020 + 
    cohort_year_2021 + year_2021, 
  cluster = ~v_id, data=d_is3, 
  family = Gamma(link = "log"))

# overall marginal effect
m_is_s3_me <- slopes(
  m_is_s3,
  newdata = subset(d_is3, treat==1),
  variables = "treat", by = "treat")

# heterogeneous ATTs
m_is_s3_meh <- slopes(
  m_is_s3,
  newdata = subset(d_is3, treat==1),
  variables = "treat",
  by = c("cohort_year", "year"))
  

# estimates without Season 3
m_is_nos3 <- fixest::feglm(
  is ~ treat:cohort_year_2020:year_2020 + 
    treat:cohort_year_2020:year_2021 +
    treat:cohort_year_2021:year_2021 + cohort_year_2020 + 
    cohort_year_2021 + year_2021, 
  cluster = ~v_id, data=subset(d_is3, 
    wave != 3), family = Gamma(link = "log"))

# overall marginal effect
m_is_nos3_me <- slopes(
  m_is_nos3,
  newdata = subset(d_is3, wave != 3 & treat==1),
  variables = "treat", by = "treat")

# heterogeneous ATTs
m_is_nos3_meh <- slopes(
  m_is_nos3,
  newdata = subset(d_is3, wave != 3 & treat==1),
  variables = "treat",
  by = c("cohort_year", "year"))

# Put together the table
ap_ind_s3 <- bind_rows(m_is_s3_me , m_is_s3_meh) %>% 
  mutate(outcome = "wS3",
         nobs = m_is_s3$nobs)

ap_ind_nos3 <- bind_rows(m_is_nos3_me, m_is_nos3_meh) %>% 
  mutate(outcome = "woS3",
         nobs = m_is_nos3$nobs)

ap_is3_table <- bind_rows(ap_ind_s3, ap_ind_nos3) %>%
  select(outcome, nobs, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2019` = "2019",
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year, nobs) %>%
  mutate(ci = paste("(", sprintf("%.1f", conf.low), ", ",
    sprintf("%.1f", conf.high), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = outcome, values_from = 
    c(nobs, estimate, ci), names_vary = "slowest")

# write table to data
write_rds(ap_is3_table, file = here("outputs", 
  "ap-is3_table.rds"))


## 6 Impact of removing fixed effects ----

# adjusted ETWFE model

pe_etwfe <- fixest::feglm(
  pe ~ 
    treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + ets_former + ets_lived +
    ets_none + ns(out_temp, df=2) + 
    ns(out_dew, df=2) | cohort_year + year, 
  data = d_p, cluster = ~v_id,
  family = Gamma(link = "log"))

pe_etwfe_me <- slopes(pe_etwfe, 
  newdata = subset(d_p, treat==1), 
  variables = "treat", by = "treat")

write_rds(pe_etwfe_me, file = here("outputs/models", 
  "pe_etwfe_me.rds"))

# no year FE
pe_etwfe_ny <- fixest::feglm(
  pe ~ 
    treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + ets_former + ets_lived +
    ets_none + ns(out_temp, df=2) + 
    ns(out_dew, df=2) | cohort_year, 
  data = d_p, cluster = ~v_id,
  family = Gamma(link = "log"))

pe_etwfe_ny_me <- slopes(pe_etwfe_ny, 
  newdata = subset(d_p, treat==1), 
  variables = "treat", by = "treat")

write_rds(pe_etwfe_ny_me, file = here("outputs/models", 
  "pe_etwfe_ny_me.rds"))

# no group FE
pe_etwfe_ng <- fixest::feglm(
  pe ~ 
    treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + ets_former + ets_lived +
    ets_none + ns(out_temp, df=2) + 
    ns(out_dew, df=2) | year, 
  data = d_p, cluster = ~v_id,
  family = Gamma(link = "log"))

pe_etwfe_ng_me <- slopes(pe_etwfe_ng, 
  newdata = subset(d_p, treat==1), 
  variables = "treat", by = "treat")

write_rds(pe_etwfe_ng_me, file = here("outputs/models", 
  "pe_etwfe_ng_me.rds"))

# no group or year FE
pe_etwfe_nfe <- fixest::feglm(
  pe ~ 
    treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + ets_former + ets_lived +
    ets_none + ns(out_temp, df=2) + 
    ns(out_dew, df=2), 
  data = d_p, cluster = ~v_id,
  family = Gamma(link = "log"))

pe_etwfe_nfe_me <- slopes(pe_etwfe_nfe, 
  newdata = subset(d_p, treat==1), 
  variables = "treat", by = "treat")

# put the results together in a table
write_rds(pe_etwfe_nfe_me, file = here("outputs/models", 
  "pe_etwfe_nfe_me.rds"))





# ETWFE plus covariates
rhs_didia <- c(rhs_didi, "hh_num", "ets_former",
  "ets_lived", "ets_none",
  "ns(out_temp, df=2)", 
  "ns(out_dew, df=2)")


## X Adjustment for district ----

# adding district
rhs_didiad <- c(rhs_didi, "hh_num", "ets_former",
  "ets_lived", "ets_none",
  "ns(out_temp, df=2)", 
  "ns(out_dew, df=2)", "factor(ID_COUNTY)")

# personal PM
m_ppm_d <- estimate_etwfe(lhs="pe", 
  rhs1=rhs_dida, rhs2=rhs_didiad, 
  data=d_p)

# ETWFE and adjusted ETWFE models for black carbon
m_pbc_d <- estimate_etwfe(lhs="bc", 
  rhs1=rhs_dida, rhs2=rhs_didiad, 
  data=d_bc)

# ETWFE and adjusted ETWFE models for indoor daily
m_i24_d <- estimate_etwfe(lhs="i24", 
  rhs1=rhs_didia, rhs2=rhs_didiad, 
  data=d_i24)

# ETWFE and adjusted ETWFE models for indoor seasonal
m_is_d <- estimate_etwfe(lhs="is", 
  rhs1=rhs_didia, rhs2=rhs_didiad, 
  data=d_is)

# write model results to output folder
write_rds(m_ppm, file = here("outputs/models",
  "m_ppm.rds"))

write_rds(m_pbc, file = here("outputs/models",
  "m_pbc.rds"))

write_rds(m_i24, file = here("outputs/models",
  "m_i24.rds"))

write_rds(m_is, file = here("outputs/models",
  "m_is.rds"))

