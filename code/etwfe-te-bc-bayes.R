#  program:  etwfe-te-bc-bayes.R
#  task:     estimate ETWFE models for BC
#  input:    various .rds files
#  output:   various tables
#  project:  BHET
#  author:   sam harper \ 2024-09-11


## 0 Load needed packages ----
library(here)
library(tidyverse)
library(osfr)
library(fixest)
library(marginaleffects)
library(modelsummary)
library(patchwork)
library(brms)
library(bayesplot)
library(tidybayes)
library(cmdstanr)
options(mc.cores = 4,
        brms.backend = "cmdstanr")

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

# personal PM2.5
d_p <- read_rds(here("data-clean",
  "ap-data-personal.rds")) %>%
  mutate(pe = PM25conc_exposureugm3 - 
           mean(PM25conc_exposureugm3))

# Gaussian (default priors)
bc_gaussian <-
  brm(data = d_personal, 
      family = gaussian(),
      bc ~ 1 + (1 | v_id) +
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,        
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 20)

bme_pred <- predictions(
  bc_gaussian, 
  newdata   = subset(d_personal, treat==1),
  variables = "treat", 
  by        = "treat"
  )

bme_avg <- slopes(
  bc_gaussian, 
  newdata   = subset(d_personal, treat==1),
  variables = "treat", 
  by        = "treat"
  ) 

# wrangle aggregate ATTs for model summary table
att_did_bc_g <- att_table(preds = bme_pred, 
  meffs = bme_avg)


# Skew normal (default priors)
bc_skew <-
  brm(data = d_personal, 
      family = skew_normal(),
      bc ~ 1 + (1 | v_id) +
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,        
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 307)

bme_pred <- predictions(
  bc_skew, 
  newdata   = subset(d_personal, treat==1),
  variables = "treat", 
  by        = "treat"
  )

bme_avg <- slopes(
  bc_skew, 
  newdata   = subset(d_personal, treat==1),
  variables = "treat", 
  by        = "treat"
  ) 

# wrangle aggregate ATTs for model summary table
att_did_bc_s <- att_table(preds = bme_pred, 
  meffs = bme_avg)

# Log normal (default priors)
bc_log <-
  brm(data = d_personal, 
      family = lognormal(),
      bc ~ 1 + (1 | v_id) +
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,        
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 570)

bme_pred <- predictions(
  bc_log, 
  newdata   = subset(d_personal, treat==1),
  variables = "treat", 
  by        = "treat"
  )

bme_avg <- slopes(
  bc_log, 
  newdata   = subset(d_personal, treat==1),
  variables = "treat", 
  by        = "treat"
  ) 

# wrangle aggregate ATTs for model summary table
att_did_bc_log <- att_table(preds = bme_pred, 
  meffs = bme_avg)

# Log normal (regularizing priors)
pe_log_inf <-
  brm(data = d_personal, 
      family = lognormal(),
      pe ~ 1 + (1 | v_id) +
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,
      prior = c(prior(normal(4, 4), class = Intercept),
                prior(normal(0, 2), class = b),
                prior(exponential(1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 397)

# Inverse gaussian
bc_log_inv <-
  brm(data = d_personal, 
      family = inverse.gaussian(),
      bc ~ 1 + (1 | v_id) +
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,
      prior = c(prior(normal(4, 4), class = Intercept),
                prior(normal(0, 2), class = b),
                prior(exponential(1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 397)

bme_pred <- predictions(
  pe_log_inf, 
  newdata   = subset(d_personal, treat==1),
  variables = "treat", 
  by        = "treat"
  )

bme_avg <- slopes(
  pe_log_inf, 
  newdata   = subset(d_personal, treat==1),
  variables = "treat", 
  by        = "treat"
  ) 

# wrangle aggregate ATTs for model summary table
att_did_log_inf <- att_table(preds = bme_pred, 
  meffs = bme_avg)

# compare posterior predictive checks
ppc_bc_g <- pp_check(bc_gaussian, ndraws = 100) + 
  xlim(-20, 80) + theme_classic()
ppc_bc_s <- pp_check(bc_skew, ndraws = 100) + 
  xlim(-20, 80) + theme_classic()
ppc_bc_l <- pp_check(bc_log, ndraws = 100) + 
  xlim(-20, 80) + theme_classic()

ppc_bc_g / ppc_bc_s / ppc_bc_l +
  plot_annotation(title = "Posterior predictive checks")


bc_log_feglm <- feglm(
  bc ~ treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021, 
    data = d_personal, 
  family = gaussian(link = "log"), 
  cluster = ~v_id)

bme_pred <- predictions(
  bc_log_feglm, 
  newdata   = subset(d_personal, treat==1),
  variables = "treat", 
  by        = "treat"
  )

bme_avg <- slopes(
  bc_log_feglm, 
  newdata   = subset(d_personal, treat==1),
  variables = "treat", 
  by        = "treat"
  ) 

# wrangle aggregate ATTs for model summary table
att_did_bc_feglm <- att_table(preds = bme_pred, 
  meffs = bme_avg)


# table of marginal estimate
modelsummary(list(
  "Gaussian" = att_did_bc_g,
  "Skew normal" = att_did_bc_s,
  "Lognormal" = att_did_bc_log,
  "FE GLM" = att_did_bc_feglm), 
  fmt=1, 
  statistic = c("std.error", "conf.int"))

# compare Bayesian and frequentist estimates
# for lognormal model

est_bc_log <- get_estimates(bc_log) %>%
  filter(grepl("b_", term)) %>%
  select(term, estimate, mad, conf.low, conf.high) %>%
  rename(std.error = mad)

modelsummary(list(
  "BRMS" = att_did_bc_log,
  "FE GLM" = slopes(bc_log_feglm, 
    newdata = subset(d_personal, treat==1),
    variables = "treat", by = "treat")), 
  fmt=1, 
  statistic = c("std.error", "conf.int"

                
brms_estimates <- data.frame(
  term = paste("ATT(", cs$group, ",", cs$t, ")", sep = ""),
  estimate = cs$att,
  std.error = cs$se)

gl2 <- data.frame()

brms_estimates <- list(tidy = est_bc_log, glance = gl2)

class(brms_estimates) <- "modelsummary_list"

modelsummary(list(
  "BRMS" = brms_estimates,
  "FE GLM" = bc_log_feglm_n),
  coef_rename = 
    c("treat:cohort_year_2019:year_2019" = "b(2019,2019)",
      "treat:cohort_year_2019:year_2021" = "b(2019,2021)",
      "treat:year_2021:cohort_year_2020" = "b(2020,2021)",
      "treat:year_2021:cohort_year_2021" = "b(2021,2021)",
      "b_cohort_year_2019:year_2019:treat" = "b(2019,2019)",
      "b_cohort_year_2019:year_2021:treat" = "b(2019,2021)",
      "b_cohort_year_2020:year_2021:treat" = "b(2020,2021)",
      "b_cohort_year_2021:year_2021:treat" = "b(2021,2021)",
      "b_Intercept" = "(Intercept)",
      "b_cohort_year_2019" = "cohort_year_2019",
      "b_cohort_year_2020" = "cohort_year_2020",
      "b_cohort_year_2021" = "cohort_year_2021",
      "b_year_2019" = "year_2019",
      "b_year_2021" = "year_2021"))





## 2 describe priors for analysis ----

b0 <- 
  brm(data = d_p, 
      family = gaussian(),
      PM25conc_exposureugm3 ~ 1 + treat,
      prior = c(prior(normal(100, 100), class = Intercept),
        prior(normal(0, 10), class = b),
        prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 4400)

b0_default <- 
  brm(data = d_p, 
      family = gaussian(),
      PM25conc_exposureugm3 ~ 1 + treat,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 405)

b0_r <- 
  brm(data = d_p, 
      family = gaussian(),
      PM25conc_exposureugm3 ~ 1 + treat,
      prior = c(prior(normal(100, 50), class = Intercept),
        prior(normal(0, 10), class = b),
        prior(normal(100, 20), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 957)

b0 <- add_criterion(b0, criterion = c("loo", "waic"))
b0_default <- add_criterion(b0_default, 
  criterion = c("loo", "waic"))
b0_r <- add_criterion(b0_r, criterion = c("loo", "waic"))

loo_compare(b0, b0_default, b0_r)

b0_prior <- prior_draws(b0)
b0_default_prior <- prior_draws(b0_default)

b0_posterior <- as_draws_df(b0)
b0_default_posterior <- as_draws_df(b0_default)




),
b1 <-
  brm(data = d_p, 
      family = gaussian(),
      PM25conc_exposureugm3 ~ 1 + treat + (1 | v_id),
      prior = c(prior(normal(100, 100), class = Intercept),
        prior(normal(0, 10), class = b),
        prior(exponential(1), class = sd),
        prior(exponential(1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes", 
      seed = 248)

mcmc_trace(b1, pars="b_Intercept") +
  theme_classic() + ylab("")

pd1 <- prior_draws(b1)
pd2 <- prior_draws(b1c)

b1 <- add_criterion(b1, criterion = "loo")
b1c <- add_criterion(b1c, criterion = "loo")
b1nc <- add_criterion(b1nc, criterion = "loo")

b1nc <-
  brm(data = d_p, 
      family = gaussian(),
      bf(pe ~ a + b, 
        a ~ 1 + (1 | v_id),
        b ~ 0 + treat,
        nl = TRUE),
      prior = c(
        prior(normal(100, 100), class = b, coef = Intercept, nlpar=a),
        prior(normal(0, 10), nlpar = b),
        prior(exponential(1), class = sd, group = v_id, nlpar = a)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes", 
      seed = 486)

loo_compare(b1, b1c)

pd %>%
  ggplot(aes(x = sd_v_id)) + 
    stat_halfeye(color= "black", fill =  '#1b9e77')

b1 %>% 
  spread_draws(sd_v_id__Intercept) %>%
  ggplot(aes(x = sd_v_id__Intercept)) + 
    stat_halfeye(color= "black", fill =  '#1b9e77')
  

summarise_draws(tidy_draws(b1))

      file = "code/fits/bhet-resp-b2")

b2 <-
  brm(data = d2, 
      family = bernoulli(),
      resp ~ 1 + (1 | v_id) + 
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,
      prior = c(prior(normal(0, 1), class = Intercept),
        prior(normal(0, 1), class = b),
        prior(exponential(1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes", 
      seed = 3975,
      file = "code/fits/bhet-resp-b2")

# covariates for basic ETWFE specification
rhs_did <- c("treat:cohort_year_2019:year_2019",
  "treat:cohort_year_2019:year_2021", 
  "treat:cohort_year_2020:year_2021",
  "treat:cohort_year_2021:year_2021",
  "cohort_year_2020", "cohort_year_2021",
  "year_2021", "year_2019", "cohort_year_2019")

# ETWFE plus covariates
rhs_dida <- c(rhs_did, "hh_num", "factor(smoking)", 
  "outdoor_temp_24h", "outdoor_dew_24h")


## 3 Estimate models for all AP outcomes ----
# personal PM
m_ppm <- estimate_etwfe(lhs="PM25conc_exposureugm3", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=d_p)

# ETWFE and adjusted ETWFE models for black carbon
m_pbc <- estimate_etwfe(lhs="bc_exp_conc", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=d_bc)

# ETWFE and adjusted ETWFE models for indoor daily
m_i24 <- estimate_etwfe(lhs="pm2.5_indoor_sensor_24h", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  # restrict to cohorts treated after 2019
  # in the absence of data in season 1
  data=subset(d_i24, cohort_year_2019!=1))

# ETWFE and adjusted ETWFE models for indoor seasonal
m_is <- estimate_etwfe(lhs="pm2.5_indoor_seasonal_hs", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  # restrict to cohorts treated after 2019
  # in the absence of data in season 1
  data=subset(d_is, cohort_year_2019!=1))

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
  mutate(category = "Personal", outcome = "PM2.5",
         nobs = m_ppm$e1$nobs)

ap_table_bc <- bind_rows(m_pbc$me_1, m_pbc$me_2) %>% 
  mutate(category = "Personal", 
         outcome = "Black carbon",
         nobs = m_pbc$e1$nobs)

ap_table_i24 <- bind_rows(m_i24$me_1, m_i24$me_2) %>% 
  mutate(category = "Indoor", 
         outcome = "Daily",
         nobs = m_i24$e1$nobs)

ap_table_is <- bind_rows(m_is$me_1, m_is$me_2) %>% 
  mutate(category = "Indoor", 
         outcome = "Seasonal",
         nobs = m_i24$e1$nobs)

# put all subtables together
ap_table1 <- bind_rows(ap_table_p, ap_table_bc,
  ap_table_i24, ap_table_is) %>%
  mutate(model = rep(c(1,2),times=4)) %>%
  select(model, nobs, estimate, conf.low, 
    conf.high, category, outcome) %>%
  relocate(category, outcome) %>%
  mutate(ci = paste("(", sprintf("%.2f", conf.low), ", ",
    sprintf("%.2f", conf.high), ")", sep="")) %>%
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
    `CI_upper`, ")", sep=""), nobs = NA) %>%
  select(-Effect, -CI_low, -CI_upper) %>%
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
temp_table <- tibble(
  category = "Point", outcome = "Mean", nobs = NA, estimate_1 = 1.96,
  ci_1 = "(0.96, 2.96)", estimate_2 = NA, 
  ci_2 = ""
)

stemp_table <- read_xlsx(here("data-clean",
  "overall_temp_table.xlsx")) %>%
  rename(`estimate_1` = att,
         `ci_1` = ci) %>%
  mutate(category = rep("Seasonal", times = 6),
    outcome = c("Mean (all)", "Mean (daytime)", 
      "Mean (heating season)", "Mean (daytime heating season)",
      "Min. (all)", "Min. (heating season)"), nobs = NA,
    estimate_2 = NA, 
    ci_2 = "") %>%
  select(-model) %>%
  relocate(category, outcome)

# join tables
m_table <- bind_rows(ap_table, temp_table, 
  stemp_table)

colnames(m_table) <- c(" ", " ", "Obs", "ATT", "(95% CI)", 
  "ATT", "(95% CI)")

tt(m_table,
   digits = 2,
  #width = c(3.5, 3, 1, 0.5, 2, 0.5, 2, 0.5, 2, 0.5, 2),
  notes = list("Note: ATT = Average Treatment Effect on the Treated, DiD = Difference-in-Differences, ETWFE = Extended Two-Way Fixed Effects.", 
     a = list(i=0, j=6,
     text = "ETWFE models for air pollution outcomes were adjusted for household size, smoking, outdoor temperature, and outdoor humidity. Temperature models not additionally adjusted."))) %>%
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
  format_tt(j=c(4,6), sprintf = "%.2f") 


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




