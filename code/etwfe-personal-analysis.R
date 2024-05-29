#  program:  etwfe-personal-analysis.R
#  task:     estimate ETWFE models for personal air pollution
#  input:    various .rds files
#  output:   various tables
#  project:  BHET
#  author:   sam harper \ 2024-05-07


## 0 Load needed packages ----
library(here)
library(tidyverse)
library(osfr)
library(fixest)
library(marginaleffects)
library(modelsummary)
library(kableExtra)
library(brms)
library(tidybayes)
library(bayesplot)
library(patchwork)
# Use the cmdstanr backend for Stan
# You need to install the cmdstanr package first
# (https://mc-stan.org/cmdstanr/) and then run cmdstanr::install_cmdstan() to
# install cmdstan on your computer.
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
  mutate(smoker = ifelse(smoking=="Smoker", 1, 0),
    lwsmoker = ifelse(smoking=="Live with smoker", 1, 0))

# personal black carbon
d_bc <- read_rds(here("data-clean",
  "ap-data-bc.rds"))


## 2 define and vizualize priors for personal exposure ----
## only including treatment and random effect for village
bp <-
  brm(data = d_p, 
      family = gaussian(),
      PM25conc_exposureugm3 ~ 1 + treat + (1 | v_id),
      prior = c(prior(normal(80, 10), class = Intercept),
                prior(normal(0, 5), class = b),
                prior(exponential(1), class = sd)),        
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 287,
      file = "code/fits/personal-priors")

bp <- add_criterion(bp, criterion = "loo")

bp_sn20 <-
  brm(data = d_p, 
      family = skew_normal(),
      PM25conc_exposureugm3 ~ 1 + treat + (1 | v_id),
      prior = c(prior(skew_normal(100, 150, 20), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(exponential(1), class = sd)),        
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 2388,
      file = "code/fits/personal-priors-sn20")

bp_sn20 <- add_criterion(bp_sn20, criterion = "loo")

l <- loo_compare(bp, bp_sn, bp_sn10, bp_sn20, criterion = "loo")
print(l, simplify = F)

# visualize the priors

bp_sn_p <- prior_draws(bp_sn20) 

# Intercept
bp_sn_p %>% ggplot(aes(x = Intercept)) + 
  stat_halfeye(color= "black", fill =  '#1b9e77') +
  labs(x = "Parameter value", 
    y = "Density") +
  theme_classic()

# treatment
bp_sn_p %>% ggplot(aes(x = b)) + 
  stat_halfeye(color= "black", fill =  '#1b9e77') +
  labs(x = "Parameter value", 
    y = "Density") +
  theme_classic()

# check the chains

bp_sn20 %>% 
  plot(variable = "b_Intercept", regex = T) +
  theme(axis.text.y = element_text(hjust = 0))

# posterior predictive checks
pp_check(bp_sn20, ndraws=100)


## ETWFE models, 
bp_etwfe <-
  brm(data = d_p, 
      family = skew_normal(),
      PM25conc_exposureugm3 ~ 1 + (1 | v_id) +
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,,
      prior = c(prior(skew_normal(100, 150, 20), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(exponential(1), class = sd)),        
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 3740,
      file = "code/fits/personal-pm-etwfe")

## check the chains
mcmc_trace(bp_etwfe, pars=c("b_Intercept", 
  "b_cohort_year_2019:year_2019:treat")) +
  theme_classic() + ylab("")

# Marginal predictions for Bayesian ETWFE (simple)

## load brms model
b2 <- readRDS(here("code/fits", 
  "personal-pm-etwfe.rds"))

bme_pred <- predictions(
  b2, 
  newdata   = subset(d_p, treat==1),
  variables = "treat", 
  by        = "treat"
  )

# plot of predicted probabilities by treatment
bme_pred_p <- bme_pred |>
  posterior_draws() |>
  ggplot(aes(x = draw, fill=factor(treat))) +
    stat_halfeye(slab_alpha = .5) + 
    annotate("text", x = 126, y = 0.7, 
           label="Control", color='#1b9e77') +
    annotate("text", x = 121, y = 0.95, 
           label="Treated", color='#d95f02') +
  scale_x_continuous(
    "Personal exposure (µg/m^3^)", 
    limits=c(105,135)) +
  scale_y_continuous("Posterior Density") +
  scale_fill_manual(values = c('#1b9e77','#d95f02')) +
  theme_classic() + 
  theme(legend.position = "none", axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=12))

bme_avg <- slopes(
  b2, 
  newdata   = subset(d_p, treat==1),
  variables = "treat", 
  by        = "treat"
  ) 

# plot of treatment effect
bme_avg_p <- bme_avg |>
  posterior_draws() |>
  ggplot(aes(x = draw)) +
    stat_halfeye(slab_alpha = .5, fill = "#7570b3") +
    annotate("text", x = -1.25, y = 0.95, 
           label="Difference", color = '#7570b3') +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray60") +
    scale_x_continuous("Marginal Effect (µg/m^3^)", 
      limits=c(-15,15)) +
    scale_y_continuous("") +
    theme_classic() + 
    theme(legend.position = "none", 
        axis.text.y = element_blank(),
        axis.line.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=12))


f2 <- bme_pred_p + bme_avg_p + 
  plot_layout(widths = c(1, 1)) +
  plot_annotation(title = "Posterior distributions of marginal predictions: Personal exposure to PM~2.5~")
f2

# wrangle aggregate ATTs for model summary table
att_did <- att_table(preds = bme_pred, 
  meffs = bme_avg)

## adjusted ETWFE model
bp_etwfe_a <-
  brm(data = d_p, 
      family = skew_normal(),
      PM25conc_exposureugm3 ~ 1 + (1 | v_id) +
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021 +
        hh_num + smoker + lwsmoker +
        outdoor_temp_24h + outdoor_dew_24h,
      prior = c(prior(skew_normal(100, 150, 20), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(exponential(1), class = sd)),        
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 1389,
      file = "code/fits/personal-pm-etwfe-a")

## load brms model
b2a <- readRDS(here("code/fits", 
  "personal-pm-etwfe-a.rds"))

bme_pred_a <- predictions(
  b2a, 
  newdata   = subset(d_p, treat==1),
  variables = "treat", 
  by        = "treat"
  )

bme_avg_a <- slopes(
  b2a, 
  newdata   = subset(d_p, treat==1),
  variables = "treat", 
  by        = "treat"
  ) 

# wrangle aggregate ATTs for model summary table
att_did_a <- att_table(preds = bme_pred_a, 
  meffs = bme_avg_a)

modelsummary(list("ETWFE" = att_did,
  "Adj. ETWFE" = att_did_a), fmt=1,
  statistic = "conf.int",
  )






estimate_etwfe <- function(
    lhs, rhs1, rhs2, data) {
  
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
  etwfe2 <- fixest::feols(fml = fml2, 
    data = data, cluster = ~v_id)
  
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
  mutate(category = "Personal", outcome = "PM2.5")

ap_table_bc <- bind_rows(m_pbc$me_1, m_pbc$me_2) %>% 
  mutate(category = "Personal", 
         outcome = "Black carbon")

ap_table_i24 <- bind_rows(m_i24$me_1, m_i24$me_2) %>% 
  mutate(category = "Indoor", 
         outcome = "Daily")

ap_table_is <- bind_rows(m_is$me_1, m_is$me_2) %>% 
  mutate(category = "Indoor", 
         outcome = "Seasonal")

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

# html table for design
# kable(aph_table, digits = 2,
#   col.names = c("Cohort", "Year", "ATT", "(95% CI)", 
#    "ATT", "(95% CI)"), #"latex", booktabs = T,
#   linesep = "") %>%
#   kable_styling(full_width = F) %>%
#   # collapse_rows(columns = 1:2, valign = "top") %>% 
#   add_header_above(c(" " = 2, 
#                     "PM2.5" = 2, "Black carbon" = 2)) %>%
#   footnote(symbol=c(paste("Joint test that all ATTs are equal: ", 
#     "F(", m_ppm$ht1$df1, ", ", m_ppm$ht1$df2, ")= " , 
#     round(m_ppm$ht1$statistic, digits=3), ", p= ", 
#     round(m_ppm$ht1$p.value, digits=3)), 
#     paste("Joint test that all ATTs are equal: ", 
#           "F(", m_pbc$ht1$df1, ", ", m_pbc$ht1$df2, ")= " ,
#           round(m_pbc$ht1$statistic, digits=3), ", p= ", 
#           round(m_pbc$ht1$p.value, digits=3))), 
#     footnote_as_chunk = T) 


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



