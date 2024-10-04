#  program:  resp-outcomes.R
#  task:     estimate models for respiratory outcomes
#  input:    bhet-master
#  output:   
#  project:  BHET
#  author:   sam harper \ 2024-10-04

## 0  packages to load
pkgs <- c('here', 'tidyverse', 'modelsummary', 
          'fixest', 'marginaleffects',
          'patchwork', 'estimatr')

#load all packages at once
lapply(pkgs, library, character.only=TRUE)



## 1 Read in dataset, limit to resp vars ----
dresp <- read_rds(here("data-clean", 
  "bhet-resp-data.rds")) 



## 2 Function to run models, 
# estimate marginal effects ----

# Function to estimate GLM for multiple outcomes
estimate_logit_did <- function(outcome_vars, predictor_vars) {
  models <- outcome_vars %>%
    set_names() %>%
    map(~ {
      formula <- as.formula(paste(.x, "~", 
        paste(predictor_vars, collapse = " + ")))
      glm(formula, data = dresp_cc, family = "binomial")
    })
  
  return(models)
}


## 3 Run models ----

# binary outcomes
b_out <- c("resp", "cough", "phlegm", "wheeze",
              "breath", "nochest")

# basic DiD specification
rhs_did <- c("treat:cohort_year_2019:year_2019",
  "treat:cohort_year_2019:year_2021", 
  "treat:cohort_year_2020:year_2021",
  "treat:cohort_year_2021:year_2021", "cohort_year_2019",
  "cohort_year_2020", "cohort_year_2021",
  "year_2019", "year_2021")

rhs_dida <- c(rhs_did, "age_health", "male", 
  "ets_2", "ets_3", "ets_4",
  "occ_2", "occ_3", "occ_4",
  "drink_2", "drink_3", "drink_4",
  "farm_2", "farm_3", "farm_4")

dresp_cc <- dresp %>%
  # limit to complete cases (ignoring bmi)
  drop_na(cresp:farm_4, -bmi)



## 4 gather estimates across models ----

# basic DiD
logit_did <- estimate_logit_did(b_out, rhs_did)
write_rds(logit_did, file = here(
  "outputs/logit-did.rds"))

# with covariates
logit_dida <- estimate_logit_did(b_out, rhs_dida)

# estimate marginal effects (simple average)
# from the basic DiD
logit_me <- lapply(logit_did, marginaleffects::slopes, 
  newdata = subset(dresp, treat==1), 
  variables = "treat", by = "treat",
  # make sure to use cluster-robust SEs
  vcov = ~v_id)
write_rds(logit_me, file = here(
  "outputs/logit-me.rds"))

# heterogenous treatment effects
logit_me_het <- lapply(logit_did, 
  marginaleffects::slopes, 
  newdata = subset(dresp, treat==1), 
  variables = "treat", 
  by = c("cohort_year", "year"),
  # make sure to use cluster-robust SEs
  vcov = ~v_id)

# from the adjusted DiD
logit_mea <- lapply(logit_dida, marginaleffects::slopes, 
  newdata = subset(dresp, treat==1), 
  variables = "treat", by = "treat",
  # make sure to use cluster-robust SEs
  vcov = ~v_id)

# heterogenous treatment effects
logit_mea_het <- lapply(logit_dida, 
  marginaleffects::slopes, 
  newdata = subset(dresp, treat==1), 
  variables = "treat", 
  by = c("cohort_year", "year"),
  # make sure to use cluster-robust SEs
  vcov = ~v_id)

# contrasts of ATTs
# heterogenous treatment effects
logit_mea_het_c <- lapply(logit_dida, 
  marginaleffects::slopes, 
  newdata = subset(dresp, treat==1), 
  variables = "treat", 
  by = c("cohort_year", "year"),
  hypothesis = c("b1 - b2 =0", 
    "b1 - b3 = 0", "b1 - b4 = 0"),
  # make sure to use cluster-robust SEs
  vcov = ~v_id)

# tests of heterogeneity
# Define a function to apply hypotheses function
apply_hypotheses <- function(variable_name) {
  hypotheses(logit_mea_het_c[[variable_name]], 
             joint = TRUE)
}

het_tests <- setNames(lapply(b_out, 
  apply_hypotheses), b_out)

# write heterogeneity test results to file
write_rds(het_tests, file = here("outputs/models",
  "r_het_tests.rds"))


## 5 Create tables ----

# grab estimates and SEs from DiD results
did_t1 <- logit_me %>% {
  tibble(
    # table = 1,
    outcome = names(logit_me),
    est = map_dbl(., "estimate") * 100,
    stderror = map_dbl(., "std.error") * 100,
    ll = est - 1.96 * stderror,
    ul = est + 1.96 * stderror,
    ci = paste("(", sprintf("%.1f", ll), ", ",
      sprintf("%.1f", ul), ")", sep="")
  )
}

# grab number of observations
did_obs <- logit_did %>% {
  tibble(
    outcome = names(logit_did),
    nobs = map_int(., "df.null") + 1
  )
}

# add observations to table
did_t1 <- did_t1 %>%
  inner_join(did_obs) %>%
  relocate(outcome, nobs)

# grab estimates and SEs from adjusted DiD results
did_t2 <- logit_mea %>% {
  tibble(
    # table = 2,
    # outcome = names(logit_me),
    esta = map_dbl(., "estimate") * 100,
    stderrora = map_dbl(., "std.error") * 100,
    lla = esta - 1.96 * stderrora,
    ula = esta + 1.96 * stderrora,
    cia = paste("(", sprintf("%.1f", lla), ", ",
      sprintf("%.1f", ula), ")", sep="")
  )
}

# put basic and adjusted DiD results together
didt <- cbind(did_t1, did_t2) %>%
  dplyr::select(nobs, est, ci, esta, cia) %>%
  mutate(outcome = c("Any symptom", "Coughing",
    "Phlegm", "Wheezing attacks", "Trouble breathing",
    "Chest trouble"),
    category = "Self-reported (pp)") %>%
  # rename for combining with other tables
  rename(`estimate_1` = "est", `ci_1` = "ci",
         `estimate_2` = "esta", `ci_2` = "cia") %>%
  relocate(category, outcome)
  

# write results table to dataset
write_rds(didt, file = here("data-clean", 
  "resp-did.rds"))

# Example table of marginal effects
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
  shape = model ~ term + statistic,
  statistic = 'conf.int')


## 6 Create heterogeneity tables by outcome ----
##   based on adjusted DiD results

# Define a function to process 
# each outcome and write to file
each_outcome <- function(outcome) {
  result <- bind_rows(logit_mea[[outcome]],
              logit_mea_het[[outcome]]) %>%
    mutate(outcome = outcome,
           estimate = estimate * 100,
           conf.low = conf.low * 100,
           conf.high = conf.high * 100) %>%
    select(outcome, estimate, conf.low, conf.high, 
           cohort_year, year) %>%
    mutate_at(vars(c(cohort_year, year)), ~ recode(., 
           `2019` = "2019",
           `2020` = "2020",
           `2021` = "2021",
           .missing = "All")) %>%
    relocate(outcome, cohort_year, year) %>%
    mutate(ci = paste("(", sprintf('%.1f', conf.low), ", ",
      sprintf('%.1f', conf.high), ")", sep="")) %>%
    select(-conf.low, -conf.high)
  
  # Write the result to an .rds file
  write_rds(result, file = here("outputs", 
    paste0("resp-het-", outcome, ".rds")))
  
  return(result)  # Return the result if you want to store it in memory as well
}

# Restate list of outcomes
b_out <- c("resp", "cough", "phlegm", 
           "wheeze", "breath", "nochest")

# Apply the function across all outcomes and store results in a list (optional)
result_list <- lapply(b_out, each_outcome)

