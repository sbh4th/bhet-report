#  program:  etwfe-fuel.R
#  task:     estimate ETWFE models for fuel type
#  input:    
#  output:   
#  project:  BHET
#  author:   sam harper \ 2024-04-17


## 0 Load needed packages ----
library(here)
library(tidyverse)
library(osfr)
library(fixest)
library(marginaleffects)
library(modelsummary)
library(kableExtra)


## 1 read in fuel type dataset ----

# OSF location of data
supp <- osf_retrieve_node("h25tz")
supp %>%
  osf_ls_files(path = "Heating-device-use/code-data",
               pattern = "csv") %>%
  osf_download(path = here("data-clean"),
               conflicts = "overwrite")

dfuel <- read_csv(here("data-clean", 
  "Expenditure_House.csv")) %>%
  # limit to variables of interest
  dplyr::select(hh_id, wave, year, ID_VILLAGE, wave, 
    hh_num, ban_status_composite, Quantity_Briquettes, 
    Quantity_coal, Quantity_Honeycomb_coal, 
    Quantity_Wood, Quantity_Straw, Quantity_LPG) %>%
  # calculate household level total consumption
  rowwise()%>%
  mutate(coal = sum(Quantity_Briquettes, 
    Quantity_coal, na.rm = T) * 1000,
    biomass = sum(Quantity_Wood, 
      Quantity_Straw, na.rm = T)) %>%
  
  # variables needed for ETWFE models
    # create year and cohort_year variables
  mutate(year = if_else(wave==1, 2018, 
      if_else(wave==2, 2019,
        if_else(wave==4, 2021, 0))),
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


## 2 estimates for
## ETWFE models, estimate the marginal effects 
## (including heterogeneity) and put results in a list ----

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
rhs_dida <- c(rhs_did, "hh_num")


## 3 Estimate models for both outcomes ----
# personal PM
m_coal <- estimate_etwfe(lhs="coal", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=dfuel)

# ETWFE and adjusted ETWFE models for black carbon
m_biomass <- estimate_etwfe(lhs="biomass", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=dfuel)


# write model results to output folder
write_rds(m_coal, file = here("outputs/models",
  "m_coal.rds"))

write_rds(m_biomass, file = here("outputs/models",
  "m_biomass.rds"))


## 3 Generate table of results for both outcomes ----

# tables of results by outcome
fuel_table_coal <- bind_rows(m_coal$me_1, m_coal$me_2) %>% 
  mutate(outcome = "Coal")

fuel_table_biomass <- bind_rows(m_biomass$me_1, 
  m_biomass$me_2) %>% 
  mutate(outcome = "Biomass")

# put all subtables together
fuel_table <- bind_rows(fuel_table_coal, 
  fuel_table_biomass) %>%
  mutate(model = rep(c(1,2),times=2)) %>%
  select(model, estimate, conf.low, 
    conf.high, outcome) %>%
  relocate(outcome) %>%
  mutate(ci = paste("(", round(conf.low, 0), ", ",
    round(conf.high, 0), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = model, values_from = 
    c(estimate, ci), names_vary = "slowest")

# write table to output folder
write_rds(fuel_table, file = here("outputs", 
  "fuel-table.rds"))

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
fuel_table_coal_h <- bind_rows(m_coal$me_2, m_coal$meh_2) %>% 
  mutate(outcome = "Coal")

fuel_table_biomass_h <- bind_rows(m_biomass$me_2, 
                                  m_biomass$meh_2) %>% 
  mutate(outcome = "Biomass")

fuel_table_h <- bind_rows(fuel_table_coal_h, 
                          fuel_table_biomass_h) %>%
  select(outcome, estimate, conf.low, conf.high, 
         cohort_year, year) %>%
  mutate_at(vars(c(cohort_year,year)), ~ recode(., 
         `2019` = "2019",
         `2020` = "2020",
         `2021` = "2021",
         .missing = "All")) %>%
  relocate(outcome, cohort_year, year) %>%
  mutate(ci = paste("(", round(conf.low, 0), ", ",
    round(conf.high, 0), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = outcome, values_from = 
    c(estimate, ci), names_vary = "slowest")

# write table to data
write_rds(fuel_table_h, file = here("outputs", 
  "fuel-table-het.rds"))

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

