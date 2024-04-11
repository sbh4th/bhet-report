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
  filter(PM25_exp_remove == 1) %>%
  # drop households with duplicate values
  distinct(hh_id, wave, PM25conc_exposureugm3, 
           .keep_all= TRUE)

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

# basic DiD specification
rhs_did <- c("treat:cohort_year_2019:year_2019",
  "treat:cohort_year_2019:year_2021", 
  "treat:cohort_year_2020:year_2021",
  "treat:cohort_year_2021:year_2021",
  "cohort_year_2020", "cohort_year_2021",
  "year_2021", "year_2019", "cohort_year_2019")

# DiD plus covariates
rhs_dida <- c(rhs_did, "hh_num", "ptc_smoking", 
  "outdoor_temp_24h", "outdoor_dew_24h")

# Function to estimate ETWFE models and marginal linear regression model # effects across different outcomes
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

# ETWFE and adjusted ETWFE models for personal PM
m_ppm <- estimate_etwfe(lhs="PM25conc_exposureugm3", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=d_personal)

# ETWFE and adjusted ETWFE models for black carbon
m_pbc <- estimate_etwfe(lhs="bc_exp_conc", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=d_bc)

# ETWFE and adjusted ETWFE models for indoor daily
m_i24 <- estimate_etwfe(lhs="pm2.5_indoor_sensor_24h", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=subset(d_ind_24h, cohort_year_2019!=1))

# ETWFE and adjusted ETWFE models for indoor seasonal
m_is <- estimate_etwfe(lhs="pm2.5_indoor_seasonal_hs", 
  rhs1=rhs_did, rhs2=rhs_dida, 
  data=subset(d_ind_seasonal, cohort_year_2019!=1))

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
    #me = paste(round(estimate,2),
     # " (", round(conf.low, 2), ", ",
    #round(conf.high, 2), ")", sep="")) %>%
  select(-conf.low, -conf.high) %>%
  pivot_wider(names_from = model, values_from = 
    c(estimate, ci), names_vary = "slowest")

# now grab outdoor estimates
ap_table2 <- read_csv(here("data-clean", 
  "DID_air_pollution.csv")) %>%
  filter(`Category` == "Outdoor") %>%
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

# write table to data
write_rds(ap_table, file = here("data-clean", 
  "ap-did-table.rds"))

kable(ap_table, digits = 2,
  col.names = c(" ", " ", "Marginal effect", "(95% CI)", 
   "Marginal effect", "(95% CI)"), #"latex", booktabs = T,
  linesep = "") %>%
  kable_styling(full_width = F) %>%
  collapse_rows(columns = 1:2, valign = "top") %>% 
  pack_rows("Air pollution", 1, 4) %>%
  add_header_above(c(" " = 2, 
                    "DiD" = 2, "Adjusted DiD" = 2))

modelsummary(list("DiD" = m_ppm$me_1, 
  "Adjusted DiD" = m_ppm$me_2),
  gof_omit = ".*",
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  statistic = NULL)


modelsummary(
  ap_panels,
  shape = "rbind",
  gof_omit = ".*",
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  statistic = NULL,
  fmt = 2)
  )

ap_panels <- list(
  "Personal: PM2.5" = list(
    "DiD" = m_ppm$me_1, 
    "Adjusted DiD" = m_ppm$me_2),
  "Personal: Black carbon" = list(
    "DiD" = m_pbc$me_1, 
    "Adjusted DiD" = m_pbc$me_2),
  "Indoor: daily" = list(
    "DiD" = m_i24$me_1,
    "Adjusted DiD" = m_i24$me_2),
  "Indoor: seasonal" = list(
    "DiD" = m_is$me_1,
    "Adjusted DiD" = m_is$me_2)
  )
  

modelsummary(
  ap_panels,
  shape = "rbind",
  gof_omit = ".*",
  estimate = "{estimate} [{conf.low}, {conf.high}]",
  statistic = NULL,
  fmt = 2)

# traditional TWFE models
p_twfe <- fixest::feols(
  PM25conc_exposureugm3 ~ treat | v_id + year,
  cluster = ~v_id, data = d_personal)
p_twfe_me <- slopes(
  
)

# Goodman-Bacon decomposition
library(bacondecomp)

df_bacon <- bacon(mipm ~ treat,
                  data = vla,
                  id_var = "v_id",
                  time_var = "year")

coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

fit_tw <- lm(vla ~ post + factor(state) + factor(year), 
             data = bacondecomp::castle)
print(paste("Two-way FE estimate =", round(fit_tw$coefficients[2], 4)))

example_attgt <- att_gt(yname = "mipm",
                        tname = "year",
                        idname = "v_id",
                        gname = "cy",
                        # xformla = ~X,
                        data = vla_cs
                        )
summary(example_attgt)
aggte(example_attgt, type = "group")

mstest1 <- mstest %>% mutate(term = c("ATT(2019, 2019)", "ATT(2019, 2021)", "ATT(2020, 2021)", "ATT(2021, 2021)"))
modelsummary(list("Adjusted ETWFE" = mstest1), 
  shape = term ~ model, 
  estimate = "{estimate} [{conf.low}, {conf.high}]", 
  statistic = NULL, gof_map = "nobs", 
  notes = paste("Joint test that all ATTs are equal: ", "F(", m_ppm$ht1$df1, ", ", m_ppm$ht1$df2, ")= " ,round(m_ppm$ht1$statistic, digits=3), ", p= ", round(m_ppm$ht1$p.value, digits=3)))


