#  program:  etwfe-te-ap-analysis.R
#  task:     estimate ETWFE modesls for air pollution
#  input:    various .rds files
#  output:   various tables
#  project:  BHET
#  author:   sam harper \ 2024-04-15


## 0 Load needed packages ----
library(here)
library(tidyverse)
library(osfr)
library(fixest)
library(marginaleffects)
library(modelsummary)
library(kableExtra)
library(patchwork)


## 1 read in clean air pollution datasets ----

# personal PM2.5
d_p <- read_rds(here("data-clean",
  "ap-data-personal.rds"))

# personal black carbon
d_bc <- read_rds(here("data-clean",
  "ap-data-bc.rds"))

# indoor daily
d_i24 <- read_rds(here("data-clean",
  "ap-data-i24h.rds"))

# indoor seasonal
d_is <- read_rds(here("data-clean",
  "ap-data-iseason.rds"))


## 2 define a function to estimate the and adjusted 
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


## 7 Pre-trends for AP data
# set theme for pre-trends
theme_pt <- function() {
  theme_classic() + 
    theme(axis.title = element_text(size=18),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      plot.subtitle = element_text(size = 12))
}

# Personal exposure
# limit to pre-intervention years/cohort
d_p_r <- d_p %>% filter(year < 2021) %>%
  mutate(et = case_when(cohort_year_2021==1 ~ "Yes",
                        cohort_year_2021==0 ~ "No"))

pt_p <- glm(PM25conc_exposureugm3 ~ year * et,
            data = d_p_r)

# Time trends by treatment status
avg_comparisons(pt_p, 
  var = "year",
  by = "cohort_year_2021",
  vcov = ~ v_id)

# Difference in time trends by treatment
pt_pt <- avg_comparisons(pt_p, 
  var = "year",
  by = "cohort_year_2021",
  hypothesis = "b2 - b1 = 0",
  vcov = ~ v_id)

# Gather estimates for difference in pre-trends
pt_text <- "Difference in trend (SE) for treated vs. untreated villages:" 
pt_p_stats <- paste(sprintf("%.1f", pt_pt$estimate),
  " (", sprintf("%.1f", pt_pt$std.error), ")", 
  ", 95% CI: ", sprintf("%.1f", pt_pt$conf.low),
  ", ", sprintf("%.1f", pt_pt$conf.high), sep="")

pt_p_test <- paste(pt_text, pt_p_stats, sep = "\n")

# Plot of trends and estimates
pt_pe_plot <- plot_predictions(pt_p, condition = c("year",
  "et")) + scale_y_continuous(limits = c(0, 150)) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  labs(subtitle = pt_p_test,
  y = expression("Personal PM"["2.5"] ~ "(µg/" ~ m^3~")"), 
    x = "") +
  scale_color_manual(name = "Treated in 2021?",
    labels = c("No", "Yes"),
    values = c("#e41a1c", "#377eb8")) +
  scale_fill_manual(name = "Treated in 2021?",
    labels = c("No", "Yes"),
    values = c("#e41a1c", "#377eb8")) + theme_pt()

# Personal black carbon
d_bc_r <- d_bc %>% filter(year < 2021) %>%
  mutate(et = case_when(cohort_year_2021==1 ~ "Yes",
                        cohort_year_2021==0 ~ "No"))
pt_bc <- glm(bc_exp_conc ~ year * et,
            data = d_bc_r)

# Time trends by treatment status
avg_comparisons(pt_bc, 
  var = "year",
  by = "et",
  vcov = ~ v_id)

# Difference in time trends by treatment
pt_bct <- avg_comparisons(pt_bc, 
  var = "year",
  by = "cohort_year_2021",
  hypothesis = "b2 - b1 = 0",
  vcov = ~ v_id)

# Gather estimates for difference in pre-trends
pt_bc_stats <- paste(sprintf("%.1f", pt_bct$estimate),
  " (", sprintf("%.1f", pt_bct$std.error), ")", 
  ", 95% CI: ", sprintf("%.1f", pt_bct$conf.low),
  ", ", sprintf("%.1f", pt_bct$conf.high), sep="")

pt_bc_test <- paste(pt_text, pt_bc_stats, sep = "\n")


# Plot of trends and estimates
pt_bc_plot <- plot_predictions(pt_bc, 
  condition = c("year", "et")) + 
  scale_y_continuous(limits = c(-0.5, 5)) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  labs(subtitle = pt_bc_test,
       y = expression("Black carbon" ~ "(µg/" ~ m^3~")"), x = "") +
  scale_color_manual(name = "Treated in 2021?",
                     labels = c("No", "Yes"),
                     values = c("#e41a1c", "#377eb8")) +
  scale_fill_manual(name = "Treated in 2021?",
                    labels = c("No", "Yes"),
                    values = c("#e41a1c", "#377eb8")) + 
  theme_pt()


# add personal plots together
pt_plots <- pt_pe_plot / pt_bc_plot

ggsave(here("images", "pe-bc-pretrends.png"), 
       plot=pt_plots, width=6.5, height=9)


