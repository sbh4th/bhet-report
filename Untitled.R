library(here)
library(tidyverse)
library(osfr)
library()

d <- read_csv(here("data-clean", 
  "BHET_master_data_22Jul2024.csv"),
  col_select = c("hh_id", "ptc_id", "gender_health",
    "ban_status_no", "age_health", "education_health",
    "smoking", "freq_drink", "sys_brachial", 
    "dia_brachial", "waist_circ", "height",
    "weight", "temp", "wave", "PM25conc_exposureugm3",
    "freq_cough", "freq_phlegm", "freq_wheezing", 
    "freq_breath", "freq_no_chest"))

## Add wealth index data ====
## retrieve processed data node
bhet_processed <- osf_retrieve_node("b4wze")
 
## download from OSF
wealth_dl <- osf_ls_files(bhet_processed) %>%
  filter(name == "Wealth Index") %>%
  osf_ls_files() %>%
  filter(name == "BHET_PCA_11Oct2023.csv") %>%
  osf_download(path = "data-clean",
               conflicts = "overwrite")
 
## read in file
dwi <- read_csv(here("data-clean", "BHET_PCA_11Oct2023.csv")) 

dwi <- dwi %>%
  # create quantiles of wealth by wave
  group_by(wave) %>%
  mutate(quintile = as.integer(cut(wealth_index, 
    quantile(wealth_index, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T), 
    labels = c(1:4), include.lowest = T))) %>%
  ungroup()

d <- read_csv(here("data-clean", 
  "BHET_master_data_05Mar2024.csv"),
  col_select = c("hh_id", "ptc_id", "gender_health",
    "ban_status_no", "age_health", "education_health",
    "smoking", "freq_drink", "sys_brachial", 
    "dia_brachial", "waist_circ", "height",
    "weight", "temp", "wave", "PM25conc_exposureugm3",
    "freq_cough", "freq_phlegm", "freq_wheezing", 
    "freq_breath", "freq_no_chest", "ID_VILLAGE",
    "ban_status_composite"))

dwi <- read_csv(here("data-clean", "BHET_PCA_11Oct2023.csv")) 

# create quantiles of wealth by wave
dwi <- dwi %>%
  group_by(wave) %>%
  mutate(wq = as.integer(cut(wealth_index, 
    quantile(wealth_index, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T), 
    labels = c(1:4), include.lowest = F))) %>%
  ungroup()

dwi %>%
  group_by(wq) %>% tally()

dt <- d %>%
  left_join(dwi, by = c("hh_id", "ptc_id", "wave")) %>%
  # restrict to baseline
  # filter(wave==1) %>%
  # select variables for inclusion
  # select(ban_status_no, age_health, )
  mutate(ever_trt = ifelse(ban_status_no==0, 1 ,0),
    et = recode_factor(ever_trt, `0` = "Never treated", 
      `1` = "Ever treated"),
    post = wave > 1, 1, 0,
    cohort_year = if_else(
      ban_status_composite==1, 2019, 
      if_else(ban_status_composite==2, 2020, 
              if_else(ban_status_composite==3, 2021, 2022))))
,
    `Age (years)` = age_health,
    `Female (%)` = ifelse(gender_health==2, 100, 0),
    `No education (%)` = ifelse(education_health==4, 100, 0),
    `Primary education (%)` = ifelse(education_health==1, 100, 0),
    `Secondary+ education (%)` = ifelse(
      education_health %in% c(2,3), 100, 0),
    `Wealth index (bottom 25%)` = ifelse(wq==1, 100, 0),
    `Wealth index (25-50%)` = ifelse(wq==2, 100, 0),
    `Wealth index (50-75%)` = ifelse(wq==3, 100, 0),
    `Wealth index (top 25%)` = ifelse(wq==4, 100, 0)
 
## add to master
master2 <- master1 %>%
  left_join(master_WI, by = c("hh_id", "ptc_id", "wave"))



rp_table <- bind_rows(bp_policy_t, r_table, 
  feno_table, inf_table) %>% 
  select(-nobs, -estimate_1, -ci_1) %>% 
  mutate(
    ll = as.numeric(sub(
      ".*,\\s*(-?\\d+\\.\\d+)\\)", "\\1", ci_2)),
    ul = as.numeric(sub(
      "\\((-?\\d+\\.\\d+),.*", "\\1", ci_2)), 
    se = abs(ll - ul) / (2 * 1.96)) %>%
  select(category, outcome, estimate_2, se) %>%
  mutate(es = c(3, 3, 2, 2, 0.5, 0.5, 
    0.1, 0.1, 5, 2, 3, 1, 3, 1, 2,
    2, 2, 2, 2))

# Apply the function row-wise and collect results
rp_table2 <- rp_table %>%
  rowwise() %>%
  mutate(results = list(retro_design_closed_form(
    es, se))) %>%
  unnest_wider(results) 
%>%
  mutate(op = 100 * power)

colnames(rp_table2) <- c(" ", " ", "Estimate", "SE", 
  "Effect", "Power", "S-bias", "M-bias")

tt(rp_table2,
   digits = 2,
   #width = c(3.5, 3, 1, 0.5, 2, 0.5, 2, 0.5, 2, 0.5, 2),
   notes = list("Note: ATT = Average Treatment Effect on the Treated, DiD = Difference-in-Differences, ETWFE = Extended Two-Way Fixed Effects, Obs = observations, pp = percentage points, ppb = parts per billion.", a = list(i=0, j=6, text = "ETWFE models for blood pressure models adjusted for age, sex, waist circumference, smoking, alcohol consumption, and use of blood pressure medication. Self-reported respiratory outcomes adjusted for age, gender, and smoking. Measured respiratory outcome (FeNO) adjusted age, gender, body mass index, frequency of drinking, smoking, and frequency of exercise, occupation, time of measurement. Inflammatory marker outcome models adjusted for age, waist circumference, occupation, wealth index quantile, frequency of drinking, tobacco smoking, and frequency of farming." ))) %>%
  group_tt(
    j = list("Observed" = 3:4, 
             "Design results" = 6:8),
    i = list("Blood pressure (mmHg)" = 1, 
             "Respiratory outcomes" = 9,
             "Inflammatory markers (%)" = 16)) %>%
  style_tt(i = c(1, 10, 18), align = "l", bold=T) %>%
  style_tt(
    i = c(2, 4, 6, 8), j = 1, 
    rowspan = 2, alignv = "t") %>%
  style_tt(
    i = 11, j = 1, rowspan = 6, alignv = "t") %>%
  style_tt(j = 1:7, align = "llccccc") %>%
  format_tt(escape = TRUE) %>%
  format_tt(j=c(3,5,8), sprintf = "%.1f") %>%
  format_tt(j = 6, sprintf = "%0.1f%%")

e_out <- rp_table$e

estimate_rp(c(""))
