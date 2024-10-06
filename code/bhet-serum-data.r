#  program:  bhet-serum-analysis.R
#  task:     estimate models for serum outcomes
#  input:    bhet-master
#  output:   
#  date:     2024-10-4
#  project:  BHET
#  author:   Kaibing Xue;Wenlu Yuan;Sam Harper 

##  0 Load needed packages ----
library(here)
library(tidyverse)
library(haven)
library(modeldb)
library(modelr)
library(osfr)
library(modelsummary)
library(readxl);library(readr);

setwd("F:/analysis/serum/DID")
i_am("bhet-serum-data.R")

## 1 Read in dataset, limit to serum vars ----
d <- read_dta(here("DID","BHET_master_data_22Jul2024.dta"), 
  col_select= c(hh_id, ptc_id, wave, ID_VILLAGE, ID_TOWN, ID_COUNTY,
                ban_status_composite,                    # treatment status
                #year,cohort_year,treat,#ETS,
                gender_health,occupation,smoking,lived_with_smoker, 
                freq_drink, freq_farming,                # health behaviors
                waist_circ, age_health, 
                vial_id,
                #quantile_WI,
                #IL6, TNF, CRP, MDA,
                #IL6_log, TNF_log, CRP_log, MDA_log
                ))

# import wealth index
dwi <- read_csv(here("DID", 
  "BHET_PCA_11Oct2023.csv"))
dwi <- dwi %>%
  group_by(wave) %>%
  mutate(wq = as.integer(cut(wealth_index, 
    quantile(wealth_index, 
      probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T), 
    labels = c(1:4), include.lowest = T))) %>%
  ungroup() %>%
  add_dummy_variables(wq, 
    values=c(1:4), remove_original = F) 
# add wealth index to main dataset
d1 <- d %>% 
  left_join(dwi, by = c("hh_id", "ptc_id", "wave"))

# drop participants didn't accept interview
d1 %>% filter(wave == 3) -> noData
d1 %>% anti_join(noData) -> d1               # no interview in winter 2020-21 (wave 3)


#2 Load biomarker dataset----

#biomarkers <- read_excel("~/Desktop/McGill/PhD thesis/BHET/data/Aim 2 data Policy and biomarkers/Descriptive statistics/data structure.xlsx")
biomarkers <- read_excel("F:/analysis/serum/biomarkers.xlsx")
nrow(biomarkers)                                                                   # 1617 obs

# Because in master dataset, year 2 biomarker vial IDs all contain a '201' prefix,
# drop this before merge master dataset with biomarker dataset
biomarkers <- biomarkers %>% 
  mutate(vial_id_new = ifelse(str_detect(vial_id, "2019"), 
                              str_replace(vial_id, "2019", "9"), 
                              vial_id),
         vial_id_new = as.numeric(vial_id_new),
         
         # make corrections to specific vial IDs
         vial_id_new = ifelse(vial_id_new == 91000, 1000, vial_id_new),
  ) %>% select(-vial_id)

# merger master dataset with biomarker data
biomarkers_merged <- d1 %>%
  full_join(biomarkers, by = c("vial_id" = "vial_id_new")) %>%
  arrange(hh_id, ptc_id, wave)
# Filter out unmatched observations.
biomarkers_merged <- biomarkers_merged %>%
  filter(ptc_id != "")  

## Assign values to measurements lower than Limit of Detection (LOD) ---
#biomarkers_merged <- read.csv("F:/analysis/serum/biomarkers_merged.csv")
#ptc_LOD <- read_csv("ptc_LOD.csv")
ptc_LOD <- read.csv("F:/analysis/serum/ptc_LOD.csv")
#ptc_LOD.IL6 <- read.csv("F:/analysis/serum/ptc_LOD.IL6.csv")
#ptc_LOD.TNF <- read.csv("F:/analysis/serum/ptc_LOD.TNF.csv")
#ptc_LOD.CRP <- read.csv("F:/analysis/serum/ptc_LOD.CRP.csv")

## ~ LOD-----
head(ptc_LOD)
IL6.LOD = 1.04
TNF.LOD1 = 5.2 # LOD for low sensitivity kits; plate 1-42
TNF.LOD2 = 0.052 # LOD for high sensitivity kits; plate 43-44 or S1-S6
CRP.LOD = 0.0521

ptc_LOD %>% 
  filter(biomarker == "IL6") ->  ptc_LOD.IL6
ptc_LOD.IL6$IL6.new <- rnorm(nrow(ptc_LOD.IL6), mean = IL6.LOD/2, sd = 0.1)
summary(ptc_LOD.IL6$IL6.new)
biomarkers_merged %>% 
  left_join(ptc_LOD.IL6, by = c("ptc_id", "wave")) %>% 
  select(-biomarker) -> biomarkers_merged

ptc_LOD %>% 
  filter(biomarker == "TNF" & 
           !ptc_id %in% c("PT2018033500", 
                          "PT2018033200", 
                         "PT2018090500")) ->  ptc_LOD.TNF.low                     # 87 obs from low sensitivity kits
ptc_LOD %>% 
  filter(biomarker == "TNF" & 
           ptc_id %in% c("PT2018033500", 
                         "PT2018033200",
                         "PT2018090500")) ->  ptc_LOD.TNF.high                     # 3 obs from high sensitivity kits
ptc_LOD.TNF.low$TNF.new <- rnorm(nrow(ptc_LOD.TNF.low), mean = TNF.LOD1/2, sd = 0.5)
summary(ptc_LOD.TNF.low$TNF.new)
ptc_LOD.TNF.high$TNF.new <- rnorm(nrow(ptc_LOD.TNF.high), mean = TNF.LOD2/2, sd = 0.01)
summary(ptc_LOD.TNF.high$TNF.new)
ptc_LOD.TNF <- rbind(ptc_LOD.TNF.low, ptc_LOD.TNF.high)
biomarkers_merged %>% 
  left_join(ptc_LOD.TNF, by = c("ptc_id", "wave")) %>% 
  select(-biomarker) -> biomarkers_merged

ptc_LOD %>% 
  filter(biomarker == "CRP") ->  ptc_LOD.CRP
ptc_LOD.CRP$CRP.new <- rnorm(nrow(ptc_LOD.CRP), mean = CRP.LOD/2, sd = 0.01)
summary(ptc_LOD.CRP$CRP.new)
biomarkers_merged %>% 
  left_join(ptc_LOD.CRP, by = c("ptc_id", "wave")) %>% 
  select(-biomarker) -> biomarkers_merged


# 3 Merged dataset --------------------------------------------

biomarkers_merged %>% 
  mutate(IL6 = ifelse(is.na(IL6.new), IL6, IL6.new)) %>% 
  mutate(TNF = ifelse(is.na(TNF.new), TNF, TNF.new)) %>% 
  mutate(CRP = ifelse(is.na(CRP.new), CRP, CRP.new)) -> biomarkers_merged

# drop those don't have biomarker data
biomarkers_merged %>% 
  filter(!is.na(IL6) & !is.na(TNF) & !is.na(CRP) & !is.na(MDA)) -> biomarkers_merged_master

## outcomes and covariates---------
WI_quantile <- quantile(biomarkers_merged_master$wealth_index, na.rm = T)
WI_quantile

d2 <- biomarkers_merged_master %>%
  mutate(
    # covariates
    male = if_else(gender_health == 1, 1, 0),
    # csmoke = if_else(smoking == 1, 1, 0),
    # fsmoke = if_else(smoking == 2, 1, 0),
    #bmi = weight / (height/100)^2,
    
    occ = case_when(
      occupation == 1 ~ "Agriculture",
      occupation %in% c(2,11,12) ~ "Manual",
      occupation %in% c(3:5) ~ "Non-manual",
      occupation %in% c(6:10) ~ "Other"),
    # add dummies for occupation
    occ_2 = if_else(
      occ=="Manual", 1, 0),
    occ_3 = if_else(
      occ=="Non-manual", 1, 0),
    occ_4 = if_else(
      occ=="Other", 1, 0),
    
    ets = case_when(
      smoking == 1 ~ "Current smoker",
      smoking == 2 ~ "Former smoker",
      smoking == 3 & lived_with_smoker %in% c(2,3) ~ 
        "Never smoker lived with smoker",
      smoking == 3 & lived_with_smoker == 1 ~ 
        "No smoking exposure"),
    # add dummies for smoking
    ets_2 = if_else(
      ets=="Former smoker", 1, 0),
    ets_3 = if_else(
      ets=="Never smoker lived with smoker", 1, 0),
    ets_4 = if_else(
      ets=="No smoking exposure", 1, 0),
    
    drink = case_when(
      freq_drink == 1 ~ "Never",
      freq_drink %in% c(2:5) ~ "Occasional",
      freq_drink %in% c(6:8) ~ "Regular",
      freq_drink == 9 ~ "Everyday"),
    # add dummies for drinking
    drink_2 = if_else(
      drink=="Occasional", 1, 0),
    drink_3 = if_else(
      drink=="Regular", 1, 0),
    drink_4 = if_else(
      drink=="Everyday", 1, 0),
    
    farm = case_when(
      freq_farming == 1 ~ "Never",
      freq_farming %in% c(2:3) ~ "Occasional",
      freq_farming == 4 ~ "Regular",
      freq_farming == 5 ~ "Everyday"),
    # add dummies for farming
    farm_2 = if_else(
      farm=="Occasional", 1, 0),
    farm_3 = if_else(
      farm=="Regular", 1, 0),
    farm_4 = if_else(
      farm=="Everyday", 1, 0)) %>%
  
  mutate(quantile_WI = case_when(wealth_index <= WI_quantile[2] ~ "<= 25%",
                                 wealth_index > WI_quantile[2] & wealth_index <= WI_quantile[3] ~ "25%-50%",
                                 wealth_index > WI_quantile[3] & wealth_index <= WI_quantile[4] ~ "50%-75%",
                                 wealth_index > WI_quantile[4]~ ">75%"),
         quantile_WI = factor(quantile_WI, c("<= 25%", "25%-50%", "50%-75%", ">75%"))) %>% 
  
  # treatment related
  mutate(year = if_else(wave==1, 2018,2019),
         cohort_year = if_else(ban_status_composite==1, 2019, 2020),
         #cohort_year_2019 = if_else(cohort_year = 2019, 1, 0),
         year_2019 =ifelse(wave == 2, 1, 0),
         treat = ifelse(year >= cohort_year, 1, 0),
         cohort_year = ifelse(cohort_year == 2020,-Inf, 
                              cohort_year)) %>%
  
  # add dummies for district
  #add_dummy_variables(district,
                      #values = c(1,2,3,4),
                      #remove_original = T) %>%
  # relabel last cohort year 
  # treatment cohort dummies
  add_dummy_variables(cohort_year, 
                      values=c(-Inf,2019), 
                      remove_original = F) %>%
  # wave dummies
  add_dummy_variables(year, 
                      values=c(2018,2019), remove_original = F) %>%
  # create unique continuous village Id
  group_by(ID_VILLAGE) %>%
  mutate(v_id = cur_group_id()) %>%
  ungroup()

## save the data---------

write_rds(d2, here("DID", "bhet-Infla-data.rds"))

