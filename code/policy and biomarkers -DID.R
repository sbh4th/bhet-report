------------
#' DID---Policy and Inflammatory/oxidative stress biomarkers 
#' Kaibing Xue;Wenlu yuan;Sam
#' Date created: Oct 2, 2024
#' Last update: Oct 5
------------
  
# packages to load----
install.packages("Rtools")  
 
library(tidyverse);library(readxl);library(readr);
library(fixest);library(lme4);library(estimatr);
library(broom.mixed);library(writexl)

# Data load---------- 
#setwd("F:/analysis/serum")
i_am("infla-outcomes.R")
dInfla <- read_rds(here("DID", 
                        "bhet-Infla-data.rds")) 
#biomarkers_merged_full <- read.csv("F:/analysis/serum/biomarkers_merged_full2.csv")

# Two-way fixed effect model ----------

## a). unadjusted model ---------------

uni_IL6<- feglm(
  IL6 ~ treat:cohort_year_2019:year_2019 +
             cohort_year_2019 + year_2019,
             data = dInfla, 
             family = Gamma(link = "log"), 
             cluster = ~ID_VILLAGE)

meffs_IL6_unadj <- marginaleffects::slopes(
  uni_IL6, newdata = subset(dInfla, treat==1),
  variables = "treat", by = "treat")

uni_TNF<- feglm(
  TNF ~ treat:cohort_year_2019:year_2019 +
    cohort_year_2019 + year_2019,
  data = dInfla, 
  family = Gamma(link = "log"), 
  cluster = ~ID_VILLAGE)

meffs_TNF_unadj <- marginaleffects::slopes(
  uni_TNF, newdata = subset(dInfla, treat==1),
  variables = "treat", by = "treat")

uni_CRP<- feglm(
  CRP ~ treat:cohort_year_2019:year_2019 +
    cohort_year_2019 + year_2019,
  data = dInfla, 
  family = Gamma(link = "log"), 
  cluster = ~ID_VILLAGE)

meffs_CRP_unadj <- marginaleffects::slopes(
  uni_CRP, newdata = subset(dInfla, treat==1),
  variables = "treat", by = "treat")

uni_MDA<- feglm(
  MDA ~ treat:cohort_year_2019:year_2019 +
    cohort_year_2019 + year_2019,
  data = dInfla, 
  family = Gamma(link = "log"), 
  cluster = ~ID_VILLAGE)

meffs_MDA_unadj <- marginaleffects::slopes(
  uni_MDA, newdata = subset(dInfla, treat==1),
  variables = "treat", by = "treat")

## b). Adjusted model ---------------
#age_CORRECTED_center + waist_circ_center + quantile_WI +
  #occupation_new+freq_drink+ETS+freq_farming

mul_IL6<- feglm(
  IL6 ~ treat:cohort_year_2019:year_2019 +
    cohort_year_2019 + year_2019+
    age_health + waist_circ + quantile_WI +
    occ+drink+ets+farm,
  data = dInfla,
  family = Gamma(link = "log"), 
  cluster = ~ID_VILLAGE)

meffs_IL6_adj <- marginaleffects::slopes(
  mul_IL6, newdata = subset(dInfla, treat==1),
  variables = "treat", by = "treat")

mul_TNF<- feglm(
  TNF ~ treat:cohort_year_2019:year_2019 +
    cohort_year_2019 + year_2019+
    age_health + waist_circ + quantile_WI +
    occ+drink+ets+farm,
  data = dInfla, 
  family = Gamma(link = "log"), 
  cluster = ~ID_VILLAGE)

meffs_TNF_adj <- marginaleffects::slopes(
  mul_TNF, newdata = subset(dInfla, treat==1),
  variables = "treat", by = "treat")

mul_CRP<- feglm(
  CRP ~ treat:cohort_year_2019:year_2019 +
    cohort_year_2019 + year_2019+
    age_health + waist_circ + quantile_WI +
    occ+drink+ets+farm,
  data = dInfla, 
  family = Gamma(link = "log"), 
  cluster = ~ID_VILLAGE)

meffs_CRP_adj <- marginaleffects::slopes(
  mul_CRP, newdata = subset(dInfla, treat==1),
  variables = "treat", by = "treat")

mul_MDA<- feglm(
  MDA ~ treat:cohort_year_2019:year_2019 +
    cohort_year_2019 + year_2019+
    age_health + waist_circ + quantile_WI +
    occ+drink+ets+farm,
  data = dInfla, 
  family = Gamma(link = "log"), 
  cluster = ~ID_VILLAGE)

meffs_MDA_adj <- marginaleffects::slopes(
  mul_MDA, newdata = subset(dInfla, treat==1),
  variables = "treat", by = "treat")


## Table Overall ATTs-------
# Unadjusted overall ATT
IL6_unadj <- meffs_IL6_unadj %>% mutate(adj = "Unadjusted")
TNF_unadj <- meffs_TNF_unadj %>% mutate(adj = "Unadjusted")
CRP_unadj <- meffs_CRP_unadj %>% mutate(adj = "Unadjusted")
MDA_unadj <- meffs_MDA_unadj %>% mutate(adj = "Unadjusted")

# Adjusted overall ATT
IL6_adj <- meffs_IL6_adj %>% mutate(adj = "Adjusted")
TNF_adj <- meffs_TNF_adj %>% mutate(adj = "Adjusted")
CRP_adj <- meffs_CRP_adj %>% mutate(adj = "Adjusted")
MDA_adj <- meffs_MDA_adj %>% mutate(adj = "Adjusted")

# Create a single data frame of overall effects
IL6_glm <- rbind(IL6_unadj, IL6_adj)
TNF_glm <- rbind(TNF_unadj, TNF_adj)
CRP_glm <- rbind(CRP_unadj, CRP_adj)
MDA_glm <- rbind(MDA_unadj, MDA_adj)

Infla_DID <- rbind(IL6_glm,TNF_glm,CRP_glm,MDA_glm)
Infla_DID %>% mutate(
  biomarker = c(rep('IL6',times=2),
                rep('TNF',times=2),
                rep('CRP',times=2),
                rep('MDA',times=2)),
  model = rep(c('unadj','adj'),times=4)) ->Infla_DID

write_xlsx(Infla_DID,"F:/analysis/serum/DID/Infla_DID.xlsx")


# Reference------

# estimate the GLM model with Gamma family and log link
# and cluster robust SEs 

model <- feols::feglm(
  outcome ~ treat:cohort_year_2019:year_2019 + treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + treat:cohort_year_2021:year_2021 + 
    cohort_year_2019 + cohort_year_2020 + cohort_year_2021 + 
    year_2019 + year_2021,
  data = data, family = Gamma(link = "log"), cluster = ~village)

# estimate overall ATT

meffs <- marginaleffects::slopes(
  model, newdata = subset(data, treat==1),
  variables = "treat", by = "treat")
