#' ------
#' Project: BHET
#' Script purpose: Study sample stats
#' Date created: October 4, 2020
#' Last Update: February 29, 2024
#' Author: Talia Sternbach
#' ------

# Set-up: Load packages, functions, data ---------------------------------------
library(tidyverse); library(haven); library(readxl)

# load master dataset (download from OSF)
master <- read_dta("OneDrive - McGill University/BHET/DATA/current_master_dataset/BHET_master_data_28Feb2024.dta") #%>%
  # drop participants only with feno data in Year 4
  #filter(!(paste0(ptc_id, wave) %in% c("PT20180200004", "PT20180242004", "PT20180299044", 
  #                                   "PT20180362004", "PT20180409004", "PT20180475004",
  #                                   "PT20180476004", "PT20180567004", "PT20180568044", 
  #                                   "PT20180651004", "PT20180665004", "PT20180669004", 
  #                                   "PT20180673004", "PT20180698044", "PT20180932004",
  #                                   "PT20181135014", "PT20181149004")))


# Participants and households overall and by study year ------------------------

## number of observations, all seasons
nrow(master) # 3,672 observations

## total number of households
length(unique(master$hh_id)) # 1,236 households

## number of households per season
master %>% group_by(wave) %>%
  summarise(n = length(unique(hh_id)))
## wave 1 = 977; wave 2 = 1,055; wave 3 = 530; wave 4 = 1,012

## total number of participants
n_ptc <- length(unique(master$ptc_id[master$ptc_id != 'PT_NA']))
n_ptc # 1,438 participants

## number of participants per season
master %>% 
  filter(wave != 3) %>%
  group_by(wave) %>%
  summarise(n = length(unique(ptc_id)))
## wave 1 = 1,003; wave 2 = 1,110; wave 4 = 1,028

# Observations per participant -------------------------------------------------
nobs_per_ptc <- master %>% 
  filter(wave != 3) %>%
  group_by(ptc_id) %>% 
  summarise(n = n())

## number of participants with 1, 2, or 3 observations
summary(factor(nobs_per_ptc$n))

## percent of participants with 1, 2, or 3 observations
round(summary(factor(nobs_per_ptc$n))/n_ptc*100,1)

## bar chart of the number of observations per participant
ggplot(nobs_per_ptc, aes(x = factor(n))) +
  geom_bar() +
  theme_bw() +
  labs(x = "\n Number of observations", 
       y = "Number of participants \n") +
  theme(panel.grid = element_blank())

# Total participants by years surveyed -----------------------------------------
panel_ptcs <- master %>% 
  filter(wave != 3) %>%
  group_by(ptc_id) %>% 
  summarise(n = n(),
            waves = paste(unique(wave), collapse = ",")) %>%
  ungroup()

head(panel_ptcs)
## n = number of observations per participant
## waves = waves in which participant was surveyed

## n participants by years of observation
## year 1 only (n = 109)
panel_ptcs %>% 
  filter(waves == "1") %>%
  nrow(.)

## year 2 only (n = 97)
panel_ptcs %>% 
  filter(waves == "2") %>%
  nrow(.)

## year 4 only (n = 159)
panel_ptcs %>% 
  filter(waves == "4") %>%
  nrow(.)

## years 1 & 2 only (n = 204)
panel_ptcs %>% 
  filter(waves == "1,2") %>%
  nrow(.)

## years 1 & 4 only (n = 60)
panel_ptcs %>% 
  filter(waves == "1,4") %>%
  nrow(.)

## years 2 & 4 only (n = 179)
panel_ptcs %>% 
  filter(waves == "2,4") %>%
  nrow(.)

## year 1, 2, 4 only (n = 630)
panel_ptcs %>% 
  filter(waves == "1,2,4") %>%
  nrow(.)

## number of new participants in Year 2 (n = 276)
panel_ptcs %>% 
  filter(waves %in% c("2", "2,4")) %>%
  nrow(.)

## number of new participants in Year 4 (n = 159)
panel_ptcs %>% 
  filter(waves %in% c("4")) %>%
  nrow(.)

# Total households by years surveyed -------------------------------------------
panel_hhs <- master %>%
  group_by(hh_id) %>% 
  summarise(n = n(),
            waves = paste(unique(wave), collapse = ",")) %>%
  ungroup()
## n = number of observations per household
## waves = waves in which household was surveyed

## n households by years of observation
## year 1 only (n = 62)
panel_hhs %>% 
  filter(waves == "1") %>%
  nrow(.)

## year 2 only (n = 20)
panel_hhs %>% 
  filter(waves == "2") %>%
  nrow(.)

## year 3 only (n = 2)
panel_hhs %>% 
  filter(waves == "3") %>%
  nrow(.)

## year 4 only (n = 68)
panel_hhs %>% 
  filter(waves == "4") %>%
  nrow(.)

## years 1 & 2 only (n = 80)
panel_hhs %>% 
  filter(waves == "1,2") %>%
  nrow(.)

## years 1 & 3 only (n = 0)
panel_hhs %>% 
  filter(waves == "1,3") %>%
  nrow(.)

## years 1 & 4 only (n = 48)
panel_hhs %>% 
  filter(waves == "1,4") %>%
  nrow(.)

## years 2 & 3 only (n = 7)
panel_hhs %>% 
  filter(waves == "2,3") %>%
  nrow(.)

## years 2 & 4 only (n = 116)
panel_hhs %>% 
  filter(waves == "2,4") %>%
  nrow(.)

## years 3 & 4 only (n = 0)
panel_hhs %>% 
  filter(waves == "3,4") %>%
  nrow(.)

## years 1, 2, 3 only (n = 53)
panel_hhs %>% 
  filter(waves == "1,2,3") %>%
  nrow(.)

## years 1, 2, 4 only (n = 312)
panel_hhs %>% 
  filter(waves == "1,2,4") %>%
  nrow(.)

## years 1, 3, 4 only (n = 1)
panel_hhs %>% 
  filter(waves == "1,3,4") %>%
  nrow(.)

## years 2, 3, 4 only (n = 46)
panel_hhs %>% 
  filter(waves == "2,3,4") %>%
  nrow(.)

## years 1, 2, 3, 4 (n = 421)
panel_hhs %>% 
  filter(waves == "1,2,3,4") %>%
  nrow(.)

## number of new households in Year 2 (n = 189)
panel_hhs %>% 
  filter(waves %in% c("2", "2,3", "2,3,4", "2,4")) %>%
  nrow(.)

## number of new households in Year 3 (n = 2)
panel_hhs %>% 
  filter(waves %in% c("3", "3,4")) %>%
  nrow(.)

## number of new households in Year 4 (n = 68)
panel_hhs %>% 
  filter(waves %in% c("4")) %>%
  nrow(.)

# Number of participants & households per village ------------------------------
PT_HH_per_village <- master %>% 
  group_by(ID_VILLAGE) %>%
  summarise(ptc = length(unique(ptc_id)),
            hh = length(unique(hh_id))) %>%
  ungroup() %>%
  print()

summary(PT_HH_per_village$ptc)
summary(PT_HH_per_village$hh)

# Min and max ptcs/hhs per village by study year
master %>%
  group_by(wave, ID_VILLAGE) %>%
  summarise(ptc = length(unique(ptc_id)),
            hh = length(unique(hh_id))) %>%
  ungroup() %>% 
  group_by(wave) %>%
  summarise(min_ptc = min(ptc),
            max_ptc = max(ptc),
            min_hh = min(hh),
            max_hh = max(hh)) %>%
  ungroup()

# min_ptc = smallest village participant sample size
# max_ptc = largest village participant sample size
# min_hh = smallest village household sample size
# max_hh = largest village household sample size
# i.e., in Year 1, we recruited between 11 and 24 participants per village.

# END --------------------------------------------------------------------------