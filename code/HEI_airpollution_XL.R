#' HEI-air pollution
#' Xiaoying Li
#' Date created: 03-14-2024
#' Last update: 

# Set-up and packages ====
library(tidyverse)
library(networkD3)
library(webshot)
library(fixest)
library(DescTools)
# Table: summary of air pollution data ###########
master <- read.csv(here("data-clean", 
                        "BHET_master_air_pollution.csv"))

### define personal PM dataset #####
df_personal_pm <- master %>% 
  dplyr::filter(PM25_exp_remove == 1) %>% 
  dplyr::distinct(hh_id, wave, PM25conc_exposureugm3, .keep_all= TRUE)
### define personal BC dataset #####
df_personal_bc <- master %>% 
  dplyr::filter(bc_exp_remove == 1) %>% 
  dplyr::distinct(hh_id, wave, bc_exp_conc, .keep_all= TRUE)
### define indoor filter PM dataset #####
df_indoor_filter_pm <- master %>% 
  dplyr::filter(usable_indoor_filter == 1) %>% 
  dplyr::anti_join(master %>% 
                     filter(usable_indoor_filter == 1) %>% 
                     group_by(hh_id, wave) %>% 
                     filter(n()>1) %>% 
                     filter(is.na(house_area))) %>% 
  dplyr::anti_join(master %>% 
                     filter(usable_indoor_filter == 1) %>% 
                     group_by(hh_id, wave) %>% 
                     filter(n()>1) %>% 
                     filter(!is.na(house_area)) %>% 
                     group_by(hh_id, wave) %>% 
                     filter(n()>1)) %>% 
  dplyr::bind_rows(master %>% 
                     filter(usable_indoor_filter == 1) %>% 
                     group_by(hh_id, wave) %>% 
                     filter(n()>1) %>% 
                     filter(!is.na(house_area)) %>% 
                     group_by(hh_id, wave) %>% 
                     filter(n()>1) %>% 
                     filter(indoor_filter_type == "teflon", 
                            indoor_filter_id != "BHET2296"))

### define indoor filter BC dataset #####
df_indoor_filter_bc <- master %>% 
  dplyr::filter(bc_indoor_remove == 1) %>% 
  dplyr::anti_join(master %>% 
                     filter(bc_indoor_remove == 1) %>% 
                     group_by(hh_id, wave) %>% 
                     filter(n()>1) %>% 
                     filter(is.na(house_area)))
### define Indoor sensor PM dataset #####
df_indoor_dup <- master %>% 
  dplyr::filter(!is.na(pm2.5_indoor_sensor_24h)) %>% 
  dplyr::group_by(hh_id, wave) %>% 
  dplyr::filter(n()>1)
df_indoor_24h <- master %>% 
  filter(!is.na(pm2.5_indoor_sensor_24h)) %>% 
  anti_join(df_indoor_dup %>% 
              filter(is.na(house_area))) %>% 
  distinct(hh_id, wave, pm2.5_indoor_sensor_24h, .keep_all= TRUE)

df_indoor_seasonal <- master %>% 
  filter(N_percent_indoor_seasonal_hs >= 0.2) %>% 
  anti_join(df_indoor_dup %>% 
              filter(is.na(house_area))) %>% 
  distinct(hh_id, wave, pm2.5_indoor_seasonal_hs, .keep_all= TRUE)
### Calculation #########
#gm.ci fuction
ci.gml <- function(x){
  gm1 = mean(log(x), na.rm = T)
  cil = exp(gm1-(1.96*(sd(log(x), na.rm = T)/sqrt(length(x)))))
  vec = c(round(cil,2))
  return (vec)
}

ci.gmupp <- function(x){
  gm1 = mean(log(x), na.rm = T)
  ciupp = exp(gm1+(1.96*(sd(log(x), na.rm = T)/sqrt(length(x)))))
  vec = c(round(ciupp,2))
  return (vec)
}

df_personal_pm %>% 
  filter(wave == "S1") %>% 
  mutate(treat = case_when(coal_ban_time == 2019 ~ "Treated", 
                           coal_ban_time == 2020 ~ "Treated", 
                           coal_ban_time == 2021 ~ "Treated", 
                           coal_ban_time == "Not Yet" ~ "Control")) %>% 
  #group_by(treat) %>% 
  summarise(pm_gm = Gmean(PM25conc_exposureugm3), 
            pm_cil = ci.gml(PM25conc_exposureugm3), 
            pm_ciupp = ci.gmupp(PM25conc_exposureugm3))

df_personal_bc %>% 
  filter(wave == "S1") %>% 
  mutate(treat = case_when(coal_ban_time == 2019 ~ "Treated", 
                           coal_ban_time == 2020 ~ "Treated", 
                           coal_ban_time == 2021 ~ "Treated", 
                           coal_ban_time == "Not Yet" ~ "Control")) %>% 
  #group_by(treat) %>% 
  summarise(pm_gm = Gmean(bc_exp_conc), 
            pm_cil = ci.gml(bc_exp_conc), 
            pm_ciupp = ci.gmupp(bc_exp_conc))

df_indoor_filter_pm %>% 
  filter(wave == "S2") %>% 
  mutate(treat = case_when(coal_ban_time == 2019 ~ "Treated", 
                           coal_ban_time == 2020 ~ "Treated", 
                           coal_ban_time == 2021 ~ "Treated", 
                           coal_ban_time == "Not Yet" ~ "Control")) %>% 
  #group_by(treat) %>% 
  summarise(pm_gm = Gmean(pm2.5_indoor_filter), 
            pm_cil = ci.gml(pm2.5_indoor_filter), 
            pm_ciupp = ci.gmupp(pm2.5_indoor_filter))

df_indoor_filter_bc %>% 
  filter(wave == "S2", 
         bc_indoor_conc != 0) %>% 
  mutate(treat = case_when(coal_ban_time == 2019 ~ "Treated", 
                           coal_ban_time == 2020 ~ "Treated", 
                           coal_ban_time == 2021 ~ "Treated", 
                           coal_ban_time == "Not Yet" ~ "Control")) %>% 
  group_by(treat) %>% 
  summarise(pm_gm = Gmean(bc_indoor_conc), 
            pm_cil = ci.gml(bc_indoor_conc), 
            pm_ciupp = ci.gmupp(bc_indoor_conc))

df_outdor_filter_pm <- read.csv("../Data/outdoor pm and bc in S1.csv", head(T)) %>% 
  filter(usable_pm == 1)
df_outdor_filter_pm %>% 
  #group_by(treat) %>% 
  summarise(pm_gm = Gmean(pm2.5), 
            pm_cil = ci.gml(pm2.5), 
            pm_ciupp = ci.gmupp(pm2.5))

df_outdor_filter_bc <- read.csv("../Data/outdoor pm and bc in S1.csv", head(T)) %>% 
  filter(usable_bc == 1)
df_outdor_filter_bc %>% 
  #group_by(treat) %>% 
  summarise(pm_gm = Gmean(bc), 
            pm_cil = ci.gml(bc), 
            pm_ciupp = ci.gmupp(bc))

# Sankey diagram - four categories ###########
## Untreated ####
links2 <- read.csv("../Data/snakey_links_four_03mar24.csv", head(T)) %>% 
  filter(status == "Untreated")
nodes2 <- read.csv("../Data/snakey_nodes_four_03mar24.csv", head(T))

node_color <- 'd3.scaleOrdinal() 
.domain(["Clean", "Clean-", "Solid", "Solid+",]) 
.range(["#4dac26", "#b8e186", "#f1b6da", "#d01c8b"])'

p <- sankeyNetwork(Links = links2, Nodes = nodes2,
                   Source = "source", Target = "target",
                   Value = "values", NodeID = "name", 
                   fontSize = 0, fontFamily = "Arial", 
                   nodeWidth = 20, iterations = 0, colourScale = node_color, 
                   sinksRight = FALSE, width = 800, height = 500)
p
saveNetwork(p, "Sankey diagram_four_Untreated.html")

## Treated in S2 ####
links2 <- read.csv("../Data/snakey_links_four_03mar24.csv", head(T)) %>% 
  filter(status == "Treated in S2")
nodes2 <- read.csv("../Data/snakey_nodes_four_03mar24.csv", head(T))

node_color <- 'd3.scaleOrdinal() 
.domain(["Clean", "Clean-", "Solid", "Solid+",]) 
.range(["#4dac26", "#b8e186", "#f1b6da", "#d01c8b"])'

p <- sankeyNetwork(Links = links2, Nodes = nodes2,
                   Source = "source", Target = "target",
                   Value = "values", NodeID = "name", 
                   fontSize = 0, fontFamily = "Arial",
                   nodeWidth = 20, iterations = 0, colourScale = node_color, 
                   sinksRight = FALSE, width = 800, height = 500)
p
saveNetwork(p, "Sankey diagram_four_Treated in S2.html")

## Treated in S4 ####
links2 <- read.csv("../Data/snakey_links_four_03mar24.csv", head(T)) %>% 
  filter(status == "Treated in S4")
nodes2 <- read.csv("../Data/snakey_nodes_four_03mar24.csv", head(T))

node_color <- 'd3.scaleOrdinal() 
.domain(["Clean", "Clean-", "Solid", "Solid+",]) 
.range(["#4dac26", "#b8e186", "#f1b6da", "#d01c8b"])'

p <- sankeyNetwork(Links = links2, Nodes = nodes2,
                   Source = "source", Target = "target",
                   Value = "values", NodeID = "name", 
                   fontSize = 0, fontFamily = "Arial",
                   nodeWidth = 20, iterations = 0, colourScale = node_color, 
                   sinksRight = FALSE, width = 800, height = 500)
p
saveNetwork(p, "Sankey diagram_four_Treated in S4.html")

# Sankey diagram - three categories ###########
## Untreated ####
links2 <- read.csv("../Data/snakey_links_three_heatpump_03mar24.csv", head(T)) %>% 
  filter(status == "Untreated")
nodes2 <- read.csv("../Data/snakey_nodes_three_heatpump_03mar24.csv", head(T))

node_color <- 'd3.scaleOrdinal() 
.domain(["Clean energy", "Mixed energy", "Solid fuel",]) 
.range(["#4dac26", "#b8e186", "#d01c8b"])'

p <- sankeyNetwork(Links = links2, Nodes = nodes2,
                   Source = "source", Target = "target",
                   Value = "values", NodeID = "name", 
                   fontSize = 0, fontFamily = "Arial",
                   nodeWidth = 20, iterations = 0, colourScale = node_color, 
                   sinksRight = FALSE, width = 800, height = 500)
p
saveNetwork(p, "Sankey diagram_three_Untreated.html")

## Treated in S2 ####
links2 <- read.csv("../Data/snakey_links_three_heatpump_03mar24.csv", head(T)) %>% 
  filter(status == "Treated in S2")
nodes2 <- read.csv("../Data/snakey_nodes_three_heatpump_03mar24.csv", head(T))

node_color <- 'd3.scaleOrdinal() 
.domain(["Clean energy", "Mixed energy", "Solid fuel",]) 
.range(["#4dac26", "#b8e186", "#d01c8b"])'

p <- sankeyNetwork(Links = links2, Nodes = nodes2,
                   Source = "source", Target = "target",
                   Value = "values", NodeID = "name", 
                   fontSize = 0, fontFamily = "Arial",
                   nodeWidth = 20, iterations = 0, colourScale = node_color, 
                   sinksRight = FALSE, width = 800, height = 500)
p
saveNetwork(p, "Sankey diagram_three_Treated in S2.html")

## Treated in S4 ####
links2 <- read.csv("../Data/snakey_links_three_heatpump_03mar24.csv", head(T)) %>% 
  filter(status == "Treated in S4")
nodes2 <- read.csv("../Data/snakey_nodes_three_heatpump_03mar24.csv", head(T))

node_color <- 'd3.scaleOrdinal() 
.domain(["Clean energy", "Mixed energy", "Solid fuel",]) 
.range(["#4dac26", "#b8e186", "#d01c8b"])'

p <- sankeyNetwork(Links = links2, Nodes = nodes2,
                   Source = "source", Target = "target",
                   Value = "values", NodeID = "name", 
                   fontSize = 0, fontFamily = "Arial",
                   nodeWidth = 20, iterations = 0, colourScale = node_color, 
                   sinksRight = FALSE, width = 800, height = 500)
p
saveNetwork(p, "Sankey diagram_three_Treated in S4.html")

# Summary of energy use patterns ##############
master_energy <- read.csv("../Data/BHET_master_air_pollution.csv") %>% 
  dplyr::distinct(hh_id, wave, .keep_all= TRUE) %>% 
  dplyr::group_by(wave, treatment_wave) %>% 
  dplyr::summarise(n = n())
master_energy <- read.csv("../Data/BHET_master_air_pollution.csv") %>% 
  dplyr::distinct(hh_id, wave, .keep_all= TRUE) %>% 
  dplyr::group_by(wave, treatment_wave, Heating_fuel_four) %>% 
  dplyr::summarise(n = n())

# Difference-in-difference ###########
master <- read.csv("../Data/BHET_master_air_pollution.csv")

### define personal PM dataset #####
df_personal_pm <- master %>% 
  dplyr::filter(PM25_exp_remove == 1) %>% 
  dplyr::distinct(hh_id, wave, PM25conc_exposureugm3, .keep_all= TRUE)
### define personal BC dataset #####
df_personal_bc <- master %>% 
  dplyr::filter(bc_exp_remove == 1) %>% 
  dplyr::distinct(hh_id, wave, bc_exp_conc, .keep_all= TRUE)
### define Indoor sensor PM dataset #####
df_indoor_dup <- master %>% 
  dplyr::filter(!is.na(pm2.5_indoor_sensor_24h)) %>% 
  dplyr::group_by(hh_id, wave) %>% 
  dplyr::filter(n()>1)

df_indoor_24h <- master %>% 
  filter(!is.na(pm2.5_indoor_sensor_24h)) %>% 
  anti_join(df_indoor_dup %>% 
              filter(is.na(house_area))) %>% 
  distinct(hh_id, wave, pm2.5_indoor_sensor_24h, .keep_all= TRUE)

df_indoor_seasonal <- master %>% 
  filter(N_percent_indoor_seasonal_hs >= 0.2) %>% 
  anti_join(df_indoor_dup %>% 
              filter(is.na(house_area))) %>% 
  distinct(hh_id, wave, pm2.5_indoor_seasonal_hs, .keep_all= TRUE)

## DID: Personal PM ##########
#### Fixed effects of Village + Year ###
personal_TWFE_2 <- feols(PM25conc_exposureugm3~ Treat_V|vid + year, 
                         vcov = ~ vid, data = df_personal_pm)
summary(personal_TWFE_2)
confint(personal_TWFE_2)
personal_TWFE_2
#### Fixed effects of household + Year + controls ###
personal_TWFE_4 <- feols(PM25conc_exposureugm3 ~ Treat_V + ptc_smoking + outdoor_temp_24h + outdoor_dew_24h + 
                           hh_num|hh_id + year, 
                         vcov = ~ vid, data= df_personal_pm)
summary(personal_TWFE_4)
confint(personal_TWFE_4)
personal_TWFE_4

## DID: Personal BC ##################
####Fixed effects of Village + Year###
personal_TWFE_2 <- feols(bc_exp_conc~ Treat_V|vid + year, 
                         vcov = ~ vid, data = df_personal_bc)
summary(personal_TWFE_2)
confint(personal_TWFE_2)
personal_TWFE_2
####Fixed effects of household + Year + controls ###
personal_TWFE_4 <- feols(bc_exp_conc ~ Treat_V + ptc_smoking + outdoor_temp_24h + outdoor_dew_24h + 
                           hh_num|hh_id + year, 
                         vcov = ~ vid, data= df_personal_bc)
summary(personal_TWFE_4)
confint(personal_TWFE_4)
personal_TWFE_4

## DID: Indoor-24h #########
####Fixed effects of Village + Year###
indoor_TWFE_2 <- feols(pm2.5_indoor_sensor_24h~ Treat_V|vid + year, 
                       vcov = ~ vid, data = df_indoor_24h)
summary(indoor_TWFE_2)
confint(indoor_TWFE_2)
indoor_TWFE_2
####Fixed effects of household + Year + controls ###
indoor_TWFE_4 <- feols(pm2.5_indoor_sensor_24h ~ Treat_V + hh_smoking + outdoor_temp_24h + outdoor_dew_24h + 
                         hh_num|hh_id + year, 
                       vcov = ~ vid, data= df_indoor_24h)
summary(indoor_TWFE_4)
confint(indoor_TWFE_4)
indoor_TWFE_4

## DID: Indoor-seasonal #########
####Fixed effects of Village + Year###
indoor_TWFE_2 <- feols(pm2.5_indoor_seasonal_hs~ Treat_V|vid + year, 
                       vcov = ~ vid, data = df_indoor_seasonal)
summary(indoor_TWFE_2)
confint(indoor_TWFE_2)
indoor_TWFE_2
####Fixed effects of household + Year + controls ###
indoor_TWFE_4 <- feols(pm2.5_indoor_seasonal_hs ~ Treat_V + hh_smoking + outdoor_temp_seasonal + outdoor_dew_seasonal + 
                         hh_num|hh_id + year, 
                       vcov = ~ vid, data= df_indoor_seasonal)
summary(indoor_TWFE_4)
confint(indoor_TWFE_4)
indoor_TWFE_4

# Plot treatment effect ##############
df <- read.csv("../Data/DID_air_pollution.csv") %>% 
  mutate(Pollutant = case_when(Pollutant == "PM2.5" ~ "PM[2.5]", 
                               TRUE ~ Pollutant))
df$Category = factor(df$Category, levels = c("Outdoor", "Indoor", "Personal"))
df$Pollutant = factor(df$Pollutant, levels = c("PM[2.5]", "Daily", "BC", "Seasonal"))

ggplot(df, aes(y= Effect, x = Estimate)) + 
  geom_point(aes(shape = Effect), size = 3.5, stroke = 1.5) + 
  geom_errorbar(aes(xmin = CI_low, xmax = CI_upper), size = 1, width = 0.2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  facet_nested_wrap(~ Category + Pollutant, nrow = 3, scales = "free_x", labeller = label_parsed) + 
  scale_shape_manual(values = c(16, 1)) + 
  theme_bw() + 
  theme(strip.text = element_text(size = 16), 
        strip.background = element_rect(color="black", size=0.8, linetype="solid"), 
        axis.title = element_text(size=16, color="black"),
        axis.text.x = element_text(size = 14, color = "black"), 
        axis.text.y = element_text(size = 14, color = "black"), 
        panel.border = element_rect(size = 0.8, colour = "black"),
        legend.position = "none", 
        legend.background = element_blank(), 
        legend.title = element_blank(), 
        legend.key.size = unit(0.8, "cm"), 
        legend.text = element_text(size = 16, color = "black"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "grey"), 
        panel.grid.minor = element_blank()) + 
  labs(x = bquote(ATT~'(μg' ~ m^-3*')'), y = "")
ggsave("../Figure/Treatment effect_personal.png", width = 6, height = 8)


library(fastDummies)
library(modeldb)
p_rep <- df_personal_pm %>%
  select(PM25conc_exposureugm3, Treat_V, vid, year, wave,
    ban_status_composite, hh_id, ptc_id, ID_VILLAGE,
    ptc_smoking, outdoor_temp_24h, outdoor_dew_24h, hh_num) %>%

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

ettw <- fixest::feols(
  PM25conc_exposureugm3 ~ treat | factor(v_id) + 
    factor(year),
    data = p_rep,
    vcov = ~factor(v_id)
)

etppm <- fixest::feols(
  PM25conc_exposureugm3 ~ treat:i(cohort_year, i.factor(year), 
    ref=-Inf, ref2 = 2018) | factor(cohort_year) + 
    factor(year),
    data = p_rep,
    vcov = ~factor(v_id)
)

etppma <- fixest::feols(
  PM25conc_exposureugm3 ~ treat:i(cohort_year, i.factor(year), 
    ref=-Inf, ref2 = 2018) + ptc_smoking + outdoor_temp_24h + 
    outdoor_dew_24h | factor(cohort_year) + factor(year),
    data = p_rep,
    vcov = ~factor(v_id)
)

bme_pred <- predictions(
  b2, 
  newdata   = subset(d2, treat==1),
  variables = "treat", 
  by        = "treat"
  )


library(brms)
library(cmdstanr)
options(mc.cores = 4,
        brms.backend = "cmdstanr")
b2 <-
  brm(data = p_rep, 
      family = gaussian(),
      PM25conc_exposureugm3 ~ 1 + (1 | v_id) + 
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,
      prior = c(prior(normal(100, 50), class = Intercept),
        prior(normal(0, 10), class = b),
        prior(exponential(1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      adapt_delta = 0.9,
      sample_prior = "yes")

b2_s <-
  brm(data = p_rep, 
      family = skew_normal(),
      PM25conc_exposureugm3 ~ 1 + (1 | v_id) + 
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,
      prior = c(prior(normal(100, 50), class = Intercept),
        prior(normal(0, 10), class = b),
        prior(exponential(1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      adapt_delta = 0.9,
      sample_prior = "yes")



bc_rep <- df_personal_bc %>%
  select(bc_exp_conc, Treat_V, vid, year, wave,
    ban_status_composite, hh_id, ptc_id, ID_VILLAGE,
    ptc_smoking, outdoor_temp_24h, outdoor_dew_24h, hh_num) %>%

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


ettw_bc <- fixest::feols(
  bc_exp_conc ~ treat | factor(v_id) + 
    factor(year),
    data = bc_rep,
    vcov = ~factor(v_id)
)

etppm_bc <- fixest::feols(
  bc_exp_conc ~ treat:i(cohort_year, i.factor(year), 
    ref=-Inf, ref2 = 2018) | factor(cohort_year) + 
    factor(year),
    data = bc_rep,
    vcov = ~factor(v_id)
)

etppm_bca <- fixest::feols(
  bc_exp_conc ~ treat:i(cohort_year, i.factor(year), 
    ref=-Inf, ref2 = 2018) + ptc_smoking + outdoor_temp_24h + 
    outdoor_dew_24h | factor(cohort_year) + factor(year),
    data = bc_rep,
    vcov = ~factor(v_id)
)


bc2 <-
  brm(data = bc_rep, 
      family = gaussian(),
      bc_exp_conc ~ 1 + (1 | v_id) + 
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,
      prior = c(prior(normal(0, 10), class = Intercept),
        prior(normal(0, 1), class = b),
        prior(exponential(1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      adapt_delta = 0.9,
      sample_prior = "yes")

# test for heterogeneity across treatment cohorts

# first test for differences across treatment effects
het <- slopes(
  etppm_bc, 
  newdata   = subset(bc_rep, treat==1),
  variables = "treat", 
  by        = "cohort_year", 
  hypothesis = c("b1 - b2 = 0", "b1 - b3 = 0")
  ) 

# now run a joint test that all of those differences are zero:
hypotheses(het, joint=TRUE)

