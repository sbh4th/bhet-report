dv2 <- d_personal %>% 
  group_by(v_id, wave) %>% 
    summarise(
      mean_pe = mean(pe, na.rm = T),
      mean_bc = mean(bc, na.rm = T),
      med_pe = median(pe, na.rm = T),
      med_bc = median(bc, na.rm = T),
      mean_pe_se = sd(pe, na.rm = T)/sqrt(n()),
      pop = n(), 
      ban = max(ban_status_composite)) %>%
  
    # treatment related variables
    mutate(
      year = if_else(wave==1, 2018, 
      if_else(wave==2, 2019,
        if_else(wave==4, 2021, 0))),
    cohort_year = if_else(
      ban==1, 2019, 
      if_else(ban==2, 2020, 
              if_else(ban==3, 2021, 2022))),
    treat = ifelse(year >= cohort_year, 1, 0),
    cohort_year = ifelse(cohort_year == 2022,-Inf, 
                         cohort_year)) %>% 
  
  # treatment cohort dummies
  add_dummy_variables(cohort_year, 
    values=c(-Inf,2019,2020,2021), 
    remove_original = F) %>%
  
  
  # wave dummies
  add_dummy_variables(year, 
    values=c(2018,2019,2021), remove_original = F) 


pe_vlm <- glm(med_pe ~ 
        treat:cohort_year_2019:year_2019 + 
        treat:cohort_year_2019:year_2021 +
        treat:cohort_year_2020:year_2021 +
        treat:cohort_year_2021:year_2021 +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + 
        year_2021, data = dv2, weights = pop,
        family = gaussian(link = "log"))

bme_pred <- predictions(
  pe_vlm, 
  newdata   = subset(dv2, treat==1),
  variables = "treat", 
  by        = "treat"
  )

bme_avg <- slopes(
  pe_vlm, 
  newdata   = subset(dv2, treat==1),
  variables = "treat", 
  by        = "treat"
  ) 

# wrangle aggregate ATTs for model summary table
att_did_pe_vlm <- att_table(preds = bme_pred, 
  meffs = bme_avg)

modelsummary(list(
  "Gaussian" = att_did_gaussian,
  "Skew normal" = att_did_skew,
  "Lognormal" = att_did_log,
  "Village means" = att_did_pe_v,
  "Village logn" = att_did_pe_vl,
  "Village med log" = att_did_pe_vlm), 
  fmt=1, 
  statistic = c("std.error", "conf.int"))
