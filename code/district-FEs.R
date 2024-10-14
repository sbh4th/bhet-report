pe_a <- fixest::feglm(
  pe ~ treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + ets_former + ets_lived + ets_none + 
    ns(out_temp, df = 2) + ns(out_dew, df = 2) |  
    cohort_year + year, 
    data = d_p, family = Gamma(link = "log"), 
    cluster = ~v_id)

pe_a_me <- slopes(pe_a, 
  newdata = subset(d_p, treat==1), 
  variables = "treat", by = "treat")

pe_ad <- fixest::feglm(
  pe ~ treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + ets_former + ets_lived + ets_none + 
    ns(out_temp, df = 2) + ns(out_dew, df = 2) |  
    cohort_year + year + ID_COUNTY, 
    data = d_p, family = Gamma(link = "log"), 
    cluster = ~v_id)

pe_ad_me <- slopes(pe_ad, 
  newdata = subset(d_p, treat==1), 
  variables = "treat", by = "treat")


bc_a <- fixest::feglm(
  bc ~ treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + ets_former + ets_lived + ets_none + 
    ns(out_temp, df = 2) + ns(out_dew, df = 2) |  
    cohort_year + year, 
    data = d_bc, family = Gamma(link = "log"), 
    cluster = ~v_id)

bc_a_me <- slopes(bc_a, 
  newdata = subset(d_bc, treat==1), 
  variables = "treat", by = "treat")

bc_ad <- fixest::feglm(
  bc ~ treat:cohort_year_2019:year_2019 + 
    treat:cohort_year_2019:year_2021 + 
    treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 +
    hh_num + ets_former + ets_lived + ets_none + 
    ns(out_temp, df = 2) + ns(out_dew, df = 2) |  
    cohort_year + year + ID_COUNTY, 
    data = d_bc, family = Gamma(link = "log"), 
    cluster = ~v_id)

bc_ad_me <- slopes(bc_ad, 
  newdata = subset(d_bc, treat==1), 
  variables = "treat", by = "treat")


i24_a <- fixest::feglm(
  i24 ~ treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 + 
    hh_num + ets_former + ets_lived + ets_none + 
    ns(out_temp, df = 2) + ns(out_dew, df = 2) |  
    cohort_year + year, 
    data = d_i24, family = Gamma(link = "log"), 
    cluster = ~v_id)

i24_a_me <- slopes(i24_a, 
  newdata = subset(d_i24, treat==1), 
  variables = "treat", by = "treat")

write_rds(pe_etwfe_ng_me, file = here("outputs/models", 
  "pe_etwfe_ng_me.rds"))

i24_ad <- fixest::feglm(
  i24 ~ treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 + 
    hh_num + ets_former + ets_lived + ets_none + 
    ns(out_temp, df = 2) + ns(out_dew, df = 2) |  
    cohort_year + year + ID_COUNTY, 
    data = d_i24, family = Gamma(link = "log"), 
    cluster = ~v_id)

i24_ad_me <- slopes(i24_ad, 
  newdata = subset(d_i24, treat==1), 
  variables = "treat", by = "treat")

is_a <- fixest::feglm(
  is ~ treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 + 
    hh_num + ets_former + ets_lived + ets_none + 
    ns(out_temp, df = 2) + ns(out_dew, df = 2) |  
    cohort_year + year, 
    data = d_is, family = Gamma(link = "log"), 
    cluster = ~v_id)

is_a_me <- slopes(is_a, 
  newdata = subset(d_is, treat==1), 
  variables = "treat", by = "treat")


is_ad <- fixest::feglm(
  is ~ treat:cohort_year_2020:year_2021 + 
    treat:cohort_year_2021:year_2021 + 
    hh_num + ets_former + ets_lived + ets_none + 
    ns(out_temp, df = 2) + ns(out_dew, df = 2) |  
    cohort_year + year + ID_COUNTY, 
    data = d_is, family = Gamma(link = "log"), 
    cluster = ~v_id)

is_ad_me <- slopes(is_ad, 
  newdata = subset(d_is, treat==1), 
  variables = "treat", by = "treat")

f <- function(x) format(round(x, 0), big.mark=",")
gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = f),
  list("raw" = "FE..year", "clean" = "Year fixed effects", "fmt" = f),
  list("raw" = "FE..cohort_year", "clean" = "Cohort fixed effects", "fmt" = f),
  list("raw" = "FE..ID_COUNTY", "clean" = "District fixed effects", "fmt" = f))

temp_t <- modelsummary(list(pe_a_me, pe_ad_me,
  bc_a_me, bc_ad_me, i24_a_me, i24_ad_me,
  is_a_me, is_ad_me), 
  coef_rename = c("treat" = "ATT"), 
  statistic = "conf.int", fmt = 1, 
  gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE',
  gof_map = gm,
  output = "tinytable")

tt(temp_t@data,
   notes = list("Note: All models adjusted for household size, smoking, outdoor temperature, and outdoor dewpoint. Standard errors clustered by village and 95% confidence intervals shown in brackets.")) %>%
  group_tt(j = list(
    "Personal PM2.5" = 2:3,
    "Black carbon" = 4:5,
    "24-hr indoor" = 6:7,
    "Seasonal indoor" = 8:9)) %>%
  style_tt(i=0, bold = TRUE) %>%
  style_tt(j = 2:9, align = "c") %>%
  format_tt(escape = TRUE)
  