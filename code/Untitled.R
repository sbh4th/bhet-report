pe_etwfe_me <- read_rds(here("outputs/models", 
  "pe_etwfe_me.rds"))
pe_etwfe_ny_me <- read_rds(here("outputs/models", 
  "pe_etwfe_ny_me.rds"))
pe_etwfe_ng_me <- read_rds(here("outputs/models", 
  "pe_etwfe_ng_me.rds"))
pe_etwfe_nfe_me <- read_rds(here("outputs/models", 
  "pe_etwfe_nfe_me.rds"))

f <- function(x) format(round(x, 0), big.mark=",")
gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = f),
  list("raw" = "FE..year", "clean" = "Year fixed effects", "fmt" = f),
  list("raw" = "FE..cohort_year", "clean" = "Cohort fixed effects", "fmt" = f),
  list("raw" = "aic", "clean" = "Covariates", "fmt" = "X"))

temp_table <- modelsummary(list("Adjusted DiD" = pe_etwfe_me, 
  "No time FE" = pe_etwfe_ny_me, 
  "No group FE" = pe_etwfe_ng_me, 
  "No FE" = pe_etwfe_nfe_me), 
  coef_rename = c("treat" = "ATT"), 
  statistic = "conf.int", fmt = 1, 
  gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE',
  gof_map = gm,
  output = "tinytable")

tt(temp_table@data,
   notes = list("Note: All models adjusted for household size, smoking, outdoor temperature, and outdoor humidity. Standard errors clustered by village.")) %>%
  style_tt(i=0, bold = TRUE) %>%
  style_tt(j = 2:5, align = "c") %>%
  format_tt(escape = TRUE)


temp_table <- read_xlsx(here("outputs",
  "chemical composition tables.xlsx"), sheet = "outdoor")




ss_table <- read_xlsx(here("outputs",
  "sample_size_table.xlsx"), skip = 2)  %>%
  select(-1)

colnames(ss_table) <- c("Outcome", 
  "Participants", "Households", "Villages",
  "Participants", "Households", "Villages",
  "Participants", "Households", "Villages",
  "Participants", "Households", "Villages",
  "Participants", "Households", "Villages")

tt(ss_table) %>%
    group_tt(
    i = list(
      "Health Measures" = 2,
      "Inflammatory biomarkers" = 5,
      "Personal air pollution" = 9,
      "Indoor air pollution" = 11,
      "Outdoor air pollution" = 14,
      "Indoor temperature" = 17),
    j = list(
      "All waves" = 2:4,
      "Wave 1" = 5:7,
      "Wave 2" = 8:10,
      "Wave 3" = 11:13,
      "Wave 4" = 14:16))

  group_tt(
    j = list("Personal PM" = 4:5,
             "Indoor Temp" = 6:7,
             "PM + Temp" = 8:9)) %>%
  group_tt(
    j = list("Adjusted Total Effect" = 2:3,
             "CDE Mediated By:" = 4:9)) %>%
