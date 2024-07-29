library(here)
library(osfr)
library(tidyverse)
library(kableExtra)
library(modelsummary)
library(readxl)
library(marginaleffects)
library(tinytable)
library(tidymodels)

options(knitr.kable.NA = "\\")
tx <- tibble(
  p = factor(rep(c("New recruitment",
                   "Wave 1 households", "Wave 2 households",
                   "Total recruitment"),2), levels = c("New recruitment",
                                                       "Wave 1 households", "Wave 2 households",
                                                       "Total recruitment")),
  g = c(1,1,1,1,2,2,2,2),
  s1 = c(977,NA,NA,977, 0,NA,NA,0),
  s2 = c(196,866,NA, 1062, 300, 0,NA, 300),
  s4 = c(68, 780, 162, 1010, 52, 0, 248, 300)
)

tx2 <- tx %>% pivot_wider(names_from = g,
                          values_from = c(s1, s2, s4), 
                          names_vary = "slowest") %>%
  mutate(s3_2 = c(0,0,246,246)) %>%
  relocate(p,s1_1,s2_1,s4_1,s1_2,s2_2,s3_2,s4_2)

kable(tx2, 
      col.names = c("Sample", "Wave 1", "Wave 2", "Wave 4", 
                    "Wave 1", "Wave 2", "Wave 3", "Wave 4"),
      "latex", booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, 
                     "Overall" = 3, "Indoor" = 4))

dst <- read_csv(here("data-clean", 
  "BHET_master_data_05Mar2024.csv"),
  col_select = c("wave", "gender_health", "age_health",
      "smoking", "waist_circ", "height", "ptc_id",
      "weight", "lived_with_smoker", "ID_VILLAGE")) %>%
  filter(wave != 3) %>%
  add_count(ptc_id) %>%
  mutate(wave = as.factor(n),
    bmi = (weight / (height/100)^2),
    female = if_else(gender_health == 2 & !is.na(gender_health),
      1, 0),
    csmoke = if_else(smoking==1 & !is.na(smoking), 1, 0),
    asmoke = if_else(smoking < 3, 1, 
      if_else(smoking==3 & lived_with_smoker %in% 
      c(2,3), 1, 0))) %>%
  mutate(across(c(female, csmoke, asmoke), as.integer))


infer::chisq_test(dst, nwaves ~ female)

ds %>%
  infer::specify(bmi ~ wave) %>%
  fit() %>%
  get_p_value(direction = "two-sided")


# air pollution results
ap_table <- read_rds(here("outputs", 
  "ap-etwfe-table.rds")) 

# temperature results
temp_table <- tibble(
  category = "Point", outcome = "Mean", estimate_1 = 1.96,
  ci_1 = "(0.96, 2.96)", estimate_2 = 1.96, 
  ci_2 = "(0.96, 2.96)"
)

stemp_table <- read_xlsx(here("data-clean",
  "overall_temp_table.xlsx")) %>%
  rename(`estimate_1` = att,
         `ci_1` = ci) %>%
  mutate(category = rep("Seasonal", times = 6),
    outcome = c("Mean (all)", "Mean (daytime)", 
      "Mean (heating season)", "Mean (daytime heating season)",
      "Min. (all)", "Min. (heating season)"),
    estimate_2 = `estimate_1`, 
    ci_2 = `ci_1`) %>%
  select(-model) %>%
  relocate(category, outcome)

# join tables
m_table <- bind_rows(ap_table, temp_table, 
  stemp_table)

colnames(m_table) <- c(" ", " ", "ATT", "(95% CI)", 
  "ATT", "(95% CI)")

tt(m_table,
   digits = 2,
  #width = c(3.5, 3, 1, 0.5, 2, 0.5, 2, 0.5, 2, 0.5, 2),
  notes = list("Note: ATT = Average Treatment Effect on the # Treated, DiD = Difference-in-Differences, ETWFE = Extended Two-Way # Fixed Effects.", a = list(i=0, j=5,
     text = "ETWFE models for air pollution outcomes were adjusted for household size, smoking, outdoor temperature, and outdoor humidity. Temperature models not additionally adjusted."))) %>%
  group_tt(
    j = list("DiD" = 3:4, 
             "Adjusted DiD" = 5:6),
    i = list("Air pollution" = 1, 
             "Indoor temperature" = 7)) %>%
  style_tt(i = c(1, 8), align = "l", bold=T) %>%
  style_tt(
    i = c(2, 4, 6), j = 1, 
    rowspan = 2, alignv = "t") %>%
  style_tt(
    i = 10, j = 1, rowspan = 6, alignv = "t") %>%
  style_tt(j = 1:6, align = "llcccc")

  style_tt(
    i = c(7, 14), j = 1, rowspan = 2, alignv = "t") %>%
  style_tt(j=1:11, fontsize = 0.8) %>%
  style_tt(i = 0, align = "c")
