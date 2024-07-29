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


kable(otm, digits = 2, longtable = T,
      col.names = c("", "ATT", "(95%CI)",
                    "ATT", "(95%CI)", "ATT", "(95%CI)", "ATT", "(95%CI)"),
      align = 'lcccccc') %>%
  kable_styling(font_size = 10,
                latex_options = "HOLD_position") %>%
  add_header_above(c(" " = 3, 
                     "Indoor PM" = 2, "Indoor Temp" = 2, "PM + Temp" = 2)) %>%
  add_header_above(c(" " = 1, 
                     "Adjusted Total Effect\\\\textsuperscript{a}" = 2,
                     "CDE Mediated By:\\\\textsuperscript{b}" = 6),
                   escape = F) %>%
  footnote(
    general = "\\\\small{}",
    alphabet = c("\\\\small{Adjusted for age, sex, waist circumference, smoking, alcohol consumption, and use of blood pressure medication.}", "\\\\small{Mediators were set to the mean value for untreated participants at baseline.}"), 
    general_title = "", threeparttable = T, escape = F)

colnames(otm) <- c("", "ATT", "(95%CI)", "ATT", 
  "(95%CI)", "ATT", "(95%CI)", "ATT", "(95%CI)")

tt(otm,
   digits = 2,
   #width = c(3.5, 3, 1, 0.5, 2, 0.5, 2, 0.5, 2, 0.5, 2),
   notes = list("Note: Results combined across 30 multiply-imputed datasets. ATT = Average Treatment Effect on the Treated, CDE = Controlled Direct Effect, DBP = Diastolic blood pressure, SBP = Systolic blood pressure.", a = list(i=0, j=2, text = "Adjusted for age, sex, waist circumference, smoking, alcohol consumption, and use of blood pressure medication."), b = list(i=0, j=c(4,6,8), text = "Mediators were set to the mean value for untreated participants at baseline."))) %>%
  group_tt(
    j = list("Indoor PM" = 4:5,
             "Indoor Temp" = 6:7,
             "PM + Temp" = 8:9)) %>%
  group_tt(
    j = list("Adjusted Total Effect\\\\textsuperscript{a}" = 2:3,
             "CDE Mediated By:" = 4:9)) %>%
  style_tt(j = 1:9, align = "lcccccccc") %>%
  format_tt(escape = TRUE) 
%>%

  style_tt(i = c(1, 10, 18), align = "l", bold=T) %>%
  style_tt(
    i = c(2, 4, 6, 8), j = 1, 
    rowspan = 2, alignv = "t") %>%
  style_tt(
    i = 11, j = 1, rowspan = 6, alignv = "t") %>%
  style_tt(j = 1:6, align = "llcccc") %>%
  format_tt(escape = TRUE) %>%
  format_tt(j=c(3,5), sprintf = "%.2f") 



