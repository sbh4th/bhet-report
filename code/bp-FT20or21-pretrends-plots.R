## 0 Load needed packages ----
library(here)
library(tidyverse)
library(osfr)
library(fixest)
library(marginaleffects)
library(modelsummary)
library(kableExtra)
library(patchwork)

# set theme for pre-trends
theme_pt <- function() {
  theme_classic() +
    theme(axis.title = element_text(size=14),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          plot.subtitle = element_text(size = 12))
}

## 1 Set data ------------------------------------------------------------------
# read in clean BP data
d_bp <- read_csv(here("Master-data", "Processed-data",
                      "BHET_master_data_22Jul2024.csv"),
                 col_select = c(1:7, "sys_brachial", "sys_central", 
                                "dia_brachial", "dia_central", contains('ban_status'))) %>%
  # remove wave 3 - no health data
  filter(wave !=3) %>%
  
  # add 'year' based on 'wave'
  mutate(year = case_when(wave == 1 ~ 2018,
                          wave == 2 ~ 2019,
                          wave == 3 ~ 2020,
                          wave == 4 ~ 2021,
                          TRUE ~ NA)) %>%
  # add 'cohort_year' based on 'ban_status'
  mutate(cohort_year_2019 = ban_status_2019,
         cohort_year_2020 = ban_status_2020,
         cohort_year_2021 = ban_status_2021)

# Limit to pre-intervention years/cohort
d_ft20or21 <- d_bp %>% 
  filter(year %in% c(2018, 2019)) %>%                                                        # Looking from waves 1 to 2
  filter(cohort_year_2020 == 1 | cohort_year_2021 == 1 | ban_status_composite == 0) %>%      # among villages that were later-treated (in 2020 or 2021) vs. never treated.
  mutate(tc = factor(ban_status_composite,                                                   # set treatment cohort labels
                     labels = c("Never", "2020", "2021")))

## 2 bSBP ----------------------------------------------------------------------
# estimate model
pt_bSBP <- glm(sys_brachial ~ year * tc,
            data = d_ft20or21)

# Time trends by treatment status
avg_comparisons(pt_bSBP,                          # group-time regression model
                var = "year",                     # estimate contrasts from 2018-2019
                by = "tc",                        # look at contrasts by treatment group
                vcov = ~ ID_VILLAGE)              # cluster standard errors at the village-level

# Difference in time trends by treatment
pt_pt_bSBP <- 
  avg_comparisons(pt_bSBP,
                  var = "year",
                  by = "tc",
                  hypothesis = c("b2 - b1 = 0", "b3 - b1 = 0"),
                  vcov = ~ ID_VILLAGE)

# Joint test for differential trends
pt_pjt_bSBP <- hypotheses(pt_pt_bSBP, joint = TRUE)

# Gather estimates for difference in pre-trends
pt_text <- "Joint F-test of equal trends by cohort:"

pt_bSBP_stats <- paste("F(", pt_pjt_bSBP$df1, ", ",
                       pt_pjt_bSBP$df2, ") = ",
                       sprintf('%.2f', pt_pjt_bSBP$statistic) , ", p = ",
                       sprintf('%.3f', pt_pjt_bSBP$p.value), sep="")

pt_bSBP_test <- paste(pt_text, pt_bSBP_stats, sep = "\n")

# Plot of trends and estimates
pt_bSBP_plot <- plot_predictions(pt_bSBP, condition = c("year", "tc")) + 
  scale_y_continuous(limits = c(50, 150)) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  labs(subtitle = pt_bSBP_test,
       y = expression("Brachial SBP (mmHg)"),
       x = "") +
  scale_color_manual(name = "Treatment\ncohort",
                     labels = c("Never", "2020", "2021"),
                     values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  scale_fill_manual(name = "Treatment\ncohort",
                    labels = c("Never", "2020", "2021"),
                    values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  theme_pt()

## 3 cSBP ----------------------------------------------------------------------
# estimate model 
pt_cSBP <- glm(sys_central ~ year * tc,
               data = d_ft20or21)

# Time trends by treatment status
avg_comparisons(pt_cSBP,                          # group-time regression model
                var = "year",                     # estimate contrasts from 2018-2019
                by = "tc",                        # look at contrasts by treatment group
                vcov = ~ ID_VILLAGE)              # cluster standard errors at the village-level

# Difference in time trends by treatment
pt_pt_cSBP <- 
  avg_comparisons(pt_cSBP,
                  var = "year",
                  by = "tc",
                  hypothesis = c("b2 - b1 = 0", "b3 - b1 = 0"),
                  vcov = ~ ID_VILLAGE)

# Joint test for differential trends
pt_pjt_cSBP <- hypotheses(pt_pt_cSBP, joint = TRUE)

# Gather estimates for difference in pre-trends
pt_text <- "Joint F-test of equal trends by cohort:"

pt_cSBP_stats <- paste("F(", pt_pjt_cSBP$df1, ", ",
                       pt_pjt_cSBP$df2, ") = ",
                       sprintf('%.2f', pt_pjt_cSBP$statistic) , ", p = ",
                       sprintf('%.3f', pt_pjt_cSBP$p.value), sep="")

pt_cSBP_test <- paste(pt_text, pt_cSBP_stats, sep = "\n")

# Plot of trends and estimates
pt_cSBP_plot <- plot_predictions(pt_cSBP, condition = c("year", "tc")) + 
  scale_y_continuous(limits = c(50, 150)) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  labs(subtitle = pt_cSBP_test,
       y = expression("Central SBP (mmHg)"),
       x = "") +
  scale_color_manual(name = "Treatment\ncohort",
                     labels = c("Never", "2020", "2021"),
                     values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  scale_fill_manual(name = "Treatment\ncohort",
                    labels = c("Never", "2020", "2021"),
                    values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  theme_pt()

## 4 bDBP ----------------------------------------------------------------------
# estimate model 
pt_bDBP <- glm(dia_brachial ~ year * tc,
               data = d_ft20or21)

# Time trends by treatment status
avg_comparisons(pt_bDBP,                          # group-time regression model
                var = "year",                     # estimate contrasts from 2018-2019
                by = "tc",                        # look at contrasts by treatment group
                vcov = ~ ID_VILLAGE)              # cluster standard errors at the village-level

# Difference in time trends by treatment
pt_pt_bDBP <- 
  avg_comparisons(pt_bDBP,
                  var = "year",
                  by = "tc",
                  hypothesis = c("b2 - b1 = 0", "b3 - b1 = 0"),
                  vcov = ~ ID_VILLAGE)

# Joint test for differential trends
pt_pjt_bDBP <- hypotheses(pt_pt_bDBP, joint = TRUE)

# Gather estimates for difference in pre-trends
pt_text <- "Joint F-test of equal trends by cohort:"

pt_bDBP_stats <- paste("F(", pt_pjt_bDBP$df1, ", ",
                       pt_pjt_bDBP$df2, ") = ",
                       sprintf('%.2f', pt_pjt_bDBP$statistic) , ", p = ",
                       sprintf('%.3f', pt_pjt_bDBP$p.value), sep="")

pt_bDBP_test <- paste(pt_text, pt_bDBP_stats, sep = "\n")

# Plot of trends and estimates
pt_bDBP_plot <- plot_predictions(pt_bDBP, condition = c("year", "tc")) + 
  scale_y_continuous(limits = c(50, 150)) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  labs(subtitle = pt_bDBP_test,
       y = expression("Brachial DBP (mmHg)"),
       x = "") +
  scale_color_manual(name = "Treatment\ncohort",
                     labels = c("Never", "2020", "2021"),
                     values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  scale_fill_manual(name = "Treatment\ncohort",
                    labels = c("Never", "2020", "2021"),
                    values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  theme_pt()

## 5 cDBP ----------------------------------------------------------------------
# estimate model 
pt_cDBP <- glm(dia_central ~ year * tc,
               data = d_ft20or21)

# Time trends by treatment status
avg_comparisons(pt_cDBP,                          # group-time regression model
                var = "year",                     # estimate contrasts from 2018-2019
                by = "tc",                        # look at contrasts by treatment group
                vcov = ~ ID_VILLAGE)              # cluster standard errors at the village-level

# Difference in time trends by treatment
pt_pt_cDBP <- 
  avg_comparisons(pt_cDBP,
                  var = "year",
                  by = "tc",
                  hypothesis = c("b2 - b1 = 0", "b3 - b1 = 0"),
                  vcov = ~ ID_VILLAGE)

# Joint test for differential trends
pt_pjt_cDBP <- hypotheses(pt_pt_cDBP, joint = TRUE)

# Gather estimates for difference in pre-trends
pt_text <- "Joint F-test of equal trends by cohort:"

pt_cDBP_stats <- paste("F(", pt_pjt_cDBP$df1, ", ",
                       pt_pjt_cDBP$df2, ") = ",
                       sprintf('%.2f', pt_pjt_cDBP$statistic) , ", p = ",
                       sprintf('%.3f', pt_pjt_cDBP$p.value), sep="")

pt_cDBP_test <- paste(pt_text, pt_cDBP_stats, sep = "\n")

# Plot of trends and estimates
pt_cDBP_plot <- plot_predictions(pt_cDBP, condition = c("year", "tc")) + 
  scale_y_continuous(limits = c(50, 150)) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  labs(subtitle = pt_cDBP_test,
       y = expression("Central DBP (mmHg)"),
       x = "") +
  scale_color_manual(name = "Treatment\ncohort",
                     labels = c("Never", "2020", "2021"),
                     values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  scale_fill_manual(name = "Treatment\ncohort",
                    labels = c("Never", "2020", "2021"),
                    values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  theme_pt()

# 6 add BP plots together ------------------------------------------------------
pt_SBP_plots <- 
  (pt_bSBP_plot + pt_bDBP_plot) / (pt_cSBP_plot + pt_cDBP_plot)

ggsave(here("HEI-report", "images", "BP-pretrends_FT20orFT21.png"),
       plot=pt_SBP_plots, width=13, height=9)

