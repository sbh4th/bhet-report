#  program:  pre-trend-plots.R
#  task:     estimate and test for pre-trends
#  input:    various .rds files
#  output:   various figures for pre-trends
#  project:  BHET
#  author:   sam harper \ 2024-08-22


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

## 1 read in clean air pollution datasets ----

# personal PM2.5
d_p <- read_rds(here("data-clean",
  "ap-data-personal.rds"))

# personal black carbon
d_bc <- read_rds(here("data-clean",
  "ap-data-bc.rds"))

## 2 pre-trends for personal AP outcomes ----

### Personal exposure

# Limit to pre-intervention years/cohort
d_p_r <- d_personal %>% 
  filter(year < 2021 & cohort_year_2019==0) %>%
  mutate(tc = factor(ban_status_composite, 
    labels = c("Never", "2020", "2021")))

# estimate model
pt_p <- glm(pe ~ year * tc,
  data = d_p_r, family = gaussian(link = "log"))

# Time trends by treatment status
avg_comparisons(pt_p, 
  var = "year",
  by = "tc",
  vcov = ~ v_id)

# Difference in time trends by treatment
pt_pt <- avg_comparisons(pt_p, 
  var = "year",
  by = "tc",
  hypothesis = c("b2 - b1 = 0", "b3 - b1 = 0"),
  vcov = ~ v_id)

pt_pjt <- hypotheses(pt_pt, joint = TRUE)

# Gather estimates for difference in pre-trends
pt_text <- "Joint F-test of equal trends by cohort:"

pt_p_stats <- paste("F(", pt_pjt$df1, ", ", 
    pt_pjt$df2, ") = ", 
    sprintf('%.2f', pt_pjt$statistic) , ", p = ",
    sprintf('%.3f', pt_pjt$p.value), sep="")

pt_p_test <- paste(pt_text, pt_p_stats, sep = "\n")

# Plot of trends and estimates
pt_pe_plot <- plot_predictions(pt_p, condition = c("year",
  "tc")) + scale_y_continuous(limits = c(0, 150)) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  labs(subtitle = pt_p_test,
  y = expression("Personal PM"["2.5"] ~ "(µg/" ~ m^3~")"), 
    x = "") +
  scale_color_manual(name = "Treatment\ncohort",
    labels = c("Never", "2020", "2021"),
    values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  scale_fill_manual(name = "Treatment\ncohort",
    labels = c("Never", "2020", "2021"),
    values = c("#e41a1c", "#377eb8", "#4daf4a")) + 
  theme_pt()

# Personal black carbon
d_bc_r <- d_bc %>% 
  filter(year < 2021 & cohort_year_2019==0) %>%
  mutate(tc = factor(ban_status_composite, 
    labels = c("Never", "2020", "2021")))

pt_bc <- glm(bc_exp_conc ~ year * tc,
            data = d_bc_r)

# Time trends by treatment status
avg_comparisons(pt_bc, 
  var = "year",
  by = "tc",
  vcov = ~ v_id)

# Difference in time trends by treatment
pt_bct <- avg_comparisons(pt_bc, 
  var = "year",
  by = "tc",
  hypothesis = c("b2 - b1 = 0", "b3 - b1 = 0"),
  vcov = ~ v_id)

pt_bcjt <- hypotheses(pt_bct, joint = TRUE)

# Gather estimates for difference in pre-trends
pt_text <- "Joint F-test of equal trends by cohort:"

# Gather estimates for difference in pre-trends
pt_bc_stats <- paste("F(", pt_bcjt$df1, ", ", 
    pt_bcjt$df2, ") = ", 
    sprintf('%.2f', pt_bcjt$statistic) , ", p = ",
    sprintf('%.3f', pt_bcjt$p.value), sep="")

pt_bc_test <- paste(pt_text, pt_bc_stats, sep = "\n")


# Plot of trends and estimates
pt_bc_plot <- plot_predictions(pt_bc, 
  condition = c("year", "tc")) + 
  scale_y_continuous(limits = c(-0.5, 5.2)) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  labs(subtitle = pt_bc_test,
       y = expression("Black carbon" ~ "(µg/" ~ m^3~")"), x = "") +
  scale_color_manual(name = "Treatment\ncohort",
    labels = c("Never", "2020", "2021"),
    values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  scale_fill_manual(name = "Treatment\ncohort",
    labels = c("Never", "2020", "2021"),
    values = c("#e41a1c", "#377eb8", "#4daf4a")) + 
  theme_pt()


# add personal plots together
pt_plots <- pt_pe_plot / pt_bc_plot

ggsave(here("images", "pe-bc-pretrends.png"), 
       plot=pt_plots, width=6.5, height=9)


