#  program:  pre-trend-plots.R
#  task:     estimate and test for pre-trends
#  input:    various .rds files
#  output:   various figures
#  project:  BHET
#  author:   sam harper \ 2024-08-15


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
d_p_r <- d_p %>% filter(year < 2021) %>%
  mutate(et = case_when(cohort_year_2021==1 ~ "Yes",
                        cohort_year_2021==0 ~ "No"))
# estimate model
pt_p <- glm(PM25conc_exposureugm3 ~ year * et,
            data = d_p_r)

# Time trends by treatment status
avg_comparisons(pt_p, 
  var = "year",
  by = "cohort_year_2021",
  vcov = ~ v_id)

# Difference in time trends by treatment
pt_pt <- avg_comparisons(pt_p, 
  var = "year",
  by = "cohort_year_2021",
  hypothesis = "b2 - b1 = 0",
  vcov = ~ v_id)

# Gather estimates for difference in pre-trends
pt_text <- 
  "Difference in trend (SE) for treated vs. untreated villages:" 

pt_p_stats <- paste(sprintf("%.1f", pt_pt$estimate),
  " (", sprintf("%.1f", pt_pt$std.error), ")", 
  ", 95% CI: ", sprintf("%.1f", pt_pt$conf.low),
  ", ", sprintf("%.1f", pt_pt$conf.high), sep="")

pt_p_test <- paste(pt_text, pt_p_stats, sep = "\n")

# Plot of trends and estimates
pt_pe_plot <- plot_predictions(pt_p, condition = c("year",
  "et")) + scale_y_continuous(limits = c(0, 150)) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  labs(subtitle = pt_p_test,
  y = expression("Personal PM"["2.5"] ~ "(µg/" ~ m^3~")"), 
    x = "") +
  scale_color_manual(name = "Treated in 2021?",
    labels = c("No", "Yes"),
    values = c("#e41a1c", "#377eb8")) +
  scale_fill_manual(name = "Treated in 2021?",
    labels = c("No", "Yes"),
    values = c("#e41a1c", "#377eb8")) + theme_pt()

# Personal black carbon
d_bc_r <- d_bc %>% filter(year < 2021) %>%
  mutate(et = case_when(cohort_year_2021==1 ~ "Yes",
                        cohort_year_2021==0 ~ "No"))
pt_bc <- glm(bc_exp_conc ~ year * et,
            data = d_bc_r)

# Time trends by treatment status
avg_comparisons(pt_bc, 
  var = "year",
  by = "et",
  vcov = ~ v_id)

# Difference in time trends by treatment
pt_bct <- avg_comparisons(pt_bc, 
  var = "year",
  by = "cohort_year_2021",
  hypothesis = "b2 - b1 = 0",
  vcov = ~ v_id)

# Gather estimates for difference in pre-trends
pt_bc_stats <- paste(sprintf("%.1f", pt_bct$estimate),
  " (", sprintf("%.1f", pt_bct$std.error), ")", 
  ", 95% CI: ", sprintf("%.1f", pt_bct$conf.low),
  ", ", sprintf("%.1f", pt_bct$conf.high), sep="")

pt_bc_test <- paste(pt_text, pt_bc_stats, sep = "\n")


# Plot of trends and estimates
pt_bc_plot <- plot_predictions(pt_bc, 
  condition = c("year", "et")) + 
  scale_y_continuous(limits = c(-0.5, 5)) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  labs(subtitle = pt_bc_test,
       y = expression("Black carbon" ~ "(µg/" ~ m^3~")"), x = "") +
  scale_color_manual(name = "Treated in 2021?",
                     labels = c("No", "Yes"),
                     values = c("#e41a1c", "#377eb8")) +
  scale_fill_manual(name = "Treated in 2021?",
                    labels = c("No", "Yes"),
                    values = c("#e41a1c", "#377eb8")) + 
  theme_pt()


# add personal plots together
pt_plots <- pt_pe_plot / pt_bc_plot

ggsave(here("images", "pe-bc-pretrends.png"), 
       plot=pt_plots, width=6.5, height=9)


