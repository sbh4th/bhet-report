#  program:  pre-trends-resp.R
#  task:     estimate and test for pre-trends
#  input:    various .rds files
#  output:   various figures for pre-trends
#  project:  BHET
#  author:   sam harper \ 2024-08-22


## 0 Load needed packages and graph options ----
library(here)
library(tidyverse)
library(marginaleffects)
library(modelsummary)
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


## 1 Read in full dataset  ----
dresp <- read_rds(here("data-clean", 
  "bhet-resp-data.rds")) 

# create data frame for analysis
drpt <- dresp %>% 
  # limit to complete cases as for models
  drop_na(b_out, age_health, male, csmoke, fsmoke) %>%
  # limited to pre-intervention years and
  # post-intervention cohorts
  filter(year < 2021 & cohort_year_2019==0) %>%
  mutate(tc = factor(ban_status_composite, 
    labels = c("Never", "2020", "2021")))


## 2 Model function  ----

# Define the function to estimate the model, gather statistics, and create a plot
run_analysis <- function(outcome, data, 
  year_var = "year", treatment_var = "tc", 
  vcov_cluster = "v_id") {
  
  # Estimate the model for the given outcome
  formula <- as.formula(paste(outcome, "~", 
    year_var, "*", treatment_var))
  pt_model <- glm(formula, data = data)
  
  # Compute average comparisons for the time trends by treatment status
  pt_comparisons <- avg_comparisons(pt_model, 
    var = year_var, by = treatment_var, 
    vcov = as.formula(paste("~", vcov_cluster)))
  
  # Compute the difference in time trends by treatment
  pt_trend_diff <- avg_comparisons(pt_model, 
    var = year_var, by = treatment_var, 
    hypothesis = c("b2 - b1 = 0", "b3 - b1 = 0"), 
    vcov = as.formula(paste("~", vcov_cluster)))
  
  # Gather statistics for the joint test
  pt_trend_jt <- hypotheses(pt_trend_diff,
    joint = TRUE)
  
  # Gather statistics for the difference in pre-trends
  pt_text <- "Joint F-test of equal trends by cohort:"
  
  pt_stats <- paste("F(", pt_trend_jt$df1, ", ", 
    pt_trend_jt$df2, ") = ", 
    sprintf('%.2f', pt_trend_jt$statistic) , ", p = ",
    sprintf('%.3f', pt_trend_jt$p.value), sep="")
  
  pt_test <- paste(pt_text, pt_stats, sep = "\n")
  
  # Plot the trends and estimates
  pt_plot <- plot_predictions(pt_model, 
    condition = c(year_var, treatment_var)) + 
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(breaks = c(2018, 2019)) +
    labs(subtitle = pt_test, 
      y = paste("Probability of", outcome),, x = "") +
    scale_color_manual(name = "Treatment\ncohort",
      labels = c("Never", "2020", "2021"),
      values = c("#e41a1c", "#377eb8", "#4daf4a")) +
    scale_fill_manual(name = "Treatment\ncohort",
      labels = c("Never", "2020", "2021"),
      values = c("#e41a1c", "#377eb8", "#4daf4a")) + 
    theme_pt()

  return(list(model = pt_model, 
    comparisons = pt_comparisons, plot = pt_plot, 
    test_text = pt_test))
}

## 3 Apply function across outcomes  ----

# List of outcomes
b_out <- c("resp", "cough", "phlegm", "wheeze", 
           "breath", "nochest")

# Corresponding descriptive y-axis labels
y_labels <- c("Probability of any symptom", 
              "Probability of coughing", 
              "Probability of phlegm", 
              "Probability of wheezing", 
              "Probability of breathlessness", 
              "Probability of no chest symptoms")

# Apply the function across all outcomes and store results
results <- lapply(b_out, 
  function(outcome) run_analysis(outcome, data = drpt))

# Access plots, models, and comparison results
plots <- lapply(results, function(res) res$plot)
models <- lapply(results, function(res) res$model)
comparisons <- lapply(results, function(res) res$comparisons)
tests <- lapply(results, function(res) res$test_text)

combined_plot <- wrap_plots(plots) + 
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "right")

ggsave(here("images", "resp-pretrends.png"), 
       plot=combined_plot, width=8.5, height=11)
