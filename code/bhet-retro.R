#  program:  bhet-retro.R
#  task:     estimate retrospective design paramters
#  input:    
#  output:   bhet-retro.rds
#  project:  BHET
#  author:   sam harper \ 2024-10-08


##  0 Load needed packages ----
library(here)
library(tidyverse)
library(retrodesign)
library(patchwork)

## 1 Load data ----
# bring in table of results for health outcomes
obs_table <- readRDS(here("outputs", 
  "op-table.rds")) |>
  # get SE of each estimate
  mutate(ll = as.numeric(sub(
      ".*,\\s*(-?\\d+\\.\\d+)\\)", "\\1", ci_2)),
    ul = as.numeric(sub(
      "\\((-?\\d+\\.\\d+),.*", "\\1", ci_2)), 
    se = abs(ll - ul) / (2 * 1.96),
    # binary indicator of 'significance'
    significant = if_else(
     abs(estimate_2) > 1.96 * se, 1, 0),
    # table of hypothetical effect sizes
    es = c(-2.5, -2.5, -2, -2, 0.5, 0.5, 
    0.1, 0.1, -5, -2, -3, -1, -3, -1, -0.5,
    -0.17, -0.42, -0.10, -0.17)) |>
  # select needed columns
  select(category, outcome, significant,
         estimate_2, se, es)

## 2 Define function ----
# estimate retrospective design parameters and 
# return a dataframe with all simulated estimates
retrodesigndf <- function(
    A, s, alpha = 0.05, df = Inf, n.sims = 10000) {
  z <- qt(1 - alpha/2, df)
  p.hi <- 1 - pt(z - A/s, df)
  p.lo <- pt(-z - A/s, df)
  power <- p.hi + p.lo
  typeS <- p.lo / power
  
  es <- A
  estimate <- A + s * rt(n.sims, df)
  significant <- if_else(abs(estimate) > s * z, 1, 0)
  exaggeration <- abs(estimate) / A
  
  # Return a dataframe with the simulation results
  results <- data.frame(
    hes = es,
    estimate = estimate,
    significant = significant,
    exaggeration = exaggeration,
    power = power
  )
  
  return(results)
}

## 3 Apply function to all estimates ----
# Run the function for all effect sizes

rd_results <- obs_table %>%
  select(category, outcome, es, se) %>%
  rowwise() %>%
  mutate(simulation_results = list(retrodesigndf(es, se))) %>%
  unnest(simulation_results) %>%
  ungroup() %>%
  mutate(sign = if_else((hes < 0 & estimate > 0) | 
    (hes > 0 & estimate < 0), 1, 0), 
    exagg = abs(estimate) / abs(hes)) %>% 
  group_by(category, outcome, significant) %>% 
  summarize(ms = mean(sign), 
    mex = mean(exagg), 
    mp = mean(power) * 100)

## 4 Summarize results ----
# combine retro power results with observed estimates
rd_table <- obs_table %>% 
  left_join(rd_results, 
    by = c("category", "outcome", "significant")) 

# save to outputs
saveRDS(rd_table, here("outputs", "bhet-retro.rds"))

## 5 Graph for BP ----
# create a plot for different effects
# Hypothetical effect sizes for BP (mmHg)
# setting SE to 1.0
possible_effects <- seq(0.1,4, by = .1)
rd_bp <- retro_design_closed_form(possible_effects, 1)

rd_bp_results <- data.frame(es = possible_effects, 
 p = rd_bp$power * 100, s = rd_bp$type_s,
 m = rd_bp$type_m)

# theme for graphs
theme_pt <- function() {
  theme_classic() + 
    theme(axis.title = element_text(size=14),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      plot.subtitle = element_text(size = 12))
}

p_plot <- ggplot(rd_bp_results, 
  aes(es, p)) + 
  geom_point(color = "#e41a1c") + 
  geom_line(color = "#e41a1c", alpha = 0.2) +
  labs(x = "Hypothetical effect size (mmHg)", 
       y = "Power (%)") +
  theme_pt()

s_plot <- ggplot(rd_bp_results, 
  aes(es, s)) + 
  geom_point(color = "#377eb8") + 
  geom_line(color = "#377eb8", alpha = 0.2) +
  labs(x = "Hypothetical effect size (mmHg)", 
       y = "P(Sign bias)") +
  theme_pt()

m_plot <- ggplot(rd_bp_results, 
  aes(es, m)) +
  geom_point(color = "#4daf4a") + 
  geom_line(color = "#4daf4a", alpha = 0.2) +
  labs(x = "Hypothetical effect size (mmHg)", 
       y = "Exaggeration ratio") +
  theme_pt()
  
# put all three plots together
rp_plot <- p_plot / (s_plot + m_plot)

# save to images folder
ggsave(here("images", "rd-plot.png"), 
  plot=rp_plot, width=8, height=8)
