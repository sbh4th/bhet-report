library(tidyverse)
library(faux)
library(modelsummary)
library(fixest)

dataf <- add_random(cluster = 20, 
  person = 30) %>%
  add_between("cluster", cond = c("control", "treated"),
    .prob = 0.5, .shuffle = TRUE) %>%
  add_recode("cond", "treated", control = 0, treated = 1) 

set.seed(4913)
# define parameters
cluster_n = 20  # number of clusters
subj_n = 30     # number of subjects
b0 = log(500)   # intercept
b1 = log(0.5)   # fixed effect of treatment
sd_log = 0.5    # log SD 
u0c = 0.5        # SD of log intercepts (random effect) 
  
# set up data structure
data <- add_random(cluster = cluster_n, 
  subj = subj_n) %>%
  
  # add and recode categorical variables
  add_between("cluster", cond = c("control", "treated"),
    .prob = 0.5, .shuffle = TRUE) %>%
  add_recode("cond", "treated", 
             control = 0, treated = 1) %>%
  
  # add random effects 
  add_ranef("cluster", u0 = u0c) %>%
  
  # calculate outcome
  mutate(
    mulog = (b0 + b1 * treated + u0),
    y = rlnorm(subj_n * cluster_n, 
    meanlog = mulog, sdlog = sd_log))

datasummary_skim(data)


# set up simulation function
sim <- function(
  # data structure
  village_n = 50, subj_n = 19, wave_n = 3,
  # fixed effects
  b0 = 0, b1 = 0, b2 = 0, b3 = 0,
  # random effects
  u0s_sd = 1, u0v_sd = 1, sigma_sd = 1,
  ... # helps the function work with pmap() below
                ) {

# set up data structure
data <- add_random(village = village_n, 
  subj = subj_n, wave = wave_n) %>%
  
  # add and recode categorical variables
  add_between("village", cond = c("control", "treated"),
    .prob = c(25, 25), .shuffle = TRUE) %>%
  add_recode("cond", "treated", control = 0, treated = 1) %>%
  add_between("wave", time = c("pre", "post1", "post2")) %>%
  mutate(post = ifelse(time == "pre", 0, 1)) %>%
  
  # add random effects 
  add_ranef("subj", u0s = u0s_sd) %>%
  add_ranef("village", u0v = u0v_sd) %>%
  add_ranef(sigma = sigma_sd) %>%
  
  # calculate outcome
  mutate(y = b0               # intercept
    + (b1 * treated)          # fixed effect of treated
    + (b2 * post)             # fixed effect of time
    + (b3 * treated * post)   # fixed effect of treated x time
    + u0s + u0v + sigma)      # compound error term

# estimate model
m <- fixest::feols(
  y ~ treated * post | village, data = data,
  notes = FALSE) # drop warnings about collinearity

tidy(m)
}

# check the function parameters for one simulation
# keeping basic data structure constant
# (50 villages, 19 subjects, 3 waves)
sim(b0 = 120, b1 = -2, b2 = -5, b3 = -3, 
  u0s_sd = 12, u0v_sd = 1, sigma_sd = 12)

# estimate over many simulated datasets
x <- crossing(
  rep = 1:100, # number of replicates
  # subj_n = 19, # range of subject N
  # village_n = 50, # fixed item N
  # wave_n = 3, # fixed wave N
  b0 = 120, # fixed intercept
  b1 = -2, # fixed treatment effect
  b2 = -5, # fixed time effect
  b3 = c(-1, -2.5, -5), # range of treatment effects
  u0s_sd = 12, # random intercept SD for subjects
  u0v_sd = 1, # random intercept SD for villages
  sigma_sd = 12, # error SD
) %>%
  mutate(analysis = pmap(., sim)) %>%
  unnest(analysis) %>%
  filter(term == "treated:post")


x %>% group_by(b3) %>%
  summarise(power = mean(p.value < .05), 
            .groups = "drop")