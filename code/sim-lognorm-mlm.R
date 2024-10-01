gen_data <- function(n1, n2, B0, B1, sd_log, u0) {
  
  cluster <- rep(1:(2 * n2), each = n1)
  TX <- rep(c(0, 1), each = n1 * n2)
  u0 <- rnorm(2 * n2, sd = u0)[cluster]
  
  mulog <- (B0 + B1 * TX + u0)
  y <- rlnorm(2 * n1 * n2, meanlog = mulog, sdlog = sd_log)
  log_y <- log(y)
  
  d <- data.frame(cluster,
                  TX,
                  y, log_y)
  d
}

set.seed(4445)
pars <- list("n1" = 30, # observations per cluster
             "n2" = 10, # clusters per treatment
             "B0" = log(500),
             "B1" = log(0.5),
             "sd_log" = 0.5,
             "u0" = 0.5)
d <- do.call(gen_data,
             pars)

fit <- brm(y ~ 1 + TX + (1 | cluster), 
           family = lognormal(), 
           data = d,
           cores = 4, 
           seed = 4445)

f <- feglm(y ~ 1 + TX, data = d, 
  family = gaussian(link = "log"))



b_simple <-
  brm(data = d_personal,
      family = lognormal(),
      pe ~ 1 + (1 | v_id) +
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021,
      iter = 2000, warmup = 1000, chains = 4, cores = 4, 
      seed = 4445)

f_simple <- feglm(pe ~ 1 + 
        cohort_year_2019 + cohort_year_2020 +
        cohort_year_2021 + year_2019 + year_2021, 
        data = d_personal,
        family = gaussian(link = "log"),
          cluster = ~ v_id)

library(RStata)
options("RStata.StataVersion" = 16)
options("RStata.StataPath"= '/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')

s_est <- '
glm pe ///
        c.treat#c.cohort_year_2019#c.year_2019 /// 
        c.treat#c.cohort_year_2019#c.year_2021 ///
        c.treat#c.cohort_year_2020#c.year_2021 ///
        c.treat#c.cohort_year_2021#c.year_2021 ///
        cohort_year_2019  cohort_year_2020 ///
        cohort_year_2021  year_2019  year_2021, link(log) family(gaussian) vce(cl v_id) nolog cformat(%4.3f)

margins, at(treat=(0 1)) subpop(if treat==1) predict(xb) post vce(unconditional)
nlcom (exp(_b[2._at]) - exp(_b[1._at])), cformat(%4.3f)
'
stata(s_est, data.in=d_personal)

m1 <- glm(deaths ~ gender + age_group + year, offset=lnpop, data = df)
