# Combined function to process columns
process_column <- function(ds, column_name) {
  column_type <- typeof(ds[[column_name]])
  
  if (column_type == "integer") {
    cprops <- ds %>% filter(wave != 3) %>%
      group_by(catvar) %>%
      summarize(
        n = n(), 
        prop = mean(.data[[column_name]], na.rm=TRUE),
        np = round(n * prop, 0),
        pr = sprintf('%.1f', prop * 100)) %>% 
      select(catvar, np, pr) %>% 
      pivot_wider(names_from = catvar, 
                  values_from = c(np, pr), 
                  names_vary = "slowest") %>%
      mutate(char = column_name,
             w1 = paste0(np_1, " (", pr_1, ")"),
             w2 = paste0(np_2, " (", pr_2, ")"),
             w3 = paste0(np_3, " (", pr_3, ")")) %>%
      select(char, w1, w2, w3)
    
    cprop_stats <- ds %>%
      mutate(!!sym(column_name) := as.factor(.data[[column_name]])) %>%
      infer::chisq_test(as.formula(paste("catvar ~", column_name))) %>%
      select(-chisq_df) %>%
      mutate(across(c('statistic', 'p_value'), 
                    ~ sprintf('%.3f', .x)))
    
    ctable <- cprops %>% bind_cols(cprop_stats)
    
  } else if (column_type == "double") {
    cmeans <- ds %>% filter(wave != 3) %>%
      group_by(catvar) %>%
      summarize(
        vmean = sprintf('%.1f', mean(.data[[column_name]], 
                                     na.rm=TRUE)),
        vsd = sprintf('%.1f', sd(.data[[column_name]], 
                                 na.rm=TRUE))) %>%
      pivot_wider(names_from = catvar, 
                  values_from = c(vmean, vsd), 
                  names_vary = "slowest") %>%
      mutate(char = column_name,
             w1 = paste0(vmean_1, " (", vsd_1, ")"),
             w2 = paste0(vmean_2, " (", vsd_2, ")"),
             w3 = paste0(vmean_3, " (", vsd_3, ")")) %>%
      select(char, w1, w2, w3)
    
    formula <- as.formula(paste(column_name, "~ catvar"))
    
    F_hat <- ds %>% 
      infer::observe(formula, stat = "F")
    
    null_dist_theory <- ds %>%
      infer::specify(formula) %>%
      hypothesize(null = "independence") %>%
      assume(distribution = "F") %>%
      get_p_value(obs_stat = F_hat, direction = "two-sided")
    
    cmeans_stats <- F_hat %>% 
      bind_cols(null_dist_theory) %>%
      rename("statistic" = stat) %>%
      mutate(across(c('statistic', 'p_value'), 
                    ~ sprintf('%.3f', .x)))
    
    ctable <- cmeans %>% bind_cols(cmeans_stats)
  }
  
  return(ctable)
}

# Combine results for all columns
process_columns <- function(ds, columns) {
  result <- bind_rows(lapply(columns, function(col) 
    process_column(ds, col)))
  return(result)
}
