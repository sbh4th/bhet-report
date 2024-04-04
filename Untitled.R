options(knitr.kable.NA = "\\")

tx <- tibble(
  p = factor(rep(c("New recruitment",
  "Households from S1", "Households from S2",
  "Total recruitment"),2), levels = c("New recruitment",
  "Households from S1", "Households from S2",
  "Total recruitment")),
  g = c(1,1,1,1,2,2,2,2),
  s1 = c(977,NA,NA,977, 0,NA,NA,0),
  s2 = c(196,866,NA, 1062, 300, 0,NA, 300),
  s4 = c(68, 780, 162, 1010, 52, 0, 248, 300)
)

tx2 <- tx %>% pivot_wider(names_from = g,
  values_from = c(s1, s2, s4)) %>%
  arrange(p,s1_1,s2_1,s4_1,s1_2,s2_2,s4_2)

kable(tx2, 
  col.names = c("Sample", "S1", "S2", "S4", "S1", "S2", "S4")) %>% #, "latex", booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, 
                     "Overall" = 3, "Indoor" = 3))

nr <- c(77,196,68,0,300,52)
s1 <- c(NA,866,780,NA,0,0)
s2 <- c(NA, NA,162,NA,NA,248)
tot <- c(977,1062,1010,0,300,300)

t1 <- data.frame(nr,s1,s2,tot)
t1 <- row.names(t1, c("New recruitment",
  "Households from S1", "Households from S2",
  "Total recruitment"))

