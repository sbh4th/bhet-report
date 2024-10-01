predict double rawresid, response
generate lnrawresid2 = ln(rawresid^2)
predict double xbetahat, xb

*** Modified Park test
regress lnrawresid2 xbetahat, robust


quietly {
glm exp_tot age female, link(log) family(gamma)
 predict double rawyhat, mu
 predict double xbeta1, xb
generate double rawvar = (pe - rawyhat)^2
generate double xbeta2 = xbeta1^2
}

glm rawvar xbeta1, link(log) family(gamma) nolog vce(cl v_id)
 test xbeta1 - 0 = 0 /* NLLS or Gaussian family */
 test xbeta1 - 1 = 0 /* Poisson family */
 test xbeta1 - 2 = 0 /* Gamma family */
 test xbeta1 - 3 = 0 /* Inverse Gaussian family */
** check fit for family test using Pregibon's Link Test
glm rawvar xbeta1 xbeta2, link(log) family(gamma) nolog vce(cl v_id)




* log(y)
reg logpe c.treat#c.cohort_year_2019#c.year_2019 c.treat#c.cohort_year_2019#c.year_2021 c.treat#c.cohort_year_2020#c.year_2021 c.treat#c.cohort_year_2021#c.year_2021 cohort_year_2019 cohort_year_2020 cohort_year_2021 year_2019 year_2021 if pe>1, vce(cl v_id)

margins, dydx(treat) subpop(if treat==1) expression(exp(predict(xb)))

/* Duan's corrected expression (assumes iid errors) */
predict uhat, residual
gen expuhat = exp(uhat)
sum expuhat
gen yhat_duan = r(mean)*exp(lnyhat)

/* Note how the mean yhat is ~6% too low */
sum price yhat*

// not quite right, since it treats E[exp(uhat)] as a constant rather than a random
margins, dydx(treat) subpop(if treat==1) expression(exp(predict(xb)))
qui sum expuhat
margins, dydx(treat) subpop(if treat==1) ///
  expression(exp(predict(xb))*`=r(mean)')
  
