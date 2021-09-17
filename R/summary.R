summary.linreg <- function(linreg_obj){
  coeff_df <- data.frame("Estimate" = linreg_obj$regression_coefficient, "Std. Error" = sqrt(diag(linreg_obj$variance_of_regression_coefficient)), "t value" = linreg_obj$t_values, "Pr(>|t|)" = linreg$p_values)
}s