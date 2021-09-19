#' summary S3 method
#' @description S3 method for objects of class linreg providing a summary of the object
#' @param linreg_obj An object of class linreg
#' @return nothing. Prints a summary of the linreg object.
#' @name summary.linreg
#' @export
summary.linreg <- function(linreg_obj){
  add_stars <- function(x){
    if(x != "<2e-16"){x <- as.numeric(x)}
    if(x < 0.001 | x == "<2e-16"){'***'}
    else if (x < 0.01) {'**'}
    else if (x < 0.05) {'*'}
    else if (x < 0.1) {'.'}
    else {'not defined'}
  }
  coeff_df <- data.frame("Estimate" = linreg_obj$regression_coefficient, "Std. Error" = sqrt(diag(linreg_obj$variance_of_regression_coefficient)), "t value" = linreg_obj$t_values, "p value" = linreg_obj$p_values)
  coeff_df[,5] <- sapply(coeff_df[,4], add_stars)
  cat('Coefficients:\n')
  print(coeff_df)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\nResidual standard error:", sqrt(linreg_obj$residual_variance), 'on', linreg_obj$degree_of_freedom, 'degrees of freedom')
}
