#' linreg
#' @description A function that performs a linear regression based on QR decomposition
#' @param formula A formula stating the form of the regression
#' @param data A dataframe to use for the regression
#' @return An object of class linreg containing regression coefficients, fitted values, residuals, degree-of-freedom, residual variance, variance of the regression coefficient, t-values and p-values
#' @source http://staff.www.ltu.se/~jove/courses/c0002m/least_squares.pdf
#' @export
linreg <-
function(formula, data){
  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]
  QR <- qr(X)
  Q <- qr.X(QR)
  R <- qr.R(QR)
  coeff <- qr.coef(QR, y)
  residuals <- qr.resid(QR, y)
  y_pred <- X %*% coeff
  df <- nrow(X) - ncol(X)
  residual_variance <- t(residuals) %*% residuals / df
  residual_variance <- residual_variance[1,1]
  var_coeff <- residual_variance * solve(t(R) %*% R)
  t_values <- coeff / sqrt(diag(var_coeff))
  p_values <- pt(t_values, df)
  res <- list(regression_coefficient = coeff,
              fitted_values = y_pred,
              residuals = residuals,
              degree_of_freedom = df,
              residuall_variance = residual_variance,
              variance_of_regression_coefficient = var_coeff,
              t_values = t_values,
              p_values = p_values)
  class(res) <- "linreg"
  return(res)
}
