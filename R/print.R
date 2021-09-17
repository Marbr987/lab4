#' print S3 method
#' @description S3 method for objects of class linreg printing the call and the coefficients of the linear model
#' @param linreg_obj An object of class linreg
#' @return nothing. Prints tha call and the coefficients of the linreg object.
#' @export
print.linreg <- function(object) {
  cat("Call:\n")
  print(object$call)
  cat("\nCoefficients:\n")
  print(object$regression_coefficient)
}
