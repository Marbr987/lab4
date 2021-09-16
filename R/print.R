print.linreg <- function(QR,y) {
  y <- round(as.numeric(x$regression_coefficient))
  names(y) <- names(x$regression_coefficient)
  cat("Call:\n")
  print(x$call)
  cat("\n\nCoefficients:\n")
  print(y)
}
