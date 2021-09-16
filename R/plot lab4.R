#' linreg
#' @description Implementing methods in class 
#' @param ggplot2
#' @param 
#' @return plot showing residuals vs fitted values
#' @export

xrange=range(x$fitted)
yrange=range(x$residuals)
plot(xrange,yrange,main="Residuals vs Fitted",sub=x$call,xlab = "Fitted values",
     ylab ="Residuals")