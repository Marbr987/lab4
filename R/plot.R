#' linreg
#' @description Implementing methods in class 
#' @param with ggplot2 packages
#' @param 
#' @return plot showing residuals vs fitted values
#' @export

plot.linreg=function(x,y)
{
  xvalue=x$fitted
  yvalue=y$residuals
  plot(xvalue,yvalue,main="Residuals vs Fitted",xlab="Fitted values",ylab="Residuals")
  stdresidual = sqrt(x$residuals)
  yvalue1 =value(stdresidual)
  plot(xvalue,yvalue1,main="Scale Location",xlab="Fitted Values",ylab=sqrt("Standardized residuals"))
}
##ggplot(ggplot2::aes(x=fitted_values, y=residuals))
plot(lm(formula = Petal.Length ~ Species, data = iris))
mod_object <- lm(Petal.Length~Species, data = iris)
print(mod_object)
