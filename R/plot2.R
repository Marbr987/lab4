#' linreg
#' @description Implementing methods in class 
#' @param with ggplot2 packages
#' @param 
#' @return plot showing residuals vs fitted values
#' @export
#' 
#ggplot(iris,aes(x=fitted_values(lm(Petal.Length ~ Species)),y=residuals))+
#geom_point()+

plot.linreg=function(x,..){
  data(iris)
  fit1 <- lm(Petal.Length~Species, data = iris)
  summary(fit1)
  library(ggplot2)
  require(ggplot2)
  ggplot(x$fitted_values,x$residuals ,aes(x=names(x$fitted_values),y=names(x$residuals)))+
    geom_point()+
    stat_smooth(method="lm",col="Black")+
    labs(title=paste("Residuals vs Fitted"))
}
fit1=lm(Petal.Length~Species, data = iris)
plot.linreg(fit1)


## ggplot(data=iris,aes(x=x$fitted_values(lm(Petal.Length ~ Species)),y=x$residuals))                          


