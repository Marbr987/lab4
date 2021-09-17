#' linreg
#' @description Implementing methods in class 
#' @param with ggplot2 packages
#' @param 
#' @return plot showing residuals vs fitted values
#' @export

plot.linreg=function(x,..)
{
  library(ggplot2)
  Plot1=data.frame(x$fitted_values,x$residuals) # what do you do with Plot1? It is not used anymore... Maybe it should be data_frame1? Also I added the x$ notation
  p1 <- ggplot(data_frame1,aes(x$fitted_values,x$residuals))+
    geom_point()+geom_abline()+
    title("Residuals vs Fitted")+
    xlab("Fitted values lm(Petal.Length ~ Species)")+
    ylab("Residuals")
  
  # data_frame1 is equal to data_frame2 right? But why is that?
  data_frame2 <- data.frame(x$fitted_values,x$residuals)
  p2 <- ggplot(data_frame2,aes(x$fitted_values,x$residuals))+
    geom_point()+geom_abline()+
    ggtitle("Scale-Location")+
    xlab("Fitted values lm(Petal.Length ~ Species)")+
    ylab(expression(bold(sqrt(bold("Standardized Residuals")))) )
  return(list(p1, p2))
}


