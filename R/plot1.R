#' linreg
#' @description Implementing methods in class 
#' @param with ggplot2 packages
#' @param 
#' @return plot showing residuals vs fitted values
#' @export

plot.linreg=function(x,..)
{
  library(ggplot2)
  Plot1=data.frame(fitted_values,residuals)
  p1 <- ggplot(data_frame1,aes(fitted_values,residuals))+
    geom_point()+geom_abline()+
    title("Residuals vs Fitted")+
    xlab("Fitted values lm(Petal.Length ~ Species)")+
    ylab("Residuals")
  
  data_frame2 <- data.frame(fitted_values,residuals)
  p2 <- ggplot(data_frame2,aes(fitted_values,residuals))+
    geom_point()+geom_abline()+
    ggtitle("Scale-Location")+
    xlab("Fitted values lm(Petal.Length ~ Species)")+
    ylab(expression(bold(sqrt(bold("Standardized Residuals")))) )
  return(list(p1, p2))
}


