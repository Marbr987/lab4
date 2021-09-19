linreg <-
  function(formula, data){
    call = match.call()
    X <- model.matrix(formula, data)
    y_var_name <- all.vars(formula)[1]
    y <- data[[y_var_name]]
    QR <- qr(X)
    Q <- qr.Q(QR)
    R <- qr.R(QR)
    coeff <- qr.coef(QR, y)
    residuals <- qr.resid(QR, y)
    y_pred <- X %*% coeff
    y_pred <- as.numeric(y_pred[,1])
    df <- nrow(X) - ncol(X)
    residual_variance <- t(residuals) %*% residuals / df
    residual_variance <- residual_variance[1,1]
    var_coeff <- residual_variance * solve(t(R) %*% R)
    t_values <- coeff / sqrt(diag(var_coeff))
    p_values <- sapply(2*(1 - pt(abs(t_values), df)), function(x) {if(x < 2e-16) {"<2e-16"} else round(x,5)})
    #standardised residual for plot2
    stand_res <<- sqrt(abs((residuals-mean(residuals)) / sqrt(residual_variance)))
    
    df1 <- data.frame(Fitted_values=y_pred,Residuals=residuals)
    p1 <- ggplot(df1,aes(Fitted_values,Residuals))+
      geom_point()+
      stat_summary(fun=median, colour="red", geom="line")+
      ggtitle("Residuals vs Fitted")+
      xlab("Fitted values")+
      ylab("Residuals")
    
    df2 <- data.frame(Fitted_values=y_pred,Residuals=stand_res)
    p2 <- ggplot(df2,aes(Fitted_values,Residuals))+
      geom_point()+
      stat_summary(fun=median, colour="red", geom="line")+
      ggtitle("Scale-Location")+
      xlab("Fitted values")+
      ylab(expression(bold(sqrt(bold("Standardized Residuals")))))
    
    res <- list(call = call,
                regression_coefficient = coeff,
                fitted_values = y_pred,
                residuals = residuals,
                degree_of_freedom = df,
                residuall_variance = residual_variance,
                variance_of_regression_coefficient = var_coeff,
                t_values = t_values,
                p_values = p_values,
                plot=list(p1,p2))
    class(res) <- "linreg"
    return(res)
  }

linreg(Petal.Length~Species,data = iris)
