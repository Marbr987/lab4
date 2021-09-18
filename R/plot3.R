plot.linreg <- function(x, ...){
  library(ggplot2)
  # What data do we need?
  data = data.frame("fitted_values" = x$fitted_values, "residuals" = x$residuals, "sqr_abs_st_residuals" = sqrt(abs(x$residuals / sqrt(x$residual_variance))))
  ggplot(data=data) +
    # Which points do we want to plot and how should the data points look like?
    geom_point(mapping = aes(x = fitted_values, y = residuals), shape = 1, size = 6) +
    # Background should be white and black borders around it
    theme(panel.border = element_rect(linetype = "solid", fill = NA), panel.background = element_rect(fill = "white", colour = "white"),
          plot.title = element_text(hjust = 0.5)) +
    # Set title
    ggtitle("Residuals vs Fitted") +
    # Set axis labels
    labs(x=paste("Fitted values\n", str(x$call), sep=""), y="Residuals") +
    # Add horizontal dotted line at y = 0.
    geom_hline(yintercept=0, linetype="dotted", color="grey")
}
