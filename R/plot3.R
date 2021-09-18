plot.linreg <- function(x, ...){
  library(ggplot2)
  # What data do we need?
  data = data.frame("fitted_values" = x$fitted_values, "residuals" = x$residuals, "sqr_abs_st_residuals" = sqrt(abs(x$residuals / sqrt(x$residual_variance))))
  # calculate positions of absolute largest 3 residuals
  largest_res <- order(abs(data$residuals), decreasing = TRUE)[1:3]
  # make the actual plot
  ggplot(data=data, mapping = aes(x = fitted_values, y = residuals)) +
    # How should the data points look like?
    geom_point(shape = 1, size = 6) +
    # Background should be white and black borders around it
    theme(panel.border = element_rect(linetype = "solid", fill = NA), panel.background = element_rect(fill = "white", colour = "white"),
          plot.title = element_text(hjust = 0.5)) +
    # Set title
    ggtitle("Residuals vs Fitted") +
    # Set axis labels including the function used for the regression in a new line
    labs(x=paste("Fitted values\n linreg(", paste(toString(x$call[2]), ")", sep=""), sep=""), y="Residuals") +
    # Add horizontal dotted line at y = 0.
    geom_hline(yintercept=0, linetype="dotted", color="grey") +
    # Add red line through the medians
    stat_summary(fun=median, colour="red", geom="line") +
    # label the 3 largest residuals
    geom_text(data = data[largest_res,], mapping = aes(x=fitted_values, y=residuals, label=largest_res), size=4, check_overlap = TRUE, nudge_x = -0.24)
}
