lin_reg <- function(formula=NA, dat=NA, x_var=NA, y_var=NA){
  model <- lm(formula, data = dat)
  p <- dat %>%
    ggplot() +
    geom_point(aes(x = dat %>% pull(x_var), y = dat %>% pull(y_var))) +
    geom_abline(intercept = model$coefficients[1], slope=model$coefficients[2]) +
    #labs(x = input$independent, y = input$dependent) + 
    labs(x = x_var, y = y_var) + 
    NULL
  return(p)
}
