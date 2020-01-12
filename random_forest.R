random_forest <- function(formula=NA, dat=NA, x_var=NA, y_var=NA){
  model <- randomForest(formula(formula), data=dat)
  x_values <- dat %>% pull(x_var)
  x_range <- tibble(x_var = seq(min(x_values), max(x_values), 0.0001))
  colnames(x_range) <- x_var
  new_pred <- predict(model, x_range)
  predicted <- bind_cols(x_range, tibble(prediction=new_pred))
  p <- dat %>%
    ggplot() +
    geom_point(aes(x=dat %>% pull(x_var), y= dat %>% pull(y_var))) +
    geom_line(aes(x=predicted %>% pull(x_var), y=predicted %>% pull(prediction)), data= predicted) +
    labs(x=x_var, y= y_var) +
    NULL
  return(p)
}