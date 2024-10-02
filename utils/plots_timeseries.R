library(tidyverse)
library(ggplot2)



ggplot_timeseries_one_stock = function(data, variables, labels_of_variables, id = NA, 
                                       time = "year", scale = TRUE) {
  
  data = data[, c(time, variables)]
  colnames(data)[1] = "time"
  
  if (scale) {
    for (var in variables) {
      data[[var]] = scale(data[[var]])[, 1]
    }
  }
  
  p = data %>% 
    pivot_longer(-time, names_to = "variable", values_to = "value") %>%
    mutate(variable = recode(variable, !!!labels_of_variables)) %>%
    
    ggplot(aes(x = time, y = value, color = variable)) +
    geom_line() +
    labs(title = paste0("Time series", ifelse(is.na(id), "", paste0(" (id = ", id, ")"))),
         x = "Time", y = "Value", color = "Variable") +
    theme_light() + 
    theme(legend.position = "bottom")
    
  return(p)
  
}


ggplot_timeseries_durations = function(data, id_timeseries, time = "year") {
  
  data = data %>% 
    group_by_at(id_timeseries) %>%
    summarise(min_time = min(!!sym(time)), max_time = max(!!sym(time))) %>%
    arrange(min_time)
  colnames(data)[colnames(data) == id_timeseries] = "id_timeseries"
    
  p = ggplot(data, aes(x = min_time, xend = max_time, 
                   y = reorder(id_timeseries, min_time), yend = reorder(id_timeseries, min_time))) +
    geom_segment(linewidth = 0.8) + 
    labs(title = "Time series durations", x = "Start time", y = "End time") +
    theme_light()
    
  return(p)
  
}






# plot_phase_space = function(df, var_x = "x", var_y = "y", all_lags_plot = -4:4) {
#   
#   df_plot = data.frame()
#   
#   for (lag_plot in all_lags_plot) {
#     
#     if (lag_plot > 0) {
#       df_plot = df_plot %>% rbind(
#         df %>% mutate(!!var_y := lead(!!sym(var_y), lag_plot)) %>% 
#           cbind(lag = lag_plot)
#       )
#     } else if (lag_plot < 0) {
#       df_plot = df_plot %>% rbind(
#         df %>% mutate(!!var_y := lag(!!sym(var_y), -lag_plot)) %>% 
#           cbind(lag = lag_plot)
#       )
#     } else {
#       df_plot = df_plot %>% rbind(df %>% cbind(lag = lag_plot)) 
#     }
#   }
#   
#   df_plot %>% 
#     ggplot(aes(x = !!sym(var_x), y = !!sym(var_y))) +
#     facet_wrap(~lag, labeller = labeller(lag = function(x) {
#       paste0(var_y, "(t", 
#              ifelse(x == 0, "", paste0(ifelse(x > 0, "+", "-"), abs(as.numeric(x)) ) ),
#              ")")
#     }) ) +
#     geom_point(alpha = 0.5) +
#     geom_smooth(method = "lm", color = "tomato") +
#     theme_light()
#   
# }




