library(tidyverse)
library(ggplot2)



ggplot_timeseries_one_stock = function(data, variables, labels_of_variables,
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
    labs(title = "Time series", x = "Time", y = "Value", color = "Variable") +
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



