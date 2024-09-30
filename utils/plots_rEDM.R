
library(ggplot2)




ggplot_simplex_rho_E = function(res_simplex_tp1, res_simplex_tp1_summary_opti_E, 
                                this_id, this_var, labels_of_variables) {
  
  these_best_E = res_simplex_tp1_summary_opti_E %>% 
    filter(id_timeseries == this_id, variable == this_var) %>% 
    mutate(clean_name_method = method_E_opti_simplex %>% gsub("_percent", "%", .) %>% gsub("_", " ", .)) %>% 
    mutate(clean_name_method = paste0(toupper(substr(clean_name_method, 1, 1)), substr(clean_name_method, 2, nchar(clean_name_method))) )
  
  p = res_simplex_tp1 %>% 
    filter(id_timeseries == this_id, variable == this_var) %>% 
    
    ggplot(aes(x = E, y = rho)) +
    geom_line() + geom_point() +
    geom_vline(data = these_best_E, aes(xintercept = E_opti_simplex), linetype = "dashed", color = "#666666") +
    geom_text_repel(data = these_best_E, color = "#666666", segment.color = "#666666",
                    aes(x = E_opti_simplex, y = rho_E_opti_simplex, label = clean_name_method)) +
    labs(title = paste0("Simplex projection for ", labels_of_variables[this_var], " (id = ", this_id, ")"),
         x = "Embedding dimension (E)", y = "Forecastinf skill (rho)") + 
    theme_minimal()
  
  return(p)
  
}


ggplot_simplex_hist_E = function(res_simplex_tp1_summary_opti_E, this_var, method_select_E,
                                 labels_of_variables) {
  
  clean_name_method = method_select_E %>% gsub("_percent", "%", .) %>% gsub("_", " ", .)
  clean_name_method = paste0(toupper(substr(clean_name_method, 1, 1)), substr(clean_name_method, 2, nchar(clean_name_method)))
  
  p = res_simplex_tp1_summary_opti_E %>% 
    filter(variable == this_var, method_E_opti_simplex == method_select_E) %>% 
    
    ggplot(aes(x = E_opti_simplex)) +
    geom_histogram(binwidth = 1, fill = "wheat3", color = "#444444") +
    labs(title = paste0("Histogram of the optimal E for ", labels_of_variables[this_var]),
         x = "Optimal embedding dimension (E)", y = "Count", subtitle = paste0("Method selection E: ", clean_name_method)) +
    theme_minimal()
  
  return(p)
  
}



ggplot_convergence = function() {
  
}