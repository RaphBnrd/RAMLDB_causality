
library(ggplot2)
library(ggalluvial)



# Simplex -----------------------------------------------------------------

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



# CCM ---------------------------------------------------------------------

ggplot_convergence_ccm_one_dir = function(res_ccm_best_E_summaries, this_id, this_causal_rel, this_tp, labels_of_variables, 
                                          values_origin = c("5%", "50%", "95%"), values_null = c("5%", "50%", "95%")) {
  
  this_summary_origin = res_ccm_best_E_summaries %>% 
    filter(id_timeseries == this_id, var_lib == this_causal_rel[2], var_target == this_causal_rel[1], tp == this_tp, 
           dataset == "origin")
  this_summary_null = res_ccm_best_E_summaries %>%
    filter(id_timeseries == this_id, var_lib == this_causal_rel[2], var_target == this_causal_rel[1], tp == this_tp, 
           dataset == "null")
  
  y_range = c(min(0, min(this_summary_origin[[values_origin[1]]]), min(this_summary_null[[values_null[1]]])),
              max(1, max(this_summary_origin[[values_origin[3]]]), max(this_summary_null[[values_null[3]]])))
  
  subtitle = paste0("Grey: null model (", values_origin[2], " quantile and ",
                    values_origin[1], "-", values_origin[3], " quantiles)  ;  ",
                    "Red: original data (", values_null[2], " quantile and ",
                    values_null[1], "-", values_null[3], " quantiles)")
  
  p = ggplot(data = this_summary_origin, aes(x = lib_size)) +
    geom_line(aes(y = !!sym(values_origin[2])), color = "firebrick2", linewidth = 1) +
    geom_line(aes(y = !!sym(values_origin[1])), color = "firebrick2", linewidth = 1, linetype = "dashed") +
    geom_line(aes(y = !!sym(values_origin[3])), color = "firebrick2", linewidth = 1, linetype = "dashed") +
    geom_line(data = this_summary_null, aes(x = lib_size, y = !!sym(values_null[2])), color = "grey30", linewidth = 1) +
    geom_ribbon(data = this_summary_null, aes(x = lib_size, ymin = !!sym(values_null[1]), ymax = !!sym(values_null[3])), 
                fill = "grey30", alpha = 0.3) +
    labs(title = paste0("Predictive skill vs library size (id = ", this_id, ")\n", 
                        "Causal relationship: ", labels_of_variables[this_causal_rel[2]], " -> ", labels_of_variables[this_causal_rel[1]],
                        " (tp = ", this_tp, ")"),
         subtitle = subtitle, 
         x = "Library size", y = expression(paste("Forecasting skill (", rho, ")"))) +
    ylim(y_range) +
    theme_light() +
    theme(plot.subtitle = element_text(size = 6.5))
  
  return(p)
  
}


ggplot_convergence_ccm_two_dir = function(res_ccm_best_E_summaries, this_id, these_vars, this_tp, labels_of_variables, 
                                          values_origin = c("5%", "50%", "95%")) {
  
  these_summary_origin = res_ccm_best_E_summaries %>% 
    filter(id_timeseries == this_id, tp == this_tp, dataset == "origin") %>% 
    filter( (var_lib == these_vars[1] & var_target == these_vars[2]) | 
            (var_lib == these_vars[2] & var_target == these_vars[1]) ) %>% 
    mutate(label_causal_rel = paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]))
  
  y_range = c(min(0, min(these_summary_origin[[values_origin[1]]])),
              max(1, max(these_summary_origin[[values_origin[3]]])))
  
  p = ggplot(these_summary_origin, aes(x = lib_size, color = label_causal_rel, fill = label_causal_rel)) +
    geom_line(aes(y = !!sym(values_origin[2])), linewidth = 1) +
    geom_line(aes(y = !!sym(values_origin[1])), linewidth = 1, linetype = "dashed") +
    geom_line(aes(y = !!sym(values_origin[3])), linewidth = 1, linetype = "dashed") +
    # geom_ribbon(aes(ymin = !!sym(values_origin[1]), ymax = !!sym(values_origin[3])), alpha = 0.3) +
    labs(title = paste0("Predictive skill vs library size (id = ", this_id, " ; tp = ", this_tp, ")"),
         color = "Causal relationship", fill = "Causal relationship",
         subtitle = paste0("Curve ", values_origin[2], " quantile and ribbon ", values_origin[1], "-", values_origin[3], " quantiles"), 
         x = "Library size", y = expression(paste("Forecasting skill (", rho, ")"))) +
    ylim(y_range) +
    theme_light() +
    theme(plot.subtitle = element_text(size = 6.5), legend.position = "bottom")
  
  return(p)
  
}

ggplot_ccm_hist_causality_one_tp = function(res_ccm_best_E_assessment, this_tp, labels_of_variables, 
                                            id_removed = c(), list_of_causality_tested = list_of_causality_tested) {
  
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], " -> ", labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  p = res_ccm_best_E_assessment %>% 
    filter(tp == this_tp) %>% 
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel)) %>%
    mutate(causality = case_when(
      id_timeseries %in% id_removed ~ "NA",
      causality ~ "Yes",
      TRUE ~ "No"
    )) %>%
    mutate(causality = factor(causality, levels = c("Yes", "No", "NA"))) %>%
    
    ggplot(aes(x = label_causal_rel, fill = causality)) +
    geom_bar(stat = "count", position = position_stack(reverse = TRUE), color = "#444444") +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5, reverse = TRUE), color = "#444444", size = 3) +
    scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371", "NA" = "#aaaaaa")) +
    labs(title = paste0("Causality assessment (tp = ", this_tp, ")"),
         x = "Causal relationship", y = "Count", fill = "Causality?") +
    theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  return(p)
  
}

ggplot_ccm_hist_causality_multiple_tp = function(res_ccm_best_E_assessment, these_tp, labels_of_variables, 
                                                 id_removed = c(), list_of_causality_tested = list_of_causality_tested) {
  
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], " -> ", labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  p = res_ccm_best_E_assessment %>% 
    filter(tp %in% these_tp) %>% 
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel)) %>%
    mutate(causality = case_when(
      id_timeseries %in% id_removed ~ "NA",
      causality ~ "Yes",
      TRUE ~ "No"
    )) %>%
    mutate(causality = factor(causality, levels = c("Yes", "No", "NA"))) %>%
    
    ggplot(aes(x = tp, fill = causality)) +
    facet_wrap(~label_causal_rel, scales = "free_y") +
    geom_bar(stat = "count", position = position_stack(reverse = TRUE), color = "#444444") +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5, reverse = TRUE), color = "#444444", size = 3) +
    scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371", "NA" = "#aaaaaa")) +
    labs(title = "Causality assessment",
         x = "Time horizon of prediction in the CCM (tp)", y = "Count", fill = "Causality?") +
    theme_light()
  
  return(p)
  
}

ggplot_ccm_alluvial_causality_multiple_tp = function(res_ccm_best_E_assessment, these_tp, labels_of_variables, 
                                                     id_removed = c(), list_of_causality_tested = list_of_causality_tested) {
  
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], " -> ", labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  alluvial_data  = res_ccm_best_E_assessment %>%
    filter(tp %in% these_tp) %>%
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel)) %>%
    mutate(causality = case_when(
      id_timeseries %in% id_removed ~ "NA",
      causality ~ "Yes",
      TRUE ~ "No"
    )) %>%
    mutate(causality = factor(causality, levels = c("NA", "No", "Yes"))) %>%
    dplyr::select(label_causal_rel, id_timeseries, tp, causality) %>% 
    pivot_wider(names_from = tp, values_from = causality, names_prefix = "tp_")  # Pivot so each tp is a column
  
  axes_mapping <- setNames(lapply(these_tp, function(tp) sym(paste0("tp_", tp))), paste0("axis", seq_along(these_tp)))

  # Create alluvial plot with multiple axes (one per tp)
  p = ggplot(alluvial_data, aes(y = 1, !!!axes_mapping)) + 
    geom_alluvium(aes(fill = tp_0), width = 1/12) +  # Adjust 'fill' as per your choice
    geom_stratum(width = 1/4, color = "#444444", linewidth = 0.2) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2, color = "black") +
    scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371", "NA" = "#aaaaaa")) +
    scale_x_discrete(limit = paste0("axis", seq_along(these_tp)), labels = these_tp) +
    labs(title = "Causality assessment across time horizons",
         x = "Time horizons (tp)", y = "Count", fill = "Causality at tp=0?") +
    theme_light() +
    facet_wrap(~label_causal_rel, scales = "free_y", ncol = 1)
  
  return(p)
  
}





