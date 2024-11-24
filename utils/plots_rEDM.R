
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
    theme_light()
  
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
    theme_light()
  
  return(p)
  
}



# CCM ---------------------------------------------------------------------

ggplot_convergence_ccm_one_dir = function(res_ccm_best_E_summaries, this_id, this_causal_rel, this_tp, labels_of_variables, 
                                          metric = "rho", values_origin = c("5", "50", "95"), values_null = c("5", "50", "95")) {
  
  values_origin = paste0(metric, "_", values_origin)
  values_null = paste0(metric, "_", values_null)
  
  this_summary_origin = res_ccm_best_E_summaries %>% 
    filter(id_timeseries == this_id, var_lib == this_causal_rel[2], var_target == this_causal_rel[1], tp == this_tp, 
           dataset == "origin")
  this_summary_null = res_ccm_best_E_summaries %>%
    filter(id_timeseries == this_id, var_lib == this_causal_rel[2], var_target == this_causal_rel[1], tp == this_tp, 
           dataset == "null")
  
  y_range = c(min(0, min(this_summary_origin[[values_origin[1]]]), min(this_summary_null[[values_null[1]]])),
              max(1, max(this_summary_origin[[values_origin[3]]]), max(this_summary_null[[values_null[3]]])))
  
  subtitle = paste0("Grey: null model (", gsub(".*_", "", values_origin[2]), "% quantile and ",
                    gsub(".*_", "", values_origin[1]), "%-", gsub(".*_", "", values_origin[3]), "% quantiles)  ;  ",
                    "Red: original data (", gsub(".*_", "", values_null[2]), "% quantile and ",
                    gsub(".*_", "", values_null[1]), "%-", gsub(".*_", "", values_null[3]), "% quantiles)")
  
  if (metric == "rho") {
    y_title = expression(paste("Forecasting skill (", rho, ")"))
  } else if (metric == "rmse") {
    y_title = "Root mean square error (RMSE)"
  } else if (metric == "mae") {
    y_title = "Mean absolute error (MAE)"
  } else {
    y_title = metric
  }
  
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
         x = "Library size", y = y_title) +
    ylim(y_range) +
    theme_light() +
    theme(plot.subtitle = element_text(size = 6.5))
  
  return(p)
  
}


ggplot_convergence_ccm_two_dir = function(res_ccm_best_E_summaries, this_id, these_vars, this_tp, labels_of_variables, 
                                          metric = "rho", values_origin = c("5", "50", "95")) {
  
  values_origin = paste0(metric, "_", values_origin)
  
  these_summary_origin = res_ccm_best_E_summaries %>% 
    filter(id_timeseries == this_id, tp == this_tp, dataset == "origin") %>% 
    filter( (var_lib == these_vars[1] & var_target == these_vars[2]) | 
            (var_lib == these_vars[2] & var_target == these_vars[1]) ) %>% 
    mutate(label_causal_rel = paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]))
  
  y_range = c(min(0, min(these_summary_origin[[values_origin[1]]])),
              max(1, max(these_summary_origin[[values_origin[3]]])))
  
  if (metric == "rho") {
    y_title = expression(paste("Forecasting skill (", rho, ")"))
  } else if (metric == "rmse") {
    y_title = "Root mean square error (RMSE)"
  } else if (metric == "mae") {
    y_title = "Mean absolute error (MAE)"
  } else {
    y_title = metric
  }
  
  p = ggplot(these_summary_origin, aes(x = lib_size, color = label_causal_rel, fill = label_causal_rel)) +
    geom_line(aes(y = !!sym(values_origin[2])), linewidth = 1) +
    geom_line(aes(y = !!sym(values_origin[1])), linewidth = 1, linetype = "dashed") +
    geom_line(aes(y = !!sym(values_origin[3])), linewidth = 1, linetype = "dashed") +
    # geom_ribbon(aes(ymin = !!sym(values_origin[1]), ymax = !!sym(values_origin[3])), alpha = 0.3) +
    labs(title = paste0("Predictive skill vs library size (id = ", this_id, " ; tp = ", this_tp, ")"),
         color = "Causal relationship", fill = "Causal relationship",
         subtitle = paste0("Curve ", gsub(".*_", "", values_origin[2]), "% quantile and ribbon ", 
                           gsub(".*_", "", values_origin[1]), "%-", 
                           gsub(".*_", "", values_origin[3]), "% quantiles"), 
         x = "Library size", y = y_title) +
    ylim(y_range) +
    theme_light() +
    theme(plot.subtitle = element_text(size = 6.5), legend.position = "bottom")
  
  return(p)
  
}

ggplot_ccm_hist_causality_one_tp = function(res_ccm_best_E_assessment, this_tp, labels_of_variables, 
                                            list_of_causality_tested = list_of_causality_tested, 
                                            df_ids_kept_from_E = NULL) {
  
  if (is.null(df_ids_kept_from_E)) {
    df_ids_kept_from_E = res_ccm_best_E_assessment %>% 
      dplyr::select(var_lib, id_timeseries) %>% rename(variable = var_lib)
  }
  
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], "\n->\n", 
                                       labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  # Filter out the ids that were removed from the E assessment (id per var_lib)
  res_ccm_best_E_assessment$kept = TRUE
  for (this_var_lib in unique(res_ccm_best_E_assessment$var_lib)) {
    these_ids_kept = df_ids_kept_from_E %>% filter(variable == this_var_lib) %>% 
      pull(id_timeseries) %>% as.character()
    # cat(paste0("Variable lib: ", this_var_lib, " ; ids removed: ", 
    #            setdiff(unique(res_ccm_best_E_assessment$id_timeseries), these_ids_kept), "\n"))
    res_ccm_best_E_assessment = res_ccm_best_E_assessment %>% 
      mutate(kept = ifelse((var_lib == this_var_lib) & !(id_timeseries %in% these_ids_kept), 
                           FALSE, kept))
  }
  
  p = res_ccm_best_E_assessment %>% 
    filter(kept, tp == this_tp) %>%
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], "\n->\n", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel)) %>%
    mutate(causality = factor(ifelse(causality, "Yes", "No"), levels = c("Yes", "No"))) %>%
    
    ggplot(aes(x = label_causal_rel, fill = causality)) +
    geom_bar(stat = "count", position = position_stack(reverse = TRUE), color = "#444444") +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5, reverse = TRUE), color = "#444444", size = 3) +
    scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371")) +
    labs(title = paste0("Causality assessment (tp = ", this_tp, ")"),
         x = "Causal relationship", y = "Count", fill = "Causality?") +
    theme_light() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  return(p)
  
}

ggplot_ccm_hist_causality_multiple_tp = function(res_ccm_best_E_assessment, these_tp, labels_of_variables, 
                                                 list_of_causality_tested = list_of_causality_tested,
                                                 df_ids_kept_from_E = NULL) {
  
  if (is.null(df_ids_kept_from_E)) {
    df_ids_kept_from_E = res_ccm_best_E_assessment %>% 
      dplyr::select(var_lib, id_timeseries) %>% rename(variable = var_lib)
  }
  
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], " -> ", 
                                       labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  # Filter out the ids that were removed from the E assessment (id per var_lib)
  res_ccm_best_E_assessment$kept = TRUE
  for (this_var_lib in unique(res_ccm_best_E_assessment$var_lib)) {
    these_ids_kept = df_ids_kept_from_E %>% filter(variable == this_var_lib) %>% 
      pull(id_timeseries) %>% as.character()
    res_ccm_best_E_assessment = res_ccm_best_E_assessment %>% 
      mutate(kept = ifelse((var_lib == this_var_lib) & !(id_timeseries %in% these_ids_kept), 
                           FALSE, kept))
  }
  
  p = res_ccm_best_E_assessment %>% 
    filter(kept, tp %in% these_tp) %>%
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel)) %>%
    mutate(causality = factor(ifelse(causality, "Yes", "No"), levels = c("Yes", "No"))) %>%
    
    ggplot(aes(x = tp, fill = causality)) +
    facet_wrap(~label_causal_rel, scales = "free_y") +
    geom_bar(stat = "count", position = position_stack(reverse = TRUE), color = "#444444") +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5, reverse = TRUE), color = "#444444", size = 3) +
    scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371")) +
    labs(title = "Causality assessment",
         x = "Time horizon of prediction in the CCM (tp)", y = "Count", fill = "Causality?") +
    theme_light()
  
  return(p)
  
}

ggplot_ccm_alluvial_causality_multiple_tp = function(res_ccm_best_E_assessment, these_tp, labels_of_variables, 
                                                     list_of_causality_tested = list_of_causality_tested,
                                                     df_ids_kept_from_E = NULL) {
  
  if (is.null(df_ids_kept_from_E)) {
    df_ids_kept_from_E = res_ccm_best_E_assessment %>% 
      dplyr::select(var_lib, id_timeseries) %>% rename(variable = var_lib)
  }
  
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], " -> ", 
                                       labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  # Filter out the ids that were removed from the E assessment (id per var_lib)
  res_ccm_best_E_assessment$kept = TRUE
  for (this_var_lib in unique(res_ccm_best_E_assessment$var_lib)) {
    these_ids_kept = df_ids_kept_from_E %>% filter(variable == this_var_lib) %>% 
      pull(id_timeseries) %>% as.character()
    res_ccm_best_E_assessment = res_ccm_best_E_assessment %>% 
      mutate(kept = ifelse((var_lib == this_var_lib) & !(id_timeseries %in% these_ids_kept), 
                           FALSE, kept))
  }
  
  alluvial_data  = res_ccm_best_E_assessment %>%
    filter(kept, tp %in% these_tp) %>%
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel)) %>%
    mutate(causality = factor(ifelse(causality, "Yes", "No"), levels = c("Yes", "No"))) %>%
    dplyr::select(label_causal_rel, id_timeseries, tp, causality) %>% 
    pivot_wider(names_from = tp, values_from = causality, names_prefix = "tp_")  # Pivot so each tp is a column
  
  axes_mapping <- setNames(lapply(these_tp, function(tp) sym(paste0("tp_", tp))), paste0("axis", seq_along(these_tp)))

  # Create alluvial plot with multiple axes (one per tp)
  p = ggplot(alluvial_data, aes(y = 1, !!!axes_mapping)) + 
    geom_alluvium(aes(fill = tp_0), width = 1/12) +  # Adjust 'fill' as per your choice
    geom_stratum(width = 1/4, color = "#444444", linewidth = 0.2) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2, color = "black") +
    scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371")) +
    scale_x_discrete(limit = paste0("axis", seq_along(these_tp)), labels = these_tp) +
    labs(title = "Causality assessment across time horizons",
         x = "Time horizons (tp)", y = "Count", fill = "Causality at tp=0?") +
    theme_light() +
    facet_wrap(~label_causal_rel, scales = "free_y", ncol = 1)
  
  return(p)
  
}


ggplot_ccm_hist_causality_tp_max_rho = function(res_ccm_best_E_assessment, labels_of_variables, 
                                                list_of_causality_tested = list_of_causality_tested,
                                                df_ids_kept_from_E = NULL) {
  
  if (is.null(df_ids_kept_from_E)) {
    df_ids_kept_from_E = res_ccm_best_E_assessment %>% 
      dplyr::select(var_lib, id_timeseries) %>% rename(variable = var_lib)
  }
  
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], "\n->\n", 
                                       labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  # Filter out the ids that were removed from the E assessment (id per var_lib)
  res_ccm_best_E_assessment$kept = TRUE
  for (this_var_lib in unique(res_ccm_best_E_assessment$var_lib)) {
    these_ids_kept = df_ids_kept_from_E %>% filter(variable == this_var_lib) %>% 
      pull(id_timeseries) %>% as.character()
    res_ccm_best_E_assessment = res_ccm_best_E_assessment %>% 
      mutate(kept = ifelse((var_lib == this_var_lib) & !(id_timeseries %in% these_ids_kept), 
                           FALSE, kept))
  }
  
  p = res_ccm_best_E_assessment %>% 
    filter(kept) %>%
    group_by(var_lib, var_target, id_timeseries) %>% 
    filter(max_median_rho_ccm_with_libs == max(max_median_rho_ccm_with_libs)) %>%
    ungroup() %>% 
    mutate(causality = case_when(
      tp > 0 ~ "Inconsistent\ncausality\n(tp > 0)",
      causality ~ "Yes",
      TRUE ~ "No"
    )) %>%
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], "\n->\n", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel)) %>%
    mutate(causality = factor(causality, levels = c("Yes", "No", "Inconsistent\ncausality\n(tp > 0)"))) %>%
    
    ggplot(aes(x = label_causal_rel, fill = causality)) +
    geom_bar(stat = "count", position = position_stack(reverse = TRUE), color = "#444444") +
    geom_text(stat = "count", aes(label = after_stat(count)), position = position_stack(vjust = 0.5, reverse = TRUE), 
              color = "#444444", size = 2) +
    scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371", "Inconsistent\ncausality\n(tp > 0)" = "#f1a055")) +
    labs(title = paste0("Causality assessment (found for an optimal tp <= 0)"),
         x = "Causal relationship", y = "Count", fill = "Causality?") +
    theme_light() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  return(p)
  
}

ggplot_ccm_alluvial_causality_one_tp_and_tp_max_rho = function(res_ccm_best_E_assessment, this_tp, labels_of_variables, 
                                                               list_of_causality_tested = list_of_causality_tested,
                                                               df_ids_kept_from_E = NULL) {
  
  if (is.null(df_ids_kept_from_E)) {
    df_ids_kept_from_E = res_ccm_best_E_assessment %>% 
      dplyr::select(var_lib, id_timeseries) %>% rename(variable = var_lib)
  }
  
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], " -> ", 
                                       labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  # Filter out the ids that were removed from the E assessment (id per var_lib)
  res_ccm_best_E_assessment$kept = TRUE
  for (this_var_lib in unique(res_ccm_best_E_assessment$var_lib)) {
    these_ids_kept = df_ids_kept_from_E %>% filter(variable == this_var_lib) %>% 
      pull(id_timeseries) %>% as.character()
    res_ccm_best_E_assessment = res_ccm_best_E_assessment %>% 
        mutate(kept = ifelse((var_lib == this_var_lib) & !(id_timeseries %in% these_ids_kept), 
                             FALSE, kept))
  }
  
  this_df_tp_max_rho = res_ccm_best_E_assessment %>% 
    filter(kept) %>%
    group_by(var_lib, var_target, id_timeseries) %>% 
    filter(max_median_rho_ccm_with_libs == max(max_median_rho_ccm_with_libs)) %>%
    ungroup() %>% 
    mutate(causality_max_rho = case_when(
      tp > 0 ~ "Inconsistent\ncausality\n(tp > 0)",
      causality ~ "Yes",
      TRUE ~ "No"
    )) %>%
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel))
  
  this_df_one_tp = res_ccm_best_E_assessment %>% 
    filter(kept, tp == this_tp) %>%
    mutate(causality_one_tp = case_when(
      causality ~ "Yes",
      TRUE ~ "No"
    )) %>% 
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel))
  
  alluvial_data = left_join(this_df_one_tp %>% dplyr::select(label_causal_rel, id_timeseries, causality_one_tp), 
                            this_df_tp_max_rho %>% dplyr::select(label_causal_rel, id_timeseries, causality_max_rho),
                            by = c("label_causal_rel", "id_timeseries")) %>%
    mutate(causality_one_tp = factor(causality_one_tp, levels = c("Inconsistent\ncausality\n(tp > 0)", "No", "Yes")),
           causality_max_rho = factor(causality_max_rho, levels = c("Inconsistent\ncausality\n(tp > 0)", "No", "Yes"))) %>%
    group_by(label_causal_rel, causality_one_tp, causality_max_rho) %>%
    summarise(n = n()) %>%
    ungroup()
  
  p = ggplot(alluvial_data, aes(axis1 = causality_one_tp, axis2 = causality_max_rho, y = n)) +
    facet_wrap(~label_causal_rel, scales = "free_y") +
    geom_alluvium(aes(fill = causality_max_rho), width = 1/12) +
    geom_stratum(width = 1/4, color = "#444444", linewidth = 0.2) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 1.4, color = "black") +
    scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371", "Inconsistent\ncausality\n(tp > 0)" = "#f0ad4e")) +
    scale_x_discrete(limits = c("Causality\nat tp = 0", "Causality\nat tp max rho\nand <= 0"), expand = c(0.1, 0.1)) +
    labs(title = paste0("Causality assessment at tp = ", this_tp, " and for an optimal tp <= 0"),
         y = "Count", fill = "Causality at tp = 0") +
    theme_light() + 
    theme(axis.title.x = element_blank(), strip.text = element_text(size = 6))
  
  return(p)
  
}


ggplot_ccm_alluvial_causality_one_tp_both_dir = function(res_ccm_best_E_assessment, this_tp, labels_of_variables, 
                                                         list_of_causality_tested = list_of_causality_tested,
                                                         df_ids_kept_from_E = NULL) {
  
  if (is.null(df_ids_kept_from_E)) {
    df_ids_kept_from_E = res_ccm_best_E_assessment %>% 
      dplyr::select(var_lib, id_timeseries) %>% rename(variable = var_lib)
  }
  
  # Keep only the pairs of causal relationships that are tested in both directions
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    if (!any(sapply(list_of_causality_tested, function(x) all(x == rev(list_of_causality_tested[[i]])) )) ) {
      next  # Skip this iteration if the reverse pair is not found
    }
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], " -> ", 
                                       labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  # Filter out the ids that were removed from the E assessment (id per var_lib)
  res_ccm_best_E_assessment$kept = TRUE
  for (this_var_lib in unique(res_ccm_best_E_assessment$var_lib)) {
    these_ids_kept = df_ids_kept_from_E %>% filter(variable == this_var_lib) %>% 
      pull(id_timeseries) %>% as.character()
    res_ccm_best_E_assessment = res_ccm_best_E_assessment %>% 
        mutate(kept = ifelse((var_lib == this_var_lib) & !(id_timeseries %in% these_ids_kept), 
                             FALSE, kept))
  }
    
  alluvial_data = res_ccm_best_E_assessment %>%
    filter(kept, tp == this_tp) %>%
    mutate(var1 = pmin(var_lib, var_target), var2 = pmax(var_lib, var_target), 
           direction = ifelse(var_lib < var_target, "causality_from_2_to_1", "causality_from_1_to_2")) %>%
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel)) %>%
    filter(label_causal_rel %in% order_labels_causal_rel) %>% 
    mutate(causality = factor(ifelse(causality, "Yes", "No"), levels = c("Yes", "No"))) %>%
    
    dplyr::select(var1, var2, id_timeseries, direction, causality) %>% 
    pivot_wider(names_from = direction, values_from = c(causality)) %>% 
    
    group_by(var1, var2, causality_from_1_to_2, causality_from_2_to_1) %>%
    summarise(n = n()) %>%
    ungroup()
  
  p = ggplot(alluvial_data, aes(axis1 = causality_from_1_to_2, axis2 = causality_from_2_to_1, y = n)) +
    facet_wrap(~ var1 + var2,
               labeller = labeller(var1 = function(var1) paste0("Var 1 = ", labels_of_variables[var1]),
                                   var2 = function(var2) paste0("Var 2 = ", labels_of_variables[var2]) ) ) +
    geom_alluvium(aes(fill = causality_from_1_to_2), width = 1/12) +
    geom_stratum(width = 1/4, color = "#444444", linewidth = 0.2) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2, color = "black") +
    scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371")) +
    scale_x_discrete(limits = c("Causality\nfrom 1 to 2", "Causality\nfrom 2 to 1"), expand = c(0.1, 0.1)) +
    labs(title = paste0("Causality assessment at tp = ", this_tp),
         y = "Count", fill = "Causality") +
    theme_light() + 
    theme(axis.title.x = element_blank())
  
  return(p)
  
}


ggplot_ccm_alluvial_causality_tp_max_rho_both_dir = function(res_ccm_best_E_assessment, labels_of_variables, 
                                                             list_of_causality_tested = list_of_causality_tested,
                                                             df_ids_kept_from_E = NULL) {
  
  if (is.null(df_ids_kept_from_E)) {
    df_ids_kept_from_E = res_ccm_best_E_assessment %>% 
      dplyr::select(var_lib, id_timeseries) %>% rename(variable = var_lib)
  }
  
  # Keep only the pairs of causal relationships that are tested in both directions
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    if (!any(sapply(list_of_causality_tested, function(x) all(x == rev(list_of_causality_tested[[i]])) )) ) {
      next  # Skip this iteration if the reverse pair is not found
    }
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], " -> ", 
                                       labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  # Filter out the ids that were removed from the E assessment (id per var_lib)
  res_ccm_best_E_assessment$kept = TRUE
  for (this_var_lib in unique(res_ccm_best_E_assessment$var_lib)) {
    these_ids_kept = df_ids_kept_from_E %>% filter(variable == this_var_lib) %>% 
      pull(id_timeseries) %>% as.character()
    res_ccm_best_E_assessment = res_ccm_best_E_assessment %>% 
        mutate(kept = ifelse((var_lib == this_var_lib) & !(id_timeseries %in% these_ids_kept), 
                             FALSE, kept))
  }
  
  alluvial_data = res_ccm_best_E_assessment %>% 
    filter(kept) %>%
    group_by(var_lib, var_target, id_timeseries) %>% 
    filter(max_median_rho_ccm_with_libs == max(max_median_rho_ccm_with_libs)) %>%
    ungroup() %>% 
    mutate(var1 = pmin(var_lib, var_target), var2 = pmax(var_lib, var_target), 
           direction = ifelse(var_lib < var_target, "causality_from_2_to_1", "causality_from_1_to_2")) %>%
    mutate(label_causal_rel = factor(paste0(labels_of_variables[var_target], " -> ", labels_of_variables[var_lib]),
                                     levels = order_labels_causal_rel)) %>%
    filter(label_causal_rel %in% order_labels_causal_rel) %>% 
    mutate(causality = case_when(
      tp > 0 ~ "Inconsistent\ncausality\n(tp > 0)",
      causality ~ "Yes",
      TRUE ~ "No"
    )) %>% 
    mutate(causality = factor(causality, levels = c("Inconsistent\ncausality\n(tp > 0)", "No", "Yes")) ) %>%
    
    dplyr::select(var1, var2, id_timeseries, direction, causality) %>% 
    pivot_wider(names_from = direction, values_from = c(causality)) %>% 
    
    group_by(var1, var2, causality_from_1_to_2, causality_from_2_to_1) %>%
    summarise(n = n()) %>%
    ungroup()

  p = ggplot(alluvial_data, aes(axis1 = causality_from_1_to_2, axis2 = causality_from_2_to_1, y = n)) +
    facet_wrap(~ var1 + var2,
               labeller = labeller(var1 = function(var1) paste0("Var 1 = ", labels_of_variables[var1]),
                                   var2 = function(var2) paste0("Var 2 = ", labels_of_variables[var2]) ) ) +
    geom_alluvium(aes(fill = causality_from_1_to_2), width = 1/12) +
    geom_stratum(width = 1/4, color = "#444444", linewidth = 0.2) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 1.7, color = "black") +
    scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371", "Inconsistent\ncausality\n(tp > 0)" = "#f0ad4e")) +
    scale_x_discrete(limits = c("Causality\nfrom 1 to 2", "Causality\nfrom 2 to 1"), expand = c(0.1, 0.1)) +
    labs(title = "Causality assessment (found for an optimal tp <= 0)",
         y = "Count", fill = "Causality") +
    theme_light() + 
    theme(axis.title.x = element_blank())
  
  return(p)
  
}



# Strength ----------------------------------------------------------------

ggplot_strength_one_stock = function(res_strength_causality_details_smap, res_strength_causality, 
                                     this_id, this_causal_rel, this_tp) {
  
  this_row = res_strength_causality %>% 
    filter(id_timeseries == this_id, tp == this_tp,
           var_cause == this_causal_rel[1], var_consequence == this_causal_rel[2])
  annot = paste0(
    "Mean strength = ", sprintf("%4.2e", this_row$this_mean_strength), "    |    ",
    "P-value t-test = ", sprintf("%4.2e", this_row$this_t_test_p_val), "\n",
    "Linear slope = ", sprintf("%4.2e", this_row$this_trend_slope), "    |    ",
    "P-value slope = ", sprintf("%4.2e", this_row$this_trend_p_val)
  )
  
  this_df = res_strength_causality_details_smap %>% 
    filter(id_timeseries == this_id, tp == this_tp,
           var_cause == this_causal_rel[1], var_consequence == this_causal_rel[2],
           var_cause_jacobian == paste0(
             this_causal_rel[1], "_t", 
             ifelse(this_tp == 0, "", ifelse(this_tp > 0, paste0("_plus_", this_tp), paste0("_minus_", abs(this_tp))) ) 
           ) )
  
  color_mean = ifelse(this_row$this_t_test_p_val >= 0.05, "#444444", 
                      ifelse(this_row$this_mean_strength > 0, "royalblue", "#ef8004"))
  color_slope = ifelse(this_row$this_trend_p_val >= 0.05, "#444444", 
                       ifelse(this_row$this_trend_slope > 0, "royalblue", "#ef8004"))
  
  p = ggplot(this_df) + 
    geom_hline(yintercept = mean(this_df$value, na.rm=T), linetype = "dashed", color = color_mean) +
    annotate("text", x = min(this_df$t), y = mean(this_df$value, na.rm=T), hjust = 0, vjust = -0.5, label = "Mean", color = color_mean) +
    geom_line(aes(t, value)) + geom_point(aes(t, value)) + 
    geom_smooth(aes(t, value), method = "lm", color = color_slope) +
    # annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, label = annot) +
    labs(title = paste0("S-map coefficients (id = ", this_id, ")"),
         subtitle = paste0(labels_of_variables[this_causal_rel[1]], 
                           "(t", ifelse(this_tp == 0, "", ifelse(this_tp > 0, paste0("+", this_tp), paste0("-", abs(this_tp))) ), ")",
                           " -> ", labels_of_variables[this_causal_rel[2]], "(t+1)"),
         x = "Time", y = "S-map coefficients", 
         caption = annot) +
    theme_light()
  
  return(p)
  
}


ggplot_strength_summary = function(res_strength_causality, res_ccm_best_E_assessment, 
                                   labels_of_variables, this_tp, list_of_causality_tested,
                                   df_ids_kept_from_E = NULL) {
  
  if (is.null(df_ids_kept_from_E)) {
    df_ids_kept_from_E = res_ccm_best_E_assessment %>% 
      dplyr::select(var_lib, id_timeseries) %>% rename(variable = var_lib)
  }
  
  order_labels_causal_rel = c()
  for (i in 1:length(list_of_causality_tested)) {
    order_labels_causal_rel = c(order_labels_causal_rel, 
                                paste0(labels_of_variables[list_of_causality_tested[[i]][1]], " -> ", labels_of_variables[list_of_causality_tested[[i]][2]]))
  }
  
  # Filter out the ids that were removed from the E assessment (id per var_consequence)
  res_strength_causality$kept = TRUE
  for (this_var_consequence in unique(res_strength_causality$var_consequence)) {
    these_ids_kept = df_ids_kept_from_E %>% filter(variable == this_var_consequence) %>% 
      pull(id_timeseries) %>% as.character()
    res_strength_causality = res_strength_causality %>% 
      mutate(kept = ifelse((var_consequence == this_var_consequence) & !(id_timeseries %in% these_ids_kept), 
                           FALSE, kept))
  }
  
  p = res_strength_causality %>% 
    filter(kept, tp == this_tp) %>% 
    mutate(sign = ifelse(this_t_test_p_val >= 0.05, "Not significant", 
                         ifelse(this_mean_strength > 0, "Positive", "Negative")),
           trend = ifelse(this_trend_p_val >= 0.05, "Not significant", 
                          ifelse(this_trend_slope > 0, "Positive", "Negative"))) %>%
    mutate(sign = factor(sign, levels = c("Negative", "Not significant", "Positive")),
           trend = factor(trend, levels = c("Negative", "Not significant", "Positive"))) %>% 
    
    left_join(res_ccm_best_E_assessment, 
              by = c("id_timeseries" = "id_timeseries", "tp" = "tp",
                     "var_cause" = "var_target", "var_consequence" = "var_lib")) %>%
    filter(causality) %>% 
    
    group_by(var_cause, var_consequence, sign, trend) %>%
    summarise(n = n()) %>% 
    ungroup() %>%
    add_column(label_causal_rel = paste0(labels_of_variables[.[["var_cause"]]], " -> ", labels_of_variables[.[["var_consequence"]]])) %>%
    filter(label_causal_rel %in% order_labels_causal_rel) %>%
    mutate(label_causal_rel = factor(label_causal_rel, levels = order_labels_causal_rel)) %>%
    dplyr::select(label_causal_rel, sign, trend, n) %>% 
    
    ggplot(aes(x = trend, y = sign, fill = n)) +
    facet_wrap(~label_causal_rel, scales = "free") +
    geom_tile() +
    geom_text(aes(label = n), vjust = 0.5) +
    scale_fill_gradient(low = "#cccccc", high = "royalblue") +
    labs(title = "S-map coefficients", fill = "Number of\nstocks",
         x = "Significance of the trend", y = "Significance of the\nmean S-map coefficients") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          strip.text = element_text(size = 8))
  
  return(p)
  
}




