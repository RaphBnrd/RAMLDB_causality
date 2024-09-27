
library(tidyverse)

# Consider we have the output of a simplex projection. 
# In particular, we have the forecasting skill rho for each embedding dimension E tested. 
# E is considered as optimal when the forecasting skill rho is maximal, while being resonably small.


get_optimal_E_simplex = function(df_out_simplex, 
                                 methods_select_E = list("max_rho" = TRUE,
                                                        "positive_range_top_x_percent" = c(0.2)) ) {
  
  # # Example of all the methods available
  # methods_select_E = list( # list of methods to select the optimal E, with their parameters
  #   # Select the E with the maximal forecasting skill rho
  #   "max_rho" = TRUE, # TRUE if you want to use this method, FALSE otherwise
  #   # Select the smallest E with the forecasting skill rho among the top x% in ranking
  #   "rank_top_x_percent" = c(0.1), # x% of the top forecasting skills, empty if not used
  #   # Select the smallest E with the forecasting skill rho in the x% highest range
  #   "range_top_x_percent" = c(0.1, 0.2), # x% of the highest forecasting skills, empty if not used
  #   # Select the smallest E with the forecasting skill rho in the x% highest range and positive
  #   "range_top_x_percent_and_positive" = c(0.2), # x% of the highest forecasting skills and positive, empty if not used
  #   # Select the smallest E with the forecasting skill rho in the x% highest range among the forecasting skills saturated to 0 if there are negative forecasting skills
  #   "positive_range_top_x_percent" = c(0.2) # x% of the highest positive forecasting skills, empty if not used
  # )
  
  out = data.frame()
  
  for (method_E in names(methods_select_E)) {
    
    if (method_E == "max_rho") {
      
      # Method 1: E with the maximal forecasting skill rho
      
      if (methods_select_E[[method_E]]) {
        out = out %>% rbind(
          data.frame(E_opti_simplex = df_out_simplex$E[which.max(df_out_simplex$rho)],
                     rho_E_opti_simplex = max(df_out_simplex$rho, na.rm = TRUE),
                     method_E_opti_simplex = "max_rho")
        )
      }
      
    } else if (method_E == "rank_top_x_percent") {
      
      # Method 2: Smallest E among the E that have a forecasting skill ranked in the top x%
      
      for (x in methods_select_E[[method_E]]) {
        rank_top_x = ceiling(x * nrow(df_out_simplex)) # round above if the rank is not an integer
        idx_best_rho = which(rank(-df_out_simplex$rho, ties.method= "min") <= rank_top_x) # get the rho of the top x% of the forecasting skills
        E_opti_simplex = min(df_out_simplex$E[idx_best_rho]) # the smallest E among these among the best rho
        
        out = out %>% rbind(
          data.frame(E_opti_simplex = E_opti_simplex,
                     rho_E_opti_simplex = df_out_simplex$rho[df_out_simplex$E == E_opti_simplex],
                     method_E_opti_simplex = paste0("rank_top_", x*100, "_percent"))
        )
      }
      
    } else if (method_E == "range_top_x_percent") {
      
      # Method 3: Smallest E among the E that have a forecasting skill value in the x% highest range
      
      for (x in methods_select_E[[method_E]]) {
        min_bound_range_top_x = max(df_out_simplex$rho, na.rm = TRUE) - 
          x * ( max(df_out_simplex$rho, na.rm = TRUE) - min(df_out_simplex$rho, na.rm = TRUE) )
        idx_best_rho = which(df_out_simplex$rho >= min_bound_range_top_x) # get the rho of the top x% of the forecasting skills
        E_opti_simplex = min(df_out_simplex$E[idx_best_rho]) # the smallest E among these among the best rho
        
        out = out %>% rbind(
          data.frame(E_opti_simplex = E_opti_simplex,
                     rho_E_opti_simplex = df_out_simplex$rho[df_out_simplex$E == E_opti_simplex],
                     method_E_opti_simplex = paste0("range_top_", x*100, "_percent"))
        )
      }
      
    } else if (method_E == "range_top_x_percent_and_positive") {
      
      # Method 4: Smallest E among the E that have a forecasting skill value in the x% highest range and positive
      
      for (x in methods_select_E[[method_E]]) {
        min_bound_range_top_x = max(df_out_simplex$rho, na.rm = TRUE) - 
          x * ( max(df_out_simplex$rho, na.rm = TRUE) - min(df_out_simplex$rho, na.rm = TRUE) )
        idx_best_rho = which(df_out_simplex$rho >= min_bound_range_top_x & 
                               df_out_simplex$rho > 0) # get the rho of the top x% of the forecasting skills that are positive
        E_opti_simplex = ifelse(length(idx_best_rho) == 0, NA,
                                min(df_out_simplex$E[idx_best_rho])) # the smallest E among these among the best rho
        rho_E_opti_simplex = ifelse(length(idx_best_rho) == 0, NA,
                                    df_out_simplex$rho[df_out_simplex$E == E_opti_simplex])
        
        out = out %>% rbind(
          data.frame(E_opti_simplex = E_opti_simplex,
                     rho_E_opti_simplex = rho_E_opti_simplex,
                     method_E_opti_simplex = paste0("range_top_", x*100, "_percent_and_positive"))
        )
      }
      
    } else if (method_E == "positive_range_top_x_percent") {
      
      # Method 5: Smallest E among the E that are in the x% highest forecasting skills with a full range saturated to 0 if there are negative forecasting skills
      
      for (x in methods_select_E[[method_E]]) {
        if (max(df_out_simplex$rho, na.rm = TRUE) <= 0) {
          E_opti_simplex = NA
          rho_E_opti_simplex = NA
        } else {
          min_bound_range_top_pos_x = max(df_out_simplex$rho, na.rm = TRUE) - 
            x * ( max(df_out_simplex$rho, na.rm = TRUE) - max(0, min(df_out_simplex$rho, na.rm = TRUE)) )
          idx_best_rho = which(df_out_simplex$rho >= min_bound_range_top_pos_x) # get the rho of the top x% of the forecasting skills
          E_opti_simplex = min(df_out_simplex$E[idx_best_rho]) # the smallest E among these among the best rho
          rho_E_opti_simplex = df_out_simplex$rho[df_out_simplex$E == E_opti_simplex]
        }
        
        out = out %>% rbind(
          data.frame(E_opti_simplex = E_opti_simplex,
                     rho_E_opti_simplex = rho_E_opti_simplex,
                     method_E_opti_simplex = paste0("positive_range_top_", x*100, "_percent"))
        )
      }
      
    }
  }
  
  return(out)
  
}

