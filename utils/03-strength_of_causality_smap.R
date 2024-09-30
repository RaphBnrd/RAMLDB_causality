
library(tidyverse)
library(rEDM)

# jacobian_smap: Compute the Jacobian matrix along time using the multivariate S-Map method
# Inputs: 
#   - data: dataframe containing the variables of the embedded space (drivers, consequence and the lags ; or any other space)
#   - thetas: vector of the theta values tested in the S-map
#   - first_column_time: boolean indicating if the first column of the dataframe is the time index 
#                        (time should be in the first column if it's in the dataframe)
#   - single_target: if NA, the Jacobian matrix is computed for all variables as target.
#                    Generally, we define the target as the consequence variable (with lag 0).



jacobian_smap <- function(data, thetas=seq(0, 20, 0.2), first_column_time=TRUE, single_target=NA) {
  
  tp_smap = 1 # time prediction (1 delay for the consequence)
  
  if (first_column_time) { 
    value_time_indices = data[[colnames(data)[1]]] # get the real time values
    variables <- colnames(data)[2:length(colnames(data))] # variables names
  } else { 
    value_time_indices = 1:nrow(data) # get the index as time values
    variables <- colnames(data) # variables names
  }
  variables_target = ifelse(is.na(single_target), variables, single_target)
  
  df_all_smap_output = data.frame()
  for (target in variables_target) { # variable to forecast (consequence)
    # performing s-map
    smap_output <- block_lnlp(block = as.data.frame(data), method = "s-map", tp = tp_smap, 
                              columns = variables,  target_column = target, theta = thetas,
                              stats_only = FALSE, first_column_time = first_column_time,
                              save_smap_coefficients = TRUE, silent = TRUE)
    df_all_smap_output = rbind(df_all_smap_output, 
                               cbind(consequence = target, smap_output))
  }
  
  # If there are several target, we only consider one value of theta that maximizes the average rho, 
  # since the system should present a non-linearity independently of the target variable.
  best_average_theta = df_all_smap_output %>% 
    group_by(theta) %>% # for each theta, we compute the average rho across all variables as target
    summarise(mean_rho = mean(rho)) %>%
    ungroup() %>% 
    filter(mean_rho == max(mean_rho)) %>% 
    pull(theta)
  
  df_out_smap_coefs = data.frame()
  
  for (i in 1:length(variables)) {
    for (j in 1:length(variables_target)) {
      df_out_smap_coefs = rbind(
        df_out_smap_coefs,
        data.frame(
          t = value_time_indices,
          var_cause_jacobian = variables[i],
          var_consequence_jacobian = ifelse(tp_smap == 0, paste0(variables_target[j], "_t"), 
                                            ifelse(tp_smap > 0, paste0(variables_target[j], "_t_plus_", tp_smap),
                                                   paste0(variables_target[j], "_t_minus_", abs(tp_smap)))),
          theta = best_average_theta,
          value = df_all_smap_output %>% 
            filter(consequence == variables_target[j], theta == best_average_theta) %>% 
            pull(smap_coefficients) %>% .[[1]] %>% 
            # select column n°i
            .[,i]
        )
      )
    }
  }
  return(df_out_smap_coefs)
}



