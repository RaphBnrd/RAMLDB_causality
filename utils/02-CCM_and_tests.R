library(rEDM)
library(Kendall)

library(dplyr)
library(tidyr)
library(ggplot2)


test_the_functions = FALSE


# Computations (CCM vs null model) ----------------------------------------


# CCM_vs_null: Compute the CCM vs null model for a given pair of variables
# Inputs: 
#   - data: dataframe containing the two variables to cross-map (ordered in time, scaled if possible)
#   - var_lib: name of the variable to use as the library (predictor, consequence of the phenomenon tested)
#   - var_target: name of the variable to use as the target (predicted, cause of the phenomenon tested)
#   - E: embedding dimension(s) to test
#   - libs: library sizes to test
#   - tp: time horizon for prediction (time to predict, positive or negative integer)
#   - num_boot_origin: number of bootstraps to compute the original CCM (num_samples in ccm function)
#   - num_shuffle_null: number of shuffles considered as the null model
#   - rd_seed: seed for the random number generator (RNGseed in ccm function)
#   - method_shuffle: method to build the surrogates for the null model
#   - stats_only: if TRUE, only the statistics of the CCM are returned (no model_output, i.e. prediction)
#   - quantiles_summary: quantiles to compute in the summary of the CCM on the original bootstraps and null model
# Outputs:
#   - list containing the results of the CCM on the original data and the null model, and their summary
#   - dataframes of the shuffled data (surrogates in the null model)
#   - list of the values of the function call


CCM_vs_null = function(data, var_lib, var_target, 
                       E = 4, libs = c(seq(20,80,20),seq(100,1000,100)), tp = 0,
                       num_boot_origin = 200, num_shuffle_null = 100, rd_seed = NULL,
                       method_shuffle = "keep_correspondance", stats_only = TRUE,
                       quantiles_summary = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) {
  
  # * * * Create null permutations * * *
  
  if (method_shuffle == "keep_correspondance") {
    # In this method, we just shuffle time indices of the original data but the
    # value signal.1(t) will remain paired with signal.2(t) (but at a different time)
    dat.shuffle = data.frame()
    for (i in 1:num_shuffle_null) {
      name_new_colx = paste0(var_lib, ".s", formatC(i, width = nchar(num_shuffle_null), flag = "0"))
      name_new_coly = paste0(var_target, ".s", formatC(i, width = nchar(num_shuffle_null), flag = "0"))
      
      rd_permut = sample(nrow(data))
      
      additional_cols = data.frame(new_colx = data[rd_permut, var_lib],
                                   new_coly = data[rd_permut, var_target])
      colnames(additional_cols) = c(name_new_colx, name_new_coly)
      if (i == 1) { dat.shuffle = additional_cols
      } else { dat.shuffle = cbind(dat.shuffle, additional_cols) }
    }
  } else {
    stop("Method not implemented yet")
  }
  
  
  out.origin = data.frame() # list of the results of the CCM on the original data: out.origin[[E]] is a dataframe
  summary.origin = data.frame() # list of the summary of the CCM on the original data: summary.origin[[E]] is a dataframe containing the quantiles
  out.null = data.frame() # for the null model: out.null[[E]] is a dataframe
  summary.null = data.frame() # for the null model: summary.null[[E]] is a dataframe
  
  
  # * * * Apply the CCM * * *
  
  for (this_E in E) {
    
    # On the original data
    if (packageVersion("rEDM") < "1") { # For instance version 0.7.4
      out_CCM_origin_this_E <- ccm(data, E = this_E, tp = tp, 
                                   lib_column = var_lib, target_column = var_target, 
                                   lib_sizes = libs, num_samples = num_boot_origin, 
                                   replace = T, RNGseed = rd_seed, stats_only = stats_only)
    } else { # For instance version 1.15.4
      out_CCM_origin_this_E <- CCM(dataFrame = data, E = this_E, Tp = tp, 
                                   columns = var_lib, target = var_target, 
                                   libSizes = paste0(libs, collapse = " "), sample = num_boot_origin,
                                   seed = ifelse(is.null(rd_seed), 0, rd_seed),
                                   includeData=TRUE)#, showPlot = TRUE)
      out_CCM_origin_this_E = out_CCM_origin_this_E[["CCM1_PredictStat"]] %>% 
        rename(lib_size = LibSize, mae = MAE, rmse = RMSE)
    }
    
    out.origin = out.origin %>% rbind(out_CCM_origin_this_E)
    
    # Summary df of the original data
    new_rows_summary.origin = cbind(
        # Rho
        out_CCM_origin_this_E %>% group_by(lib_size) %>% 
          summarise(rho_mean = mean(rho, na.rm = T), 
                    rho_mean.m.sd = mean(rho, na.rm = T) - sd(rho, na.rm = T), 
                    rho_mean.p.sd = mean(rho, na.rm = T) + sd(rho, na.rm = T)),
        as.matrix(aggregate(out_CCM_origin_this_E$rho, by = list(as.factor(out_CCM_origin_this_E$lib_size)), 
                            quantile, probs = quantiles_summary, na.rm = T)[,'x']) %>%
          as.data.frame() %>%
          setNames(paste0("rho_", quantiles_summary * 100)),
        # MAE
        out_CCM_origin_this_E %>% group_by(lib_size) %>% 
          summarise(mae_mean = mean(mae, na.rm = T), 
                    mae_mean.m.sd = mean(mae, na.rm = T) - sd(mae, na.rm = T), 
                    mae_mean.p.sd = mean(mae, na.rm = T) + sd(mae, na.rm = T)) %>% 
          dplyr::select(-lib_size),
        as.matrix(aggregate(out_CCM_origin_this_E$mae, by = list(as.factor(out_CCM_origin_this_E$lib_size)), 
                            quantile, probs = quantiles_summary, na.rm = T)[,'x']) %>%
          as.data.frame() %>%
          setNames(paste0("mae_", quantiles_summary * 100)),
        # RMSE
        out_CCM_origin_this_E %>% group_by(lib_size) %>% 
          summarise(rmse_mean = mean(rmse, na.rm = T), 
                    rmse_mean.m.sd = mean(rmse, na.rm = T) - sd(rmse, na.rm = T), 
                    rmse_mean.p.sd = mean(rmse, na.rm = T) + sd(rmse, na.rm = T)) %>% 
          dplyr::select(-lib_size),
        as.matrix(aggregate(out_CCM_origin_this_E$rmse, by = list(as.factor(out_CCM_origin_this_E$lib_size)), 
                            quantile, probs = quantiles_summary, na.rm = T)[,'x']) %>%
          as.data.frame() %>%
          setNames(paste0("rmse_", quantiles_summary * 100))
      )
    summary.origin = summary.origin %>% rbind(
      new_rows_summary.origin %>% 
        add_column(E = this_E, .before = 1)
    )
    
    
    # On the null model
    for (i in 1:num_shuffle_null) {
      
      this_col_lib_shuffle = paste0(var_lib, ".s", formatC(i, width = nchar(num_shuffle_null), flag = "0"))
      this_col_target_shuffle = paste0(var_target, ".s", formatC(i, width = nchar(num_shuffle_null), flag = "0"))
      
      if (packageVersion("rEDM") < "1") { # For instance version 0.7.4
        out_CCM_this_null_this_E <- ccm(dat.shuffle[, c(this_col_lib_shuffle, this_col_target_shuffle)], 
                                        E = this_E, tp = tp,
                                        lib_column = this_col_lib_shuffle, 
                                        target_column = this_col_target_shuffle,
                                        lib_sizes = libs, num_samples = 1, 
                                        replace = T, RNGseed = rd_seed)
      } else { # For instance version 1.15.4
        out_CCM_this_null_this_E <- CCM(dataFrame = dat.shuffle[, c(this_col_lib_shuffle, this_col_target_shuffle)], 
                                        E = this_E, Tp = tp, 
                                        columns = this_col_target_shuffle, 
                                        target = this_col_target_shuffle,
                                        libSizes = paste0(libs, collapse = " "), sample = 1,
                                        seed = ifelse(is.null(rd_seed), 0, rd_seed),
                                        includeData=TRUE, noTime = TRUE)#, showPlot = TRUE)
        out_CCM_this_null_this_E = out_CCM_this_null_this_E[["CCM1_PredictStat"]] %>% 
          rename(lib_size = LibSize, mae = MAE, rmse = RMSE)
      }
      
      out.null = out.null %>% rbind(
        out_CCM_this_null_this_E %>% 
          mutate(shuffle = i, lib_column = var_lib, target_column = var_target)
      )
    }
    
    out_CCM_all_null_this_E = out.null %>% filter(E == this_E)
    
    # Summary df of the null model
    new_rows_summary.null = cbind(
      # Rho
      out_CCM_all_null_this_E %>% group_by(lib_size) %>% 
        summarise(rho_mean = mean(rho, na.rm = T), 
                  rho_mean.m.sd = mean(rho, na.rm = T) - sd(rho, na.rm = T), 
                  rho_mean.p.sd = mean(rho, na.rm = T) + sd(rho, na.rm = T)),
      as.matrix(aggregate(out_CCM_all_null_this_E$rho, by = list(as.factor(out_CCM_all_null_this_E$lib_size)), 
                          quantile, probs = quantiles_summary, na.rm = T)[,'x']) %>%
        as.data.frame() %>%
        setNames(paste0("rho_", quantiles_summary * 100)),
      # MAE
      out_CCM_all_null_this_E %>% group_by(lib_size) %>%
        summarise(mae_mean = mean(mae, na.rm = T), 
                  mae_mean.m.sd = mean(mae, na.rm = T) - sd(mae, na.rm = T), 
                  mae_mean.p.sd = mean(mae, na.rm = T) + sd(mae, na.rm = T)) %>% 
        dplyr::select(-lib_size),
      as.matrix(aggregate(out_CCM_all_null_this_E$mae, by = list(as.factor(out_CCM_all_null_this_E$lib_size)), 
                          quantile, probs = quantiles_summary, na.rm = T)[,'x']) %>%
        as.data.frame() %>%
        setNames(paste0("mae_", quantiles_summary * 100)),
      # RMSE
      out_CCM_all_null_this_E %>% group_by(lib_size) %>%
        summarise(rmse_mean = mean(rmse, na.rm = T), 
                  rmse_mean.m.sd = mean(rmse, na.rm = T) - sd(rmse, na.rm = T), 
                  rmse_mean.p.sd = mean(rmse, na.rm = T) + sd(rmse, na.rm = T)) %>% 
        dplyr::select(-lib_size),
      as.matrix(aggregate(out_CCM_all_null_this_E$rmse, by = list(as.factor(out_CCM_all_null_this_E$lib_size)), 
                          quantile, probs = quantiles_summary, na.rm = T)[,'x']) %>%
        as.data.frame() %>%
        setNames(paste0("rmse_", quantiles_summary * 100))
    )
    summary.null = summary.null %>% rbind(
      new_rows_summary.null %>% 
        add_column(E = this_E, .before = 1)
    )
    
  }
  
  
  return(list(out.origin = out.origin, summary.origin = summary.origin, 
              out.null = out.null, summary.null = summary.null,
              dat.shuffle = dat.shuffle, 
              values_call = list(
                call = match.call(), 
                data = data, var_lib = var_lib, var_target = var_target, E = E, libs = libs, 
                num_boot_origin = num_boot_origin, num_shuffle_null = num_shuffle_null,
                rd_seed = rd_seed, method_shuffle = method_shuffle, stats_only = stats_only
              )
  ))
}


# Test the function on datasets

if (test_the_functions) {
  cat("\n - - - Compute the CCM vs null model for N1 xmap N2 - - - \n")
  dat.N = read.csv("../convergent_cross_mapping-tuto/material/ere0785-sup-0004.csv", 
                   header = TRUE, sep = ",", dec = ".") %>% 
    dplyr::select(N1, N2) %>% 
    mutate(N1 = scale(N1)[,1], N2 = scale(N2)[,1])
  res.N = CCM_vs_null(dat.N, "N1", "N2", E = 4:6, libs = c(seq(20,80,20),seq(100,1000,100)),
                      num_boot_origin = 200, num_shuffle_null = 100, rd_seed = NULL,
                      method_shuffle = "keep_correspondance",
                      quantiles_summary = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))
  
  cat("\n - - - Compute the CCM vs null model for M1 xmap M2 - - - \n")
  dat.M = read.csv("../convergent_cross_mapping-tuto/material/ere0785-sup-0006.csv", 
                   header = TRUE, sep = ",", dec = ".") %>% 
    dplyr::select(M1, M2) %>%
    mutate(M1 = scale(M1)[,1], M2 = scale(M2)[,1])
  res.M = CCM_vs_null(dat.M, "M1", "M2", E = 4, libs = c(seq(20,80,20),seq(100,1000,100)),
                      num_boot_origin = 200, num_shuffle_null = 100)
}





# Assess the causality ----------------------------------------------------




# test_causality: Test the causality between two variables using the CCM
# Inputs:
#   - summary.origin.one.E: summary of the CCM on the original data (output of CCM_vs_null) for a single E (e.g. summary.origin[[E]])
#   - summary.null.one.E: summary of the CCM on the null model (output of CCM_vs_null) for a single E (e.g. summary.null[[E]])
#   - val.origin.compare: value to compare in the original data (e.g. "50%" for the median)
#   - val.null.compare: value to compare in the null model (e.g. "95%" for the 95% quantile)
#   - val.origin.kendall: value to use for the Kendall's tau test in the original data (e.g. "50%" for the median)
#   - criterium_compare: criterium to use for the comparison between the original and null models
#   - libs_for_crit: library sizes to use for the comparison criterium (if NULL, all library sizes are used)
#   - threshold_p_val_kendall: threshold for the p-value of the Kendall's tau test
#   - full_output: if TRUE, return a list with all the results and the values of the function call (if FALSE, return only the causality)
#   - silent: if FALSE, print the results
# Outputs:
#   - if full_output = FALSE, return a boolean indicating if the causality is supported
#   - if full_output = TRUE, return a list with all the results and the values of the function call
#      . causality: boolean indicating if the causality is supported
#      . compare.origin.null: dataframe with the comparison between the original and null models (summary value considered for each library size)
#      . vect_compare.origin.null: vector with the comparison between the original and null models (boolean for each library size)
#      . kendall.tau.test: result of the Kendall's tau test on the original data
#      . kendall.tau: value of the Kendall's tau
#      . kendall.p_val: p-value of the Kendall's tau test
#      . values_call: list with the values of the function call

test_causality = function (summary.origin.one.E, summary.null.one.E, 
                           val.origin.compare = "rho_50", val.null.compare = "rho_95",
                           val.origin.kendall = "rho_50",
                           criterium_compare = "strictly_above_null", libs_for_crit = NULL,
                           threshold_p_val_kendall = 0.05,
                           full_output = TRUE, silent = FALSE) {
  
  summary.origin.one.E = summary.origin.one.E %>% arrange(lib_size)
  summary.null.one.E = summary.null.one.E %>% arrange(lib_size)
  
  
  # Check when original (val.origin.compare) is above the random shuffle (val.null.compare)
  
  if (criterium_compare %in% c("strictly_above_null", "90%_above_null", "95%_above_null")) {
    
    compare.origin.null = full_join(
      summary.origin.one.E %>% dplyr::select(lib_size, all_of(val.origin.compare)) %>% 
        rename(!!paste0(val.origin.compare, ".origin") := all_of(val.origin.compare)),
      summary.null.one.E %>% dplyr::select(lib_size, all_of(val.null.compare)) %>%
        rename(!!paste0(val.null.compare, ".null") := all_of(val.null.compare)),
      by = "lib_size"
    ) %>% 
      mutate(origin_above_null = !!sym(paste0(val.origin.compare, ".origin")) > 
               !!sym(paste0(val.null.compare, ".null")) )
    
    vect_compare.origin.null = compare.origin.null %>% pull(origin_above_null)
    names(vect_compare.origin.null) = compare.origin.null$lib_size
    
    if (is.null(libs_for_crit)) libs_for_crit = compare.origin.null$lib_size
    
    if (criterium_compare == "strictly_above_null") {
      crit_compare_satisfied = all(vect_compare.origin.null[as.character(libs_for_crit)])
    } else if (criterium_compare == "90%_above_null") {
      crit_compare_satisfied = mean(vect_compare.origin.null[as.character(libs_for_crit)]) >= 0.90
    } else if (criterium_compare == "95%_above_null") {
      crit_compare_satisfied = mean(vect_compare.origin.null[as.character(libs_for_crit)]) >= 0.95
    }
    
    if (!silent) {
      cat(paste0("\nThe value of ", val.origin.compare, " in the original data is strictly above the value of ", 
                 val.null.compare, " in the null model for the following library sizes:\n",
                 paste0(names(vect_compare.origin.null[vect_compare.origin.null]), collapse = ", "), 
                 "\n(expected to match criterium:\n", paste0(libs_for_crit, collapse = ", "), ")\n",
                 "  >  Comparison criterium satisfied: ", crit_compare_satisfied, "\n"))
    }
    
  } else {
    stop("Criterium for the comparison not implemented yet")
  }
  
  # Check if Kendall's tau reports an increasing trend in the original (tau > 0 and p-value < 0.05, on the median)
  kendall.tau.test = MannKendall(summary.origin.one.E[[val.origin.kendall]])
  
  
  crit_kendall_satisfied = kendall.tau.test$tau[1] > 0 & 
    kendall.tau.test$sl[1] < threshold_p_val_kendall
  
  # Global criterium for causality
  causality = crit_compare_satisfied & crit_kendall_satisfied
  
  if (!silent) {
    cat(paste0("\nKendall's tau test on the value ", val.origin.kendall, " of the original data:\n",
               "tau = ", round(kendall.tau.test$tau[1], 3), "  ;  ",
               "p-value = ", if_else(kendall.tau.test$sl[1] > 10000 | kendall.tau.test$sl[1] < 0.001,
                                     formatC(kendall.tau.test$sl[1], format = 'e', digits = 2),
                                     as.character(round(kendall.tau.test$sl[1], 3)) ), 
               "\n  >  Original cross-mapping shows a significant increasing trend: ", 
               crit_kendall_satisfied, "\n"))
    
    cat(paste0("\n * * * Causality: ", causality, " * * * \n"))
  }
  
  
  if (full_output) {
    return(list(
      causality = causality,
      compare.origin.null = compare.origin.null,
      vect_compare.origin.null = vect_compare.origin.null,
      kendall.tau.test = kendall.tau.test,
      kendall.tau = kendall.tau.test$tau[1],
      kendall.p_val = kendall.tau.test$sl[1],
      values_call = list(
        call = match.call(),
        summary.origin.one.E = summary.origin.one.E,
        summary.null.one.E = summary.null.one.E,
        val.origin.compare = val.origin.compare,
        val.null.compare = val.null.compare,
        val.origin.kendall = val.origin.kendall,
        criterium_compare = criterium_compare,
        libs_for_crit = libs_for_crit,
        threshold_p_val_kendall = threshold_p_val_kendall
      )
    ))
  } else {
    return(causality)
  }
  
}


# Test the function on datasets

if (test_the_functions) {
  cat("\n - - - Test the causality for N1 xmap N2 - - - \n")
  res_causality.N = test_causality(res.N$summary.origin[[1]], res.N$summary.null[[1]],
                                   val.origin.compare = "50%", val.null.compare = "95%",
                                   val.origin.kendall = "50%",
                                   criterium_compare = "strictly_above_null", libs_for_crit = NULL,
                                   threshold_p_val_kendall = 0.05,
                                   full_output = TRUE, silent = FALSE)
  cat("\n\n\n - - - Test the causality for M1 xmap M2 - - - \n")
  res_causality.M = test_causality(res.M$summary.origin[[1]], res.M$summary.null[[1]])
  
  # res.M$summary.origin[[1]] %>% ggplot(aes(x = lib_size, y = `50%`)) + geom_line() + geom_point()
}




