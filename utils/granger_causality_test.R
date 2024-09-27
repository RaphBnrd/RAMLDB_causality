
library(tidyverse)

library(MASS)# for stepAIC




# Function Granger causality ----------------------------------------------


granger_causality_assessment_pair = function(df, causality_tested = NULL, max_order_tested = 10,
                                             first_column_time = TRUE, silent = TRUE, details = TRUE) {
  
  # Set all the causality that will be tested (if not provided)
  if (is.null(causality_tested)) {
    columns = names(df)
    if (first_column_time) columns = columns[-1]
    causality_tested = expand.grid(cause = columns, consequence = columns) %>% 
      filter(cause != consequence)
  }
  # causality_tested = causality_tested %>% as.data.frame() %>% 
  #   mutate(causality = NA, orders = as.list(rep(NA, nrow(.))), coefs = as.list(rep(NA, nrow(.))) )
  causality_tested = causality_tested %>% as.data.frame() %>% 
    mutate(causality = NA, orders = NA, AIC = NA, r_squared = NA)
  causality_tested_detailed = data.frame()
  
  # Loop over all the causality to be tested
  for (k in 1:nrow(causality_tested)) {
    
    var_cause = as.character(causality_tested$cause[k])
    var_consequence = as.character(causality_tested$consequence[k])
    
    if (!silent) cat(paste0("\n\n* * * Cause: ", var_cause, " - Consequence: ", var_consequence, " * * *\n"))
    
    # Select the number of lags to include in the null model (with only the consequence variable)
    res.ar = ar(df[[var_consequence]], order.max = max_order_tested)
    best_order_consequence = res.ar$order
    
    if (!silent) cat(paste0("Best order for consequence (", var_consequence, "):  p=", best_order_consequence, "\n"))
    
    # Create the dataframe with the lags of the consequence variable
    df_model = data.frame(consequence_t_m_0 = df[[var_consequence]])
    if (best_order_consequence > 0) {
      for (ord in 1:best_order_consequence) {
        df_model = df_model %>% mutate(!!paste0("consequence_t_m_", ord) := lag(df[[var_consequence]], ord))
      }
    }
    
    # Add all the lags of the cause variable until the maximum order tested
    for (ord in 1:max_order_tested) {
      df_model = df_model %>% mutate(!!paste0("cause_t_m_", ord) := lag(df[[var_cause]], ord))
    }
    df_model = df_model %>% na.omit()
    
    # Define the null model
    min_formula = paste0("consequence_t_m_0 ~ ",
                         ifelse(best_order_consequence > 0, 
                                paste0(paste0("consequence_t_m_", 1:best_order_consequence), collapse = " + "), "1"))
    min_mod = do.call("lm", list(formula = as.formula(min_formula), data = df_model)) # do.call is necessary to avoid errors with stepAIC
    # Define the full model (including all the lags of the causal variable)
    max_formula = paste0("consequence_t_m_0 ~ ",
                         ifelse(best_order_consequence > 0,
                                paste0(paste0(paste0("consequence_t_m_", 1:best_order_consequence), collapse = " + "), " + "), ""),
                         paste0(paste0("cause_t_m_", 1:max_order_tested), collapse = " + "))
    
    # Step BIC to find the lags in the causal variable that decrease the BIC
    best_mod = stepAIC(
      min_mod, scope = list(lower = min_formula, upper = max_formula), trace = FALSE,
      direction = "both", k = log(nrow(get("df_model"))) # remove the k parameter to use the AIC
    )
    
    # Get the orders of the lags of the causal variable that are selected
    orders = grep("^cause_", names(best_mod$coefficients), value = TRUE) %>% 
      gsub("cause_t_m_", "", .) %>% as.numeric()
    
    causality = length(orders) > 0
    
    coefs = summary(best_mod)$coefficients
    
    if (!silent) cat(paste0("Causality=", causality, " - Order=", paste0(orders, collapse = ";"), "\n"))
    
    
    # Save the results
    causality_tested$causality[k] = causality
    causality_tested$orders[[k]] = orders %>% paste0(collapse = "|")
    causality_tested$AIC[k] = AIC(best_mod)
    causality_tested$r_squared[k] = summary(best_mod)$r.squared
    
    causality_tested_detailed = causality_tested_detailed %>% rbind(
      data.frame(cause = var_cause, consequence = var_consequence, causality = causality, 
                 variable = rownames(coefs) %>% gsub("\\(Intercept\\)", "intercept", .) %>% gsub("_[^_]*", "", .),
                 lag = as.numeric(sapply(rownames(coefs), function(x) ifelse(grepl("_t_m_", x), gsub(".*_t_m_", "", x), NA))),
                 coef_estimate = unname(coefs[, "Estimate"]), se = unname(coefs[, "Std. Error"]), 
                 t_value = unname(coefs[, "t value"]), p_value = unname(coefs[, "Pr(>|t|)"]) )
    )
    
  }
  
  return(list(causality_tested = causality_tested, 
              causality_tested_detailed = causality_tested_detailed))
  
}



# Test on a simulated dataset --------------------------------------------

test_the_functions = FALSE

if (test_the_functions) {
  t_max = 100
  df_GC_test = data.frame(t = 1:t_max, x = runif(t_max, 0, 1))
  df_GC_test$y = - 1 * lag(df_GC_test$x, 1) + 2 * lag(df_GC_test$x, 2) + rnorm(t_max, 0, 0.01)
  df_GC_test = df_GC_test %>% na.omit()
  
  source("utils/plot_timeseries.R")
  plot_phase_space(df_GC_test, var_x = "x", var_y = "y")
  
  # df = df_GC_test
  # causality_tested = NULL
  # max_order_tested = 10
  # first_column_time = TRUE
  # silent = FALSE
  
  res.gc = granger_causality_assessment_pair(df_GC_test, causality_tested = NULL, 
                                             max_order_tested = 10, silent = FALSE)
  
  # write.csv(res.gc$causality_tested, "test1.csv")
  # write.csv(res.gc$causality_tested_detailed, "test2.csv")
  
}


