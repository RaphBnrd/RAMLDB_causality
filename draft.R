
df %>% 
  group_by_at(vars(all_of(name_id_timeseries))) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  pull(all_of(name_id_timeseries))


all_ids_plots = df %>% 
  group_by_at(vars(all_of(name_id_timeseries))) %>%
  summarise(n = n()) %>%
  filter(n >= 40) %>%
  pull(all_of(name_id_timeseries)) %>% unique()

ids1 = res_ccm_best_E_assessment %>% 
  filter(tp == 0, var_lib == "prodbest.div", var_target == "sst.z", causality, id_timeseries %in% all_ids_plots) %>% 
  pull(id_timeseries)

ids2 = res_ccm_best_E_assessment %>% 
  filter(tp == 0, var_lib == "prodbest.div", var_target == "UdivUmsypref", causality, id_timeseries %in% all_ids_plots) %>% 
  pull(id_timeseries)

ids3 = res_strength_causality %>% 
  filter(tp == 0, var_cause == "UdivUmsypref", var_consequence == "prodbest.div", id_timeseries %in% all_ids_plots,
         this_trend_p_val < 0.05, this_trend_slope < 0) %>%
  pull(id_timeseries)

(common_ids = intersect(intersect(ids1, ids2), ids3))



# Pierre et al ------------------------------------------------------------

i = 30

this_id = df %>% group_by(stockid) %>% summarise(n.obs = n()) %>% arrange(desc(n.obs)) %>% pull(stockid) %>% .[i]
this_var_lib = "prodbest.div"
this_var_target = "sst.z"
this_E = 4
this_tp = 2

this_df = df[which(df[[name_id_timeseries]] == this_id), c(name_time, this_var_lib, this_var_target)]
this_libs = unique(round(seq(5, nrow(this_df)-this_E, length.out = 20)))

out_ccm = suppressWarnings(
  CCM_vs_null(this_df, this_var_lib, this_var_target, 
              E = this_E, libs = this_libs, tp = this_tp,
              num_boot_origin = 100, num_shuffle_null = 100, rd_seed = NULL,
              method_shuffle = "keep_correspondance", stats_only = TRUE,
              quantiles_summary = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))
)

out_ccm$out.origin %>% 
  ggplot(aes(x = lib_size, y = log(rho))) + 
  geom_point(alpha = 0.3) + 
  # GLM
  geom_smooth(method = "glm", method.args = list(family = "gaussian"))

# GLM
glm_rho = glm(log(rho) ~ lib_size, data = out_ccm$out.origin)
summary(glm_rho)
signif_GLM = summary(glm_rho)$coefficients[2, 4] < 0.05

# Kendal test
kendal = cor.test(out_ccm$out.origin$lib_size, out_ccm$out.origin$rho, method = "kendall")
signif_kendal = kendal$p.value < 0.05 & kendal$estimate > 0




# Figures -----------------------------------------------------------------



library(ggalluvial)

df_GC = read.csv("out/20240923-test/computations/04_a-out_granger_causality.csv")
df_CCM = read.csv("out/20240906-ini/computations/out_strength_conf_ccm_smap.csv")
load("out/20240906-ini/computations/out_smap_best_E.RData") # res.smap.best.E

df_GC_details = read.csv("out/20240923-test/computations/04_b-out_granger_causality_details.csv")
df_GC_for_filter = df_GC_details %>% 
  group_by(stockid, cause, consequence) %>%
  summarise(strongest_abs_coef = max(abs(coef_estimate)), sum_coef = sum(coef_estimate))


# Alluvial plot

df_plot = left_join(
  
  df_GC %>% dplyr::select(-orders) %>% rename(causality_GC = causality),
  # df_GC %>% dplyr::select(-orders) %>% rename(causality_GC = causality) %>%
  #   left_join(df_GC_for_filter, by = c("stockid" = "stockid", "cause" = "cause", "consequence" = "consequence")) %>%
  #   mutate(causality_GC = ifelse(!causality_GC,
  #                                FALSE, # if the GC is FALSE, it remains FALSE
  #                                ifelse(causality_GC & strongest_abs_coef < 0.5,
  #                                       FALSE, # if the GC is TRUE but the sum of the coefficients is less than 0.2, it becomes FALSE
  #                                       TRUE)) ), # otherwise, it remains TRUE
  # df_GC %>% dplyr::select(-orders) %>% rename(causality_GC = causality) %>%
  #   left_join(df_GC_for_filter, by = c("stockid" = "stockid", "cause" = "cause", "consequence" = "consequence")) %>%
  #   mutate(causality_GC = ifelse(!causality_GC,
  #                                FALSE, # if the GC is FALSE, it remains FALSE
  #                                ifelse(causality_GC & r_squared < 0.5,
  #                                       FALSE, # if the GC is TRUE but the sum of the coefficients is less than 0.2, it becomes FALSE
  #                                       TRUE)) ), # otherwise, it remains TRUE
  
  df_CCM %>% dplyr::select(stockid, var_lib, var_target, causality, max_rho) %>% rename(causality_CCM = causality),
  by = c("stockid" = "stockid", "cause" = "var_target", "consequence" = "var_lib")
) %>% 
  mutate(label_cause = paste0(cause, " -> ", consequence)) %>% 
  arrange(label_cause, causality_GC, causality_CCM)


# ggplot(df_plot, aes(axis1 = causality_CCM, axis2 = causality_GC)) +
#   geom_alluvium(aes(fill = label_cause)) +
#   geom_stratum() +
#   scale_x_discrete(limits = c("CCM", "Granger Causality"), expand = c(0.1, 0.1)) +
#   geom_text(aes(label = after_stat(stratum), color = after_stat(stratum)), 
#             stat = "stratum", size = 8, fontface = "bold") +
#   scale_color_manual(values = c("FALSE" = "#de7371", "TRUE" = "#97d89a")) +
#   labs(title = "Comparison of Granger causality and CCM") +
#   theme_minimal() +
#   guides(fill = guide_legend(title = "Cause -> Consequence"), color = guide_none()) + 
#   theme(axis.text.x = element_text(size = 12))


# Alluvial plot

df_plot_grouped <- df_plot %>%
  group_by(label_cause, causality_GC, causality_CCM) %>%
  summarise(flow_count = n(), .groups = "drop") %>%
  distinct(label_cause, causality_GC, causality_CCM, flow_count)

p = df_plot_grouped %>% 

  ggplot(aes(axis1 = causality_CCM, axis2 = causality_GC, y = flow_count)) +
  geom_alluvium(aes(fill = label_cause)) +
  geom_stratum() +
  
  # Add text labels for stratum boxes
  geom_text(aes(label = after_stat(stratum), color = after_stat(stratum)), 
            stat = "stratum", size = 5, fontface = "bold") +
  
  # Add text labels for the flows with the count for each distinct combination
  geom_text(aes(label = flow_count, group = label_cause),
            stat = "alluvium", size = 3) +
  
  scale_x_discrete(limits = c("CCM", "Granger Causality"), expand = c(0.1, 0.1)) +
  scale_color_manual(values = c("FALSE" = "#de7371", "TRUE" = "#97d89a")) +
  
  labs(title = "Comparison of Granger causality and CCM", y = "Count") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Cause -> Consequence"), color = guide_none()) + 
  theme(axis.text.x = element_text(size = 12))
ggsave("out/20240923-test/granger_causality_alluvial.png", p, width = 10, height = 6, dpi = 300)

p = p + facet_wrap(~label_cause, scales = "free_y") + theme_minimal()
ggsave("out/20240923-test/granger_causality_alluvial_facet.png", p, width = 10, height = 6, dpi = 300)


df_plot %>% 
  group_by(cause, consequence) %>%
  summarise(perc_causality_GC = sum(causality_GC) / n() * 100,
            perc_causality_CCM = sum(causality_CCM) / n() * 100)



# Boxplot coefficients
p = df_GC_details %>% 
  mutate(label_cause = paste0(cause, " -> ", consequence)) %>%
  ggplot(aes(x = factor(lag), y = coef_estimate, color = variable)) +
  facet_wrap(~label_cause) +
  geom_boxplot() +
  labs(title = "Boxplot of the coefficients for the Granger Causality",
       x = "Lag", y = "Coefficient estimate") +
  theme_minimal() + 
  coord_cartesian(ylim = c(-1, 1))
ggsave("out/20240923-test/granger_causality-boxplot_coefs.png", p, width = 10, height = 6, dpi = 300)

p = df_GC_details %>% 
  mutate(label_cause = paste0(cause, " -> ", consequence)) %>%
  filter(variable == "cause") %>% 
  ggplot(aes(x = lag, y = coef_estimate, color = variable, group = stockid)) +
  facet_wrap(~label_cause) +
  geom_point(alpha = 0.3) + geom_line(alpha = 0.3) + 
  labs(title = "Boxplot of the coefficients for the Granger Causality",
       x = "Lag", y = "Coefficient estimate") +
  theme_minimal() + 
  coord_cartesian(ylim = c(-1, 1))
ggsave("out/20240923-test/granger_causality-scatter_coefs.png", p, width = 10, height = 6, dpi = 300)




# Links? 

df_info_CCM = res.smap.best.E %>% 
  group_by(variable, stockid, E) %>%
  summarise(best_theta = theta[which.max(rho)][1], .groups = "drop") %>%
  ungroup()

# Link theta

p = df_plot %>% 
  mutate(case = case_when(
    causality_GC & causality_CCM ~ "Both",
    causality_GC ~ "GC",
    causality_CCM ~ "CCM",
    TRUE ~ "None"
  )) %>% 
  left_join(df_info_CCM, by = c("stockid" = "stockid", "consequence" = "variable")) %>%
  ggplot(aes(x = best_theta, fill = label_cause)) +
  facet_wrap(~case, labeller = labeller(case = function(x) paste0("Causality found for: ", x))) +
  geom_histogram(position = "dodge", binwidth = 0.4) +
  labs(title = "Distribution of the best theta for the S-map",
       x = "Best theta", y = "Count") +
  theme_minimal()
ggsave("out/20240923-test/granger_causality-hist_theta.png", p, width = 10, height = 6, dpi = 300)

# Link rho
p = df_plot %>% 
  mutate(case = case_when(
    causality_GC & causality_CCM ~ "Both",
    causality_GC ~ "GC",
    causality_CCM ~ "CCM",
    TRUE ~ "None"
  )) %>% 
  ggplot(aes(x = max_rho, fill = label_cause)) +
  facet_wrap(~case, labeller = labeller(case = function(x) paste0("Causality found for: ", x))) +
  geom_histogram(position = "dodge", binwidth = 0.1) +
  labs(title = "Distribution of the max rho for the CCM",
       x = "Max forecasting skill", y = "Count") +
  theme_minimal()
ggsave("out/20240923-test/granger_causality-hist_rho.png", p, width = 10, height = 6, dpi = 300)

# Link E

p = df_plot %>% 
  mutate(case = case_when(
    causality_GC & causality_CCM ~ "Both",
    causality_GC ~ "GC",
    causality_CCM ~ "CCM",
    TRUE ~ "None"
  )) %>% 
  left_join(df_info_CCM, by = c("stockid" = "stockid", "consequence" = "variable")) %>%
  ggplot(aes(x = E, fill = label_cause)) +
  facet_wrap(~case, labeller = labeller(case = function(x) paste0("Causality found for: ", x))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of the best embeding dimensions for the Simplex",
       x = "Best E", y = "Count") +
  theme_minimal()
ggsave("out/20240923-test/granger_causality-hist_E.png", p, width = 10, height = 6, dpi = 300)


# Quality of GC
p = df_GC %>% rename(causality_GC = causality) %>%
  left_join(df_CCM %>% dplyr::select(stockid, var_lib, var_target, causality) %>% rename(causality_CCM = causality),
            by = c("stockid" = "stockid", "cause" = "var_target", "consequence" = "var_lib")) %>%
  mutate(label_cause = paste0(cause, " -> ", consequence),
         case = case_when(
           causality_GC & causality_CCM ~ "Both",
           causality_GC ~ "GC",
           causality_CCM ~ "CCM",
           TRUE ~ "None"
         )) %>%
  ggplot(aes(x = AIC, fill = label_cause)) +
  facet_wrap(~case) +
  geom_histogram(position = "dodge") +
  labs(title = "Quality of the Granger Causality",
       x = "AIC", y = "Count") + 
  theme_minimal()
ggsave("out/20240923-test/granger_causality-hist_AIC.png", p, width = 10, height = 6, dpi = 300)


p = df_GC %>% rename(causality_GC = causality) %>%
  left_join(df_CCM %>% dplyr::select(stockid, var_lib, var_target, causality) %>% rename(causality_CCM = causality),
            by = c("stockid" = "stockid", "cause" = "var_target", "consequence" = "var_lib")) %>%
  mutate(label_cause = paste0(cause, " -> ", consequence),
         case = case_when(
           causality_GC & causality_CCM ~ "Both",
           causality_GC ~ "GC",
           causality_CCM ~ "CCM",
           TRUE ~ "None"
         )) %>%
  ggplot(aes(x = r_squared, fill = label_cause)) +
  facet_wrap(~case) +
  geom_histogram(position = "dodge") +
  labs(title = "Quality of the Granger Causality",
       x = "R squared", y = "Count") + 
  theme_minimal()
ggsave("out/20240923-test/granger_causality-hist_r_squared.png", p, width = 10, height = 6, dpi = 300)



