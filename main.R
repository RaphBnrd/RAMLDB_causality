rm(list = ls())

library(tidyverse) 
library(jsonlite)
library(progress) # For the progress bars

library(ggplot2)
library(ggtext)
library(ggrepel)
library(RColorBrewer) # for color palettes

library(rEDM) # For Empircal dynamic modeling (simplex, CCM, block_lnlp...)
library(Kendall) # For Kendall's tau

source("utils/plots_timeseries.R")
source("utils/plots_map.R")


# Parameters of execution -------------------------------------------------

# * * * Choose the parameters * * *

set.seed(123) # Set a seed for reproducibility of the results

# date_time_exe = format(Sys.time(), "%Y%m%d_%H%M%S")

date_time_exe = "20260215_113834"
suffix_name_exe = "article-SProd-RAMv4_66"
list_of_causality_tested = list(
  c("sst.z", "prodbest.divTB"),
  c("UdivUmsypref", "prodbest.divTB"),
  c("prodbest.divTB", "sst.z"),
  c("prodbest.divTB", "UdivUmsypref")
)

# date_time_exe = "20250808_131455"
# suffix_name_exe = "article-Recruitment-RAMv4_66"
# list_of_causality_tested = list(
#   c("sst.z", "R"),
#   c("UdivUmsypref", "R"),
#   c("R", "sst.z"),
#   c("R", "UdivUmsypref")
# )

# import_CCM = TRUE
# import_smap = TRUE
import_CCM = FALSE
import_smap = FALSE

path_dataframe_input = "data/timeseries_clean-v4.66.csv"
name_id_timeseries = "stockid" # Name of the column with the ID of the time series
name_time = "year" # Name of the column with the time variable

ids_single_plots = c("CALSCORPSCAL", "CHROCKSPCOAST", "BRNROCKPCOAST")

plots_with_titles = FALSE
types_plots = c("pdf", "png")


# * * * Automatically set up some parameters * * *

dir_out = paste0("out/", date_time_exe, "-", suffix_name_exe, "/")

labels_of_variables = c(
  "sst" = "SST", 
  "sst.z" = "SST", # "SST (z-score)",
  "prodbest" = "SProd", # "Surplus production", 
  "prodbest.divTB" = "SProd", # "Surplus production", # "Surplus production (divided by mean TB)",
  "UdivUmsypref" = "HRate", # "Harvest rate",
  "R" = "Recruitment"
)

# All distinct variables mentioned in the causality list
all_vars = unique(unlist(list_of_causality_tested))


# * * * Set up the output directories * * *

if (!dir.exists(dir_out)) {
  dir.create(dir_out) # Create the output directory
}
for (typ in types_plots) { 
  if (!dir.exists(paste0(dir_out, typ))) {
    dir.create(paste0(dir_out, typ)) # Create the directories for each type of plot
  }
}
if (!dir.exists(paste0(dir_out, "csv/"))) {
  dir.create(paste0(dir_out, "csv/")) # Create the directory for CSV outputs
}



# Import and plot dataset -------------------------------------------------


# * * * Import the dataset * * *

# Filter out rows with NA in any of the variables
df = read.csv(path_dataframe_input)
cat(paste0("Initial number of stocks = ", length(unique(df[[name_id_timeseries]])), "\n"))

df = df %>% filter(if_all(all_of(all_vars), ~ !is.na(.)))

# IDs that will be included in the analysis
all_ids = df %>% 
  group_by_at(vars(all_of(name_id_timeseries))) %>%
  summarise(n = n()) %>%
  filter(n >= 40) %>%
  pull(all_of(name_id_timeseries)) %>% unique()

df = df %>%
  filter(.data[[name_id_timeseries]] %in% all_ids) # Filter the dataframe to keep only the IDs with enough data

info_stocks = df %>% 
  dplyr::select(stockid, stocklong, assessid, scientificname, commonname, areaid, region) %>% 
  distinct()

write.csv(info_stocks, paste0(dir_out, "csv/00-info_stocks.csv"), row.names = FALSE)    


# * * * Map of pie charts of timeseries length * * *

color.palette.blues = brewer.pal(6, "Blues")
names(color.palette.blues) = c("< 40", "40 to 59", "60 to 79", "80 to 99", "100 to 119", ">= 120")
p.fig1a = ggplot_map_piechart(
  df %>%
    group_by_at(name_id_timeseries) %>%
    mutate(
      dur = max(year) - min(year) + 1,
      dur_category = case_when(
        dur < 40 ~ "< 40",
        dur >= 40 & dur < 60 ~ "40 to 59",
        dur >= 60 & dur < 80 ~ "60 to 79",
        dur >= 80 & dur < 100 ~ "80 to 99",
        dur >= 100 & dur < 120 ~ "100 to 119",
        dur >= 120 ~ ">= 120"
      )
    ) %>%
    mutate(
      dur_category = factor(
        dur_category,
        levels = c("< 40", "40 to 59", "60 to 79", "80 to 99", "100 to 119", ">= 120")
      )
    ),
  name_id_timeseries,
  var_pie = "dur_category",
  label_var_pie = "Duration in years"
) + scale_fill_manual(values = color.palette.blues) + 
  annotate("text", x = -Inf, y = Inf, label = "(a)", vjust = 1.5, hjust = -3, size = 4, fontface = "bold")
  
if (!plots_with_titles) p.fig1a = p.fig1a + labs(title = NULL, subtitle = NULL)
print(p.fig1a)

for (typ in types_plots) {
  ggsave(p.fig1a, filename = paste0(dir_out, typ, "/fig1a-map_piechart_durations.", typ),
         device = typ, width = 8, height = 4)
}


# * * * Timeseries for 1 stock * * *

for (this_id.p.fig1b in ids_single_plots) {
  p.fig1b = ggplot_timeseries_one_stock(
    df[df[[name_id_timeseries]] == this_id.p.fig1b, ], 
    unique(unlist(list_of_causality_tested)), 
    labels_of_variables, 
    id = this_id.p.fig1b, 
    time = name_time, 
    scale = TRUE,
    hide_ylabel = TRUE
  ) + 
    # annotate("text", x = -Inf, y = -Inf, label = "(b)", vjust = 5, hjust = -0.1, size = 5, fontface = "bold") + 
    annotate("text", x = -Inf, y = Inf, label = "(b)", vjust = 1.1, hjust = 1.1, size = 5, fontface = "bold") + 
    coord_cartesian(clip = "off")
  if (!plots_with_titles) p.fig1b = p.fig1b + labs(title = NULL, subtitle = NULL)
  print(p.fig1b)
  
  for (typ in types_plots) {
    ggsave(p.fig1b, 
           filename = paste0(dir_out, typ, "/fig1b-timeseries_one_stock-", this_id.p.fig1b, ".", typ),
           device = typ, width = 6, height = 3.5)
  }
}



# Simplex projection ------------------------------------------------------

# * * * Apply the simplex projection * * *

cat(" * * * Simplex projection * * *\n")
cat("-> Assess the optimal embedding dimension with a time horizon tp = 1...\n")

res_E_opti = data.frame()

pb = progress::progress_bar$new(format = "[:bar] :percent eta: :eta", total = length(all_ids) * length(all_vars))

for (this_id in all_ids) {
  for (this_var in all_vars) {
    
    this_df = df %>% filter(.data[[name_id_timeseries]] == this_id) %>%
      dplyr::select(all_of(c(name_time, this_var)))
    
    
    # Forecasting skill depending on the embedding dimension for the simplex projection
    
    this_rho_E <- EmbedDimension(
      dataFrame = this_df, lib = paste0("1 ", nrow(this_df)), pred = paste0("1 ", nrow(this_df)),
      columns = this_var, target = this_var, maxE = 10, Tp = 1, showPlot = FALSE
    ) %>% 
      filter(E > 1)
    
    # Optimal embedding dimension
    #   > Smallest E among the E that are in the 20% highest forecasting skills with
    #     a full range saturated to 0 if there are negative forecasting skills
    #   > If all forecasting skills are negative, then E_opti = argmax(rho(E))
    
    if (max(this_rho_E$rho, na.rm = TRUE) <= 0) {
      this_E_opti = this_rho_E$E[which.max(this_rho_E$rho)]
      this_rho_opti = max(this_rho_E$rho, na.rm = TRUE)
      this_method_opti = "max_rho"
      this_method_opti_clean = "Max rho"
    } else {
      min_bound_range_top_pos_20 = max(this_rho_E$rho, na.rm = TRUE) - 
        20/100 * ( max(this_rho_E$rho, na.rm = TRUE) - max(0, min(this_rho_E$rho, na.rm = TRUE)) )
      idx_best_rho = which(this_rho_E$rho >= min_bound_range_top_pos_20) # get the rho of the top 20% of the forecasting skills
      this_E_opti = min(this_rho_E$E[idx_best_rho]) # the smallest E among these among the best rho
      this_rho_opti = this_rho_E$rho[this_rho_E$E == this_E_opti]
      this_method_opti = "positive_range_top_20_percent"
      this_method_opti_clean = "Positive range\ntop 20%"
    }
    
    res_E_opti <- res_E_opti %>% rbind(
      data.frame(
        id_timeseries = this_id,
        variable = this_var,
        E_opti = this_E_opti,
        rho_opti = this_rho_opti,
        method_opti = this_method_opti
      )
    )
    
    # Plot
    
    if (this_id %in% ids_single_plots) {
      p.figAdd1 <- ggplot(this_rho_E, aes(x = E, y = rho)) +
        geom_line() + geom_point() +
        geom_vline(xintercept = this_E_opti, linetype = "dashed", color = "#666") +
        geom_text_repel(data = data.frame(x = this_E_opti, y = this_rho_opti, label = this_method_opti_clean),
                        color = "#666666", segment.color = "#666666", aes(x = x, y = y, label = label)) + 
        labs(title = paste0("Embedding Dimension for ", this_id, " - ", labels_of_variables[[this_var]]),
             x = "Embedding Dimension (E)", y = "Forecasting Skill (rho)\nin the Simplex projection") +
        theme_light()
      if (!plots_with_titles) p.figAdd1 = p.figAdd1 + labs(title = NULL, subtitle = NULL)
      print(p.figAdd1)
      
      for (typ in types_plots) {
        ggsave(p.figAdd1, filename = paste0(dir_out, typ, "/figAdd1-simplex_projection-", this_id, "-", this_var, ".", typ),
               device = typ, width = 6, height = 4)
      }
    }
    
    pb$tick()
  }
}

res_E_opti = res_E_opti %>% 
  left_join(info_stocks, by = c("id_timeseries" = name_id_timeseries))

write.csv(res_E_opti, paste0(dir_out, "csv/01-res_E_opti.csv"), row.names = FALSE)    


p.figAdd2 <- ggplot(res_E_opti, aes(x = E_opti)) + 
  facet_wrap(~ variable, labeller = as_labeller(labels_of_variables)) +
  geom_histogram(binwidth = 1, fill = "#0072B2", color = "#444") +
  labs(title = "Histogram of Optimal Embedding Dimension (E) for each variable",
       x = "Optimal Embedding Dimension (E)", y = "Count") +
  theme_light()
print(p.figAdd2)
if (!plots_with_titles) p.figAdd2 = p.figAdd2 + labs(title = NULL, subtitle = NULL)
for (typ in types_plots) {
  ggsave(p.figAdd2, filename = paste0(dir_out, typ, "/figAdd2-optimal_embedding_dimension.", typ),
         device = typ, width = 10, height = 4)
}
  


# CCM ---------------------------------------------------------------------


cat(" * * * CCM with the best E * * *\n")
cat("-> compute CCM\n")

res_CCM = data.frame()


if (!import_CCM) {
  pb = progress::progress_bar$new(format = "[:bar] :percent eta: :eta", total = length(all_ids) * length(list_of_causality_tested))
  
  for (this_id in all_ids) {
    for ( i in 1:length(list_of_causality_tested)) {
        
      this_df = df %>% filter(.data[[name_id_timeseries]] == this_id)
      var_target = list_of_causality_tested[[i]][1]
      var_lib = list_of_causality_tested[[i]][2]
      this_E = res_E_opti$E_opti[res_E_opti$id_timeseries == this_id & res_E_opti$variable == var_lib]
      this_libs = unique(round(seq(
        max(4, this_E)+1, nrow(this_df)-this_E, 
        length.out = min(20, nrow(this_df) - this_E - max(5, this_E) + 1)
      )))
      
      # CCM original
      tmp = CCM(dataFrame = this_df[, c(var_lib, var_target)], E = this_E, Tp = 0, 
                columns = var_lib, target = var_target, 
                libSizes = paste0(this_libs, collapse = " "), 
                sample = 100, includeData = TRUE, noTime = TRUE)
      this_ccm = tmp[["CCM1_PredictStat"]]
      
      out_origin = this_ccm %>% group_by(LibSize) %>% summarise(
        median = median(rho), q5 = quantile(rho, 0.05), q95 = quantile(rho, 0.95)) %>% 
        mutate(label = "Original")
      
      # CCM surrogates
      this_ccm_surr = data.frame()
      for (id_surr in 1:100) {
        idx_shuffle = sample(1:nrow(this_df), nrow(this_df), replace = FALSE)
        this_df_surr = this_df[idx_shuffle, c(var_lib, var_target)]
        
        tmp = CCM(dataFrame = this_df_surr, E = this_E, Tp = 0, 
                  columns = var_lib, target = var_target, 
                  libSizes = paste0(this_libs, collapse = " "), 
                  sample = 1, includeData = TRUE, noTime = TRUE)
        this_ccm_surr = rbind(this_ccm_surr, tmp[["CCM1_PredictStat"]])
        
      }
      
      out_null = this_ccm_surr %>% group_by(LibSize) %>% summarise(
        median = median(rho), q5 = quantile(rho, 0.05), q95 = quantile(rho, 0.95)) %>% 
        mutate(label = "Surrogates")
      
      
      res_CCM = res_CCM %>% rbind(
        out_origin %>% 
          mutate(id_timeseries = this_id, var_target = var_target, var_lib = var_lib, E = this_E),
        out_null %>%
          mutate(id_timeseries = this_id, var_target = var_target, var_lib = var_lib, E = this_E)
      )
      
      # Plot CCM
      if (this_id %in% ids_single_plots) {
        
        p.fig2 = rbind(out_origin, out_null) %>% 
          ggplot(aes(x = LibSize)) +
          geom_ribbon(aes(ymin = q5, ymax = q95, fill = label), alpha = 0.2) +
          geom_line(aes(y = median, color = label)) +
          scale_color_manual(values = c("red", "#444")) +
          scale_fill_manual(values = c("red", "#444")) +
          labs(title = paste0("CCM for ", this_id, ": ", labels_of_variables[[var_target]], 
                              " causing ", labels_of_variables[[var_lib]]), 
               x = "Library Size", y = "Cross Mapping Skill (rho)", 
               color = "Timeseries", fill = "Timeseries") + 
          annotate("text", x = Inf, y = -Inf, vjust = -1.3, hjust = 1.1, size = 3.5, 
                   label = paste0(labels_of_variables[[var_target]], " causing ", labels_of_variables[[var_lib]])) +
          theme_light() + coord_cartesian(clip = "off") + 
          theme(plot.title = element_text(size = 10), legend.position = "top") + 
          guides(fill = "none", color = "none")
        if (i == 1) {
          p.fig2 = p.fig2 + 
            annotate("text", x = -Inf, y = Inf, vjust = 0.9, hjust = 1.75, size = 5,
                     label = "(a) ", fontface = "bold")
        } else if (i == 3) {
          p.fig2 = p.fig2 + 
            annotate("text", x = -Inf, y = Inf, vjust = 0.9, hjust = 1.75, size = 5,
                     label = "(b) ", fontface = "bold")
        }
        if (!plots_with_titles) p.fig2 = p.fig2 + labs(title = NULL, subtitle = NULL)
        print(p.fig2)
        for (typ in types_plots) {
          ggsave(p.fig2, filename = paste0(dir_out, typ, "/fig2-CCM-", this_id, "-", var_target, "_causing_", var_lib, ".", typ),
                 device = typ, width = 5, height = 4)
        }
      }
      
      pb$tick()
    }
  }
  
  res_CCM = res_CCM %>%
    left_join(info_stocks, by = c("id_timeseries" = name_id_timeseries))

  write.csv(res_CCM, paste0(dir_out, "csv/02-res_CCM.csv"), row.names = FALSE)

} else {
  res_CCM = read.csv(paste0(dir_out, "csv/02-res_CCM.csv"))
}


# Test causality ----------------------------------------------------------

# * * * Assess causality * * *

cat("-> assess the causality\n")

res_causality = data.frame()

for (this_id in all_ids) {
  for ( i in 1:length(list_of_causality_tested)) {
    this_res_CCM = res_CCM %>% 
      filter(id_timeseries == this_id & var_target == list_of_causality_tested[[i]][1] & 
               var_lib == list_of_causality_tested[[i]][2]) %>% 
      pivot_wider(names_from = label, values_from = c(median, q5, q95)) %>% 
      arrange(LibSize)
    
    this_rho_simplex = res_E_opti %>% 
      filter(id_timeseries == this_id & variable == list_of_causality_tested[[i]][2]) %>%
      pull(rho_opti)
    pos_rho_simplex = this_rho_simplex > 0
    
    test_above_surr = sum(this_res_CCM[["median_Original"]] > this_res_CCM[["q95_Surrogates"]]) / 
      nrow(this_res_CCM) >= 0.9
    
    tmp_kendall = MannKendall(this_res_CCM[["median_Original"]])
    test_kendall_tau = tmp_kendall$tau[1] > 0 & tmp_kendall$sl[1] < 0.05
    
    res_causality = res_causality %>% rbind(
      data.frame(
        id_timeseries = this_id,
        var_cause = list_of_causality_tested[[i]][1],
        var_consequence = list_of_causality_tested[[i]][2],
        causality = test_above_surr & test_kendall_tau,
        rho_simplex_cons = this_rho_simplex,
        positive_rho_simplex_cons = pos_rho_simplex,
        test_above_surr = test_above_surr,
        test_kendall_tau = test_kendall_tau,
        kendall_tau = tmp_kendall$tau[1],
        kendall_pval = tmp_kendall$sl[1],
        rho_max = max(this_res_CCM[["median_Original"]], na.rm = TRUE)
      )
    )

  }
}

res_causality = res_causality %>% 
  left_join(info_stocks, by = c("id_timeseries" = name_id_timeseries)) %>% 
  mutate(causality = factor(ifelse(!positive_rho_simplex_cons, "Not relevant",
                                   ifelse(causality, "Yes", "No") ),
                            levels = c("Yes", "No", "Not relevant")))

write.csv(res_causality, paste0(dir_out, "csv/03-res_causality.csv"), row.names = FALSE)


# * * * Plot causality * * *

if (identical(list_of_causality_tested, list(c("sst.z", "prodbest.divTB"),
                                             c("UdivUmsypref", "prodbest.divTB"),
                                             c("prodbest.divTB", "sst.z"),
                                             c("prodbest.divTB", "UdivUmsypref")) ) ) {
  order_rel = c("HRate\ncausing\nSProd", "SProd\ncausing\nHRate", "SST\ncausing\nSProd", "SProd\ncausing\nSST")
  x_positions <- c(
    "HRate\ncausing\nSProd" = 1,
    "SProd\ncausing\nHRate" = 2,
    "SST\ncausing\nSProd"   = 3.7,  # Gap after x=2
    "SProd\ncausing\nSST"   = 4.7
  )
} else {
  order_rel = list_of_causality_tested %>%
    map_chr(~ paste0(labels_of_variables[[.x[1]]], "\ncausing\n", labels_of_variables[[.x[2]]]))
  x_positions <- setNames(1:length(order_rel), order_rel)
}


tmp = res_causality %>% 
  mutate(rel = paste0(labels_of_variables[var_cause], "\ncausing\n", labels_of_variables[var_consequence])) %>%
  mutate(rel = factor(rel, levels = unique(order_rel))) %>%
  group_by(rel, causality) %>%
  summarise(n = n(), .groups = "drop")

tmp$percentage = NA
for (i in 1:nrow(tmp)) {
  tmp[i, "percentage"] = 100 * tmp$n[i] / 
    tmp %>% filter( (rel == tmp$rel[i]) & (causality %in% c("Yes", "No")) ) %>% 
    pull(n) %>% sum()
}

p.fig4a = tmp %>% filter(causality %in% c("Yes", "No")) %>% 
  mutate(x_pos = x_positions[as.character(rel)]) %>% 
  ggplot(aes(x = x_pos, y = n, fill = causality)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), color = "#444444") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), color = "#444444", 
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) +
  scale_fill_manual(values = c("Yes" = "#97d89a", "No" = "#de7371", "Not relevant" = "#999999")) +
  labs(title = "Causality tests results", x = "Causal relationship", y = "Count", fill = "Causality?") +
  theme_light() + 
  # theme(axis.text.x = element_text(angle = 45, hjust = .7, vjust = .7)) + 
  annotate("text", x = -Inf, y = Inf, label = "(a)", vjust = 0.7, hjust = 2, size = 5, fontface = "bold") + 
  # annotate("text", x = Inf, y = Inf, label = "(a)", vjust = 1.5, hjust = -1.5, size = 5, fontface = "bold") + 
  coord_cartesian(clip = "off") + 
  scale_x_continuous(breaks = x_positions, labels = names(x_positions), limits = c(min(x_positions)-0.5, max(x_positions)+0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

if (!plots_with_titles) p.fig4a = p.fig4a + labs(title = NULL, subtitle = NULL)
print(p.fig4a)

for (typ in types_plots) {
  ggsave(p.fig4a, filename = paste0(dir_out, typ, "/fig4a-causality_tests_results.", typ),
         device = typ, width = 5.5, height = 4)
}


if (identical(list_of_causality_tested, list(c("sst.z", "prodbest.divTB"),
                                             c("UdivUmsypref", "prodbest.divTB"),
                                             c("prodbest.divTB", "sst.z"),
                                             c("prodbest.divTB", "UdivUmsypref")) ) ) {
  tmp_b = res_causality %>% 
    filter((var_cause == "sst.z" & var_consequence == "prodbest.divTB") |
             (var_cause == "UdivUmsypref" & var_consequence == "prodbest.divTB")) %>%
    mutate(rel = paste0(labels_of_variables[var_cause], "\ncausing\n", labels_of_variables[var_consequence])) %>%
    mutate(rel = factor(rel, levels = unique(order_rel))) %>%
    dplyr::select(id_timeseries, rel, causality) %>%
    mutate(causality = ifelse(causality %in% c("No", "Not relevant"), 
                              "No or Not relevant", as.character(causality))) %>%
    pivot_wider(names_from = rel, values_from = causality) %>% 
    mutate(group = case_when(
      (`SST\ncausing\nSProd` == "Yes" & `HRate\ncausing\nSProd` == "Yes") ~ "Both",
      (`SST\ncausing\nSProd` == "Yes" & `HRate\ncausing\nSProd` == "No or Not relevant") ~ "Only SST",
      (`SST\ncausing\nSProd` == "No or Not relevant" & `HRate\ncausing\nSProd` == "Yes") ~ "Only HRate",
      (`SST\ncausing\nSProd` == "No or Not relevant" & `HRate\ncausing\nSProd` == "No or Not relevant") ~ "None",
      TRUE ~ "NA"
    )) %>%
    group_by(group) %>%
    summarize(n = n()) %>% 
    mutate(group = factor(group, levels = c("Both", "Only HRate", "Only SST", "None") )) %>% 
    arrange(group) %>%
    mutate(prop = 100 * n / sum(n))
  
  p.fig4b = tmp_b %>% 
    ggplot(aes(x = "", y = -prop, fill = group)) +
    geom_bar(stat = "identity", width = 1, color = "#444444", alpha=0.7) +
    coord_polar("y", start = -pi/2) +
    geom_text(aes(x = 1.22, label = paste0(round(prop, 1), "%\n(", n, " stocks)")), 
              position = position_stack(vjust = 0.5), size = 3.3) +
    labs(title = "Causality tests results", fill = "Causality?") +
    theme_void() +
    scale_fill_brewer(palette = "Dark2") + 
    theme(legend.position = "right") +
    annotate("text", x = -Inf, y = Inf, label = "(b)", vjust = -10, hjust = 7, 
             size = 5, fontface = "bold")
  
  if (!plots_with_titles) p.fig4b = p.fig4b + labs(title = NULL, subtitle = NULL)
  print(p.fig4b)
  
  for (typ in types_plots) {
    ggsave(p.fig4b, filename = paste0(dir_out, typ, "/fig4b-causality_tests_results.", typ),
           device = typ, width = 5.5, height = 4)
  }
}


print(
  res_causality %>% mutate(rel = paste0(var_cause, " to ", var_consequence)) %>% 
    filter(causality != "Not relevant") %>% 
    group_by(rel) %>% summarize(n = n())
)





# Strength of causality ---------------------------------------------------



cat(" * * * Strength of the causality * * *\n")
cat("-> assess the strength of the causality\n")


if (!import_smap) {
  res_smap = data.frame()
  
  pb = progress::progress_bar$new(format = "[:bar] :percent eta: :eta", total = length(all_ids) * length(list_of_causality_tested))
  
  for (this_id in all_ids) {
    for ( i in 1:length(list_of_causality_tested) ) {
      
      this_var_cause = list_of_causality_tested[[i]][1] # target in the CCM, but driver/lib in the S-map
      this_var_consequence = list_of_causality_tested[[i]][2] # lib in the CCM, but target in the S-map
      
      this_causality = res_causality %>% 
        filter(id_timeseries == this_id, 
               var_cause == this_var_cause, var_consequence == this_var_consequence) %>%
        pull(causality)
      
      if (this_causality != "Yes") {
        pb$tick()
        next
      } # If the causality is not significant, we skip this pair
      
      this_E = res_E_opti %>% 
        filter(id_timeseries == this_id, variable == this_var_consequence) %>%
        pull(E_opti)
      
      
      # * * * We build the embedding space of dimension E * * *
      
      # including the cause, the consequence, eventually other significant drivers and the lags of the consequence
      
      this_df = data.frame(time = df[df[[name_id_timeseries]] == this_id, name_time])
      
      # We scale the variables
      this_cause_vect_scaled = scale(df[df[[name_id_timeseries]] == this_id, this_var_cause])[,1]
      this_consequence_vect_scaled = scale(df[df[[name_id_timeseries]] == this_id, this_var_consequence])[,1]
      
      # First dimension = consequence at t
      this_df[[paste0(this_var_consequence, "_t")]] = this_consequence_vect_scaled
      
      # Second dimension = cause at t
      this_df[[paste0(this_var_cause, "_t")]] = this_cause_vect_scaled
      
      # Fill with other drivers if E > 2
      if (this_E > 2) { # If we have enough space in the embedding space
        sub_df_these_other_drivers = res_causality %>% 
          filter(id_timeseries == this_id, 
                 var_cause != this_var_cause, var_consequence == this_var_consequence, 
                 causality == "Yes")
        if (nrow(sub_df_these_other_drivers) > 0) { # If there are other drivers
          these_other_drivers = sub_df_these_other_drivers %>%
            arrange(desc(rho_max)) %>% # sort the drivers by max_rho
            pull(var_cause)
          
          these_other_drivers = these_other_drivers[1:min(this_E-2, length(these_other_drivers))]
          
          # Add the drivers
          if (length(these_other_drivers) == 1) { # if only one driver (to avoid the problems while selecting elements in a single element vector)
            this_df[[paste0(these_other_drivers, "_t")]] = 
              scale(df[df[[name_id_timeseries]] == this_id, these_other_drivers])[,1]
          } else {
            for (k in 1:length(these_other_drivers)) {
              this_df[[paste0(these_other_drivers[k], "_t")]] = 
                scale(df[df[[name_id_timeseries]] == this_id, these_other_drivers[k]])[,1]
            }
          }
        }
      }
      
      # Fill with lags of the consequence if the embedding space is not full
      if (this_E - this_df %>% dplyr::select(-time) %>% ncol() > 0) {
        for (k in 1:( this_E - this_df %>% dplyr::select(-time) %>% ncol() )) {
          this_df[[paste0(this_var_consequence, "_t_m", k)]] = lag(this_consequence_vect_scaled, k)
        }
      }
      
      this_df = this_df %>% na.omit()
      
      cols_for_strength = this_df %>% dplyr::select(-time) %>% colnames()
      
      # * * * Apply the S-map * * *
      
      this_smap = data.frame()
      for (this_theta in seq(0, 20, 0.2)) {
        out_smap = SMap(dataFrame = this_df, 
                        lib = paste0("1 ", nrow(this_df)), pred = paste0("1 ", nrow(this_df)), 
                        E = this_E, Tp = 1, knn = 0, tau = 0, theta = this_theta, exclusionRadius = 0, 
                        columns = cols_for_strength, target = paste0(this_var_consequence, "_t"), 
                        embedded = TRUE, showPlot = FALSE, noTime = FALSE)
        
        forecasting_skill = cor(out_smap[["predictions"]]$Observations, 
                                out_smap[["predictions"]]$Predictions, use = "complete.obs")
        
        tmp = out_smap[["coefficients"]]
        colnames(tmp)[colnames(tmp) == paste0("∂", this_var_consequence, "_t", 
                                              "/∂", this_var_cause, "_t")] = "strength"
        
        this_tmp_smap = tmp %>% dplyr::select(time, strength) %>% 
          mutate(rho = forecasting_skill, 
                 var_cause = this_var_cause, var_consequence = this_var_consequence, 
                 id_timeseries = this_id, theta = this_theta) %>% 
          left_join(out_smap[["predictions"]], by = "time")
        
        this_smap = this_smap %>% rbind(this_tmp_smap)
      }
      
      this_smap = this_smap %>% 
        filter(rho == max(rho, na.rm = TRUE))
      
      res_smap = res_smap %>% rbind(this_smap)
      
      pb$tick()
    }
  }
  
  res_smap = res_smap %>% 
    left_join(info_stocks, by = c("id_timeseries" = name_id_timeseries))
  
  write.csv(res_smap, paste0(dir_out, "csv/04-res_smap.csv"), row.names = FALSE)
  
} else {
  res_smap = read.csv(paste0(dir_out, "csv/04-res_smap.csv"))
}



# Test the s-map ----------------------------------------------------------


cat("-> test the strength of the causality\n")

pb = progress::progress_bar$new(format = "[:bar] :percent eta: :eta", total = length(all_ids) * length(list_of_causality_tested))

res_test_smap = data.frame()
for (this_id in all_ids) {
  for ( i in 1:length(list_of_causality_tested)) {
    
    this_var_cause = list_of_causality_tested[[i]][1]
    this_var_consequence = list_of_causality_tested[[i]][2]
    
    this_smap = res_smap %>% 
      filter(id_timeseries == this_id, var_cause == this_var_cause,
             var_consequence == this_var_consequence)

    if (nrow(this_smap) == 0) {
      pb$tick()
      next
    }
    
    # * * * Tests on the strength * * *
    
    # Mean
    this_mean_strength = mean(this_smap$strength, na.rm = TRUE)
    # T-test
    this_t_test = t.test(this_smap$strength)
    this_t_test_stat = this_t_test$statistic
    this_t_test_p_val = this_t_test$p.value
    # Trend
    this_lm_trend = lm(strength ~ time, data = this_smap)
    this_trend_intercept = coef(this_lm_trend)[1]
    this_trend_slope = coef(this_lm_trend)[2]
    this_trend_p_val = summary(this_lm_trend)$coefficients[2,4]
    this_trend_R2 = summary(this_lm_trend)$r.squared
    
    res_test_smap = res_test_smap %>% rbind(
      data.frame(
        id_timeseries = this_id,
        var_cause = this_var_cause,
        var_consequence = this_var_consequence,
        rho = this_smap$rho[1],
        theta = this_smap$theta[1],
        mean_strength = this_mean_strength,
        t_test_statistic = this_t_test_stat,
        t_test_p_val = this_t_test_p_val,
        trend_intercept = this_trend_intercept,
        trend_slope = this_trend_slope,
        trend_p_val = this_trend_p_val,
        trend_R2 = this_trend_R2
      )
    )
    
    # * * * Plot the S-map * * *
    
    if (this_id %in% ids_single_plots) {
      
      # # Time series prediction S-map
      # 
      # label_colors = c("#0072B2", "#D55E00", "#009E73")
      # names(label_colors) = c(
      #   paste0(labels_of_variables[this_var_consequence], "\n(observations)"),
      #   paste0(labels_of_variables[this_var_consequence], "\n(predictions)"),
      #   paste0(labels_of_variables[this_var_cause], "\n(observations)")
      # )
      # this_df_ts = df[df[[name_id_timeseries]] == this_id, ]
      # colnames(this_df_ts)[colnames(this_df_ts) == name_time] = "time"
      # colnames(this_df_ts)[colnames(this_df_ts) == this_var_cause] = "cause"
      # this_df_ts$cause = scale(this_df_ts$cause)[,1]
      # this_df_ts = this_df_ts %>% dplyr::select(time, cause)
      # 
      # p.figAdd3a = this_smap %>% na.omit() %>% 
      #   mutate(rib_min = Predictions - sqrt(Pred_Variance),
      #          rib_max = Predictions + sqrt(Pred_Variance)) %>%
      #   left_join(this_df_ts, by = "time") %>% 
      #   ggplot(aes(x = time)) +
      #   geom_line(aes(y = Observations, color = paste0(labels_of_variables[this_var_consequence], "\n(observations)"))) +
      #   geom_line(aes(y = Predictions, color = paste0(labels_of_variables[this_var_consequence], "\n(predictions)"))) +
      #   geom_ribbon(aes(ymin = rib_min, ymax = rib_max, fill = "Std deviation of predictions"), alpha = 0.2) +
      #   geom_line(aes(y = cause, color = paste0(labels_of_variables[this_var_cause], "\n(observations)") )) +
      #   scale_color_manual(values = label_colors)+
      #   scale_fill_manual(values = c("Std deviation of predictions" = "#D55E00")) +
      #   labs(title = paste0("S-map for ", this_id, ": ", labels_of_variables[this_var_cause], 
      #                       " to ", labels_of_variables[this_var_consequence]),
      #        x = "Time", y = "Scaled value", color = "Timeseries", fill = "Ribbon") +
      #   theme_light() + 
      #   theme(legend.position = "bottom", legend.text = element_text(size = 6),
      #         legend.title = element_text(size = 8))
      # if (!plots_with_titles) p.figAdd3a = p.figAdd3a + labs(title = NULL, subtitle = NULL)
      # print(p.figAdd3a)
      # 
      # # Observations vs predictions S-map
      # 
      # p.figAdd3b = ggplot(this_smap %>% na.omit(), aes(x = Observations, y = Predictions)) +
      #   geom_point() +
      #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#666") +
      #   labs(title = paste0("S-map for ", this_id, ": ", labels_of_variables[this_var_cause], 
      #                       " to ", labels_of_variables[this_var_consequence]),
      #        x = "Observations", y = "Predictions") +
      #   theme_light()
      # if (!plots_with_titles) p.figAdd3b = p.figAdd3b + labs(title = NULL, subtitle = NULL)
      # print(p.figAdd3b)
      
      # S-map coefficients over time
      
      # annot = paste0("Mean strength = ", sprintf("%4.2e", this_mean_strength), "    |    ",
      #                "P-value t-test = ", sprintf("%4.2e", this_t_test_p_val), "\n",
      #                "Linear slope = ", sprintf("%4.2e", this_trend_slope), "    |    ",
      #                "P-value slope = ", sprintf("%4.2e", this_trend_p_val) )
      annot = paste0("Mean strength = ", sprintf("%4.2e", this_mean_strength), " (p-val = ", sprintf("%4.2e", this_t_test_p_val), ")\n",
                     "Linear slope = ", sprintf("%4.2e", this_trend_slope), " (p-val = ", sprintf("%4.2e", this_trend_p_val), ")" )
      txt_rel = paste0(labels_of_variables[this_var_cause], " causing ", labels_of_variables[this_var_consequence])
      color_mean = ifelse(this_t_test_p_val >= 0.05, "#444444", 
                          ifelse(this_mean_strength > 0, "royalblue", "#ef8004"))
      color_slope = ifelse(this_trend_p_val >= 0.05, "#444444", 
                           ifelse(this_trend_slope > 0, "royalblue", "#ef8004"))
      
      p.fig3 = ggplot(this_smap %>% na.omit(), aes(x = time, y = strength)) + 
        geom_smooth(method = "lm", color = color_slope, formula = y ~ x) +
        geom_line() + geom_point() +
        geom_hline(yintercept = this_mean_strength, linetype = "dashed", color = color_mean) +
        annotate("text", x = min(this_smap$time, na.rm=T), y = mean(this_smap$strength, na.rm=T), hjust = 0, vjust = -0.5, label = "Mean", color = color_mean) +
        annotate("text", x = Inf, y = Inf, vjust = 2.7, hjust = 1.05, size = 3.5, label = txt_rel) +
        geom_label(aes(x = x, y = y, label = txt), data = data.frame(x = -Inf, y = -Inf, txt = annot), 
                   hjust = -0.03, vjust = -0.3, size = 3.5, color = "#444444",
                   fill = "white", label.padding = unit(0.4, "lines")) +
        labs(title = paste0("S-map for ", this_id, ": ", labels_of_variables[this_var_cause], 
                            " to ", labels_of_variables[this_var_consequence]),
             # caption = annot,
             x = "Year", y = "S-map coefficients") +
        theme_light()
      if (!plots_with_titles) p.fig3 = p.fig3 + labs(title = NULL, subtitle = NULL)
      if (unique(this_smap$var_cause) == "sst.z" & 
          unique(this_smap$var_consequence) == "prodbest.divTB") {
        p.fig3 = p.fig3 +
          annotate("text", x = -Inf, y = Inf, vjust = 1, hjust = 2.7, size = 5,
                   label = "(a)", fontface = "bold") +
          coord_cartesian(clip = "off")
      } else if (unique(this_smap$var_cause) == "UdivUmsypref" & 
                 unique(this_smap$var_consequence) == "prodbest.divTB") {
        p.fig3 = p.fig3 +
          annotate("text", x = -Inf, y = Inf, vjust = 1, hjust = 2.03, size = 5,
                   label = "(b)", fontface = "bold") +
          coord_cartesian(clip = "off")
      }
      print(p.fig3)
      
      
      this_df = df %>% 
        filter(.data[[name_id_timeseries]] == this_id) %>%
        dplyr::select(all_of(c(name_time, this_var_cause, this_var_consequence))) %>% 
        rename(t = name_time, cause = this_var_cause, consequence = this_var_consequence)
      # p.fig3.bis = ggplot(this_df, aes(cause, consequence, color = t)) +
      #   geom_point() + 
      #   scale_color_viridis_c(option = "cividis") +
      #   geom_smooth(method = "lm", color = "#444444", formula = y ~ x, se = FALSE,
      #               linetype = "dashed", linewidth = 0.5) +
      #   labs(x = paste0(labels_of_variables[this_var_cause], " (observations)"), 
      #        y = paste0(labels_of_variables[this_var_consequence], " (observations)"),
      #        color = "Year") +
      #   theme_light()
      # if (!plots_with_titles) p.fig3.bis = p.fig3.bis + labs(title = NULL, subtitle = NULL)
      # if (unique(this_smap$var_cause) == "sst.z" & 
      #     unique(this_smap$var_consequence) == "prodbest.divTB") {
      #   p.fig3.bis = p.fig3.bis +
      #     annotate("text", x = -Inf, y = Inf, vjust = 1, hjust = 1.7, size = 6.5,
      #              label = "(a)", fontface = "bold") +
      #     coord_cartesian(clip = "off")
      # } else if (unique(this_smap$var_cause) == "UdivUmsypref" & 
      #            unique(this_smap$var_consequence) == "prodbest.divTB") {
      #   p.fig3.bis = p.fig3.bis +
      #     annotate("text", x = -Inf, y = Inf, vjust = 1, hjust = 1.7, size = 6.5,
      #              label = "(b)", fontface = "bold") +
      #     coord_cartesian(clip = "off")
      # }
      # print(p.fig3.bis)
      
      cat(paste0(
        "\nCorrelation between ", labels_of_variables[this_var_cause], " and ", 
        labels_of_variables[this_var_consequence], " for ", this_id, ": ", 
        cor(this_df$cause, this_df$consequence, use = "complete.obs"), "\n"))
                 
      
      for (typ in types_plots) {
        # ggsave(p.figAdd3a, filename = paste0(dir_out, typ, "/figAdd3a-smap_predictions-", this_id, "-", this_var_cause, "_to_", this_var_consequence, ".", typ),
        #        device = typ, width = 6, height = 4)
        # ggsave(p.figAdd3b, filename = paste0(dir_out, typ, "/figAdd3b-smap_observations_vs_predictions-", this_id, "-", this_var_cause, "_to_", this_var_consequence, ".", typ),
        #        device = typ, width = 6, height = 4)
        ggsave(p.fig3, filename = paste0(dir_out, typ, "/fig3-smap_strength_of_influence-", this_id, "-", this_var_cause, "_to_", this_var_consequence, ".", typ),
               device = typ, width = 6, height = 4)
        # ggsave(p.fig3.bis, filename = paste0(dir_out, typ, "/fig3_bis-correlation_timeseries-", this_id, "-", this_var_cause, "_to_", this_var_consequence, ".", typ),
        #        device = typ, width = 6, height = 4)
      }
    }
    
    pb$tick()
  }
}

res_test_smap = res_test_smap %>% 
  mutate(mean = ifelse(t_test_p_val <= 0.05, ifelse(mean_strength > 0, "Positive", "Negative"), "Not significant"),
         trend = ifelse(trend_p_val <= 0.05, ifelse(trend_slope > 0, "Positive", "Negative"), "Not significant"))

write.csv(res_test_smap, paste0(dir_out, "csv/05-res_test_smap.csv"), row.names = FALSE)


# Export results from CCM and S-map in a single csv

res_causality %>% 
  left_join(res_test_smap, 
            by = c("id_timeseries", "var_cause", "var_consequence")) %>% 
  dplyr::select(id_timeseries, stocklong, scientificname, var_cause, var_consequence, 
                causality, test_above_surr, kendall_tau, kendall_pval,
                mean_strength, t_test_p_val, trend_slope, trend_p_val, trend_R2) %>% 
  mutate(var_cause = labels_of_variables[var_cause],
         var_consequence = labels_of_variables[var_consequence]) %>%
  rename(
    stockid = id_timeseries,
    CCM_test_above_surrogates = test_above_surr,
    CCM_kendall_tau = kendall_tau,
    CCM_kendall_pval = kendall_pval,
    S_map_mean_coef = mean_strength,
    S_map_mean_ttest_pval = t_test_p_val,
    S_map_trend_slope = trend_slope,
    S_map_trend_pval = trend_p_val,
    S_map_trend_R2 = trend_R2
  ) %>%
  write.csv(paste0(dir_out, "csv/all_res.csv"), row.names = FALSE)


# Plot summary smap
library(patchwork)

tmp = res_test_smap %>% 
  filter((var_cause == list_of_causality_tested[[1]][1] & 
          var_consequence == list_of_causality_tested[[1]][2]) |
         (var_cause == list_of_causality_tested[[2]][1] & 
          var_consequence == list_of_causality_tested[[2]][2])) %>% 
  group_by(var_cause, var_consequence, mean, trend) %>%
  summarise(n = n(), .groups = "drop") %>% 
  mutate(rel = paste0(labels_of_variables[var_cause], " causing ", labels_of_variables[var_consequence])) %>%
  mutate(rel = factor(rel, levels = gsub("\n", " ", order_rel)[c(1,3)])) %>%
  
  mutate(trend = factor(ifelse(trend == "Positive", "Increasing", 
                               ifelse(trend == "Negative", "Decreasing", "Stable")),
                        levels = c("Decreasing", "Stable", "Increasing")),
         mean = factor(ifelse(mean == "Not significant", "Neutral", mean),
                       levels = c("Negative", "Neutral", "Positive"))) %>%
  # Add row if there is no data
  complete(rel, mean, trend, fill = list(n = 0))

p.fig5a = tmp %>% 
  filter(rel == "HRate causing SProd") %>% 
  ggplot(aes(x = trend, y = mean, fill = n)) +
  # facet_wrap(~rel, scales = "free") +
  geom_tile() + geom_text(aes(label = n), vjust = 0.5, size = 4) +
  scale_fill_gradient(low = "#ffffff", high = "royalblue", limits = c(0, max(tmp$n))) +
  guides(fill = "none") +
  labs(title = "HRate causing SProd", fill = "Number of\nstocks", 
       x = "Slope of S-map coefficients", y = "Mean S-map coefficient") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.text = element_markdown(size = 8), 
        plot.title = element_text(size=16, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  plot_annotation(tag_levels = list("(a)")) +
  theme(plot.tag = element_text(size = 12, face = "bold"), plot.tag.position = c(0.03, 0.98))
print(p.fig5a)

for (typ in types_plots) {
  ggsave(p.fig5a, filename = paste0(dir_out, typ, "/fig5a-strength_summary.", typ),
         device = typ, width = 4, height = 4)
}

p.fig5b = tmp %>% 
  filter(rel == "SST causing SProd") %>% 
  ggplot(aes(x = trend, y = mean, fill = n)) +
  # facet_wrap(~rel, scales = "free") +
  geom_tile() + geom_text(aes(label = n), vjust = 0.5, size = 4) +
  scale_fill_gradient(low = "#ffffff", high = "royalblue", limits = c(0, max(tmp$n))) +
  guides(fill = "none") +
  labs(title = "SST causing SProd", fill = "Number of\nstocks", 
       x = "Slope of S-map coefficients", y = "Mean S-map coefficient") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.text = element_markdown(size = 8), 
        plot.title = element_text(size=16, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  plot_annotation(tag_levels = list("(b)")) +
  theme(plot.tag = element_text(size = 12, face = "bold"), plot.tag.position = c(0.03, 0.98))
print(p.fig5b)

for (typ in types_plots) {
  ggsave(p.fig5b, filename = paste0(dir_out, typ, "/fig5b-strength_summary.", typ),
         device = typ, width = 4, height = 4)
}


# * * * Link with time series duration * * *

tmp = res_causality %>% 
  # mutate(causality = ifelse(causality, "Causality", "No causality")) %>% 
  left_join(df %>% group_by_at(name_id_timeseries) %>% 
              summarise(duration = n()), by = c("id_timeseries" = name_id_timeseries) ) %>%
  left_join(res_test_smap, by = c("id_timeseries", "var_cause", "var_consequence")) %>%
  dplyr::select(id_timeseries, var_cause, var_consequence, duration, causality, mean, trend) %>% 
  mutate(mean = ifelse(is.na(mean), "No causality", mean),
         trend = ifelse(is.na(trend), "No causality", trend), 
         var_cause = labels_of_variables[var_cause],
         var_consequence = labels_of_variables[var_consequence]) %>%
  pivot_longer(cols = c("causality", "mean", "trend"), 
               names_to = "test", values_to = "value") %>% 
  
  group_by(id_timeseries, var_cause, var_consequence) %>% 
  mutate(var_tmp = ("No" %in% value) | ("Not relevant" %in% value)) %>% 
  ungroup() %>% 
  filter(!( (test %in% c("mean", "trend")) & var_tmp ) ) %>% 
  
  mutate(rel = factor(paste0(var_cause, " to ", var_consequence), levels = order_rel),
         test = case_when(test == "causality" ~ "Causality",
                          test == "mean" ~ "Sign",
                          test == "trend" ~ "Trend"),
         col = case_when(value == "Yes" ~ "#97d89a", 
                         value == "No" ~ "#de7371", 
                         value == "Not relevant" ~ "#444444", 
                         value == "No causality" ~ "#de7371", 
                         value == "Positive" ~ "royalblue", 
                         value == "Not significant" ~ "#666666", 
                         value == "Negative" ~ "#ef8004") ) %>% 
  group_by(test, value) %>% 
  mutate(n_all = n()) %>% 
  mutate(value_all = paste0(value, "\n(n=", n_all, ")")) %>% 
  ungroup() %>%
  group_by(rel, test, value) %>%
  mutate(n_rel = n()) %>% 
  mutate(value_rel = paste0(value, "\n(n=", n_rel, ")")) %>% 
  ungroup()

# Comparison of the effects
library(emmeans)
tmp_lm1 = lm(duration ~ value_all, data = tmp %>% filter(test == "Causality"))
tmp_emm1 = emmeans(tmp_lm1, pairwise ~ value_all, adjust = "bonferroni")
cat("\nEffect of the causality assessed on the length of the timeseries:\n")
tmp_emm1$contrasts

tmp_lm2 = lm(duration ~ value_all, data = tmp %>% filter(test == "Sign"))
tmp_emm2 = emmeans(tmp_lm2, pairwise ~ value_all, adjust = "bonferroni")
cat("\nEffect of the sign assessed on the length of the timeseries:\n")
tmp_emm2$contrasts

tmp_lm3 = lm(duration ~ value_all, data = tmp %>% filter(test == "Trend"))
tmp_emm3 = emmeans(tmp_lm3, pairwise ~ value_all, adjust = "bonferroni")
cat("\nEffect of the trend assessed on the length of the timeseries:\n")
tmp_emm3$contrasts






p.figAdd4a = ggplot(tmp, aes(x = value_all, y = duration)) + 
  facet_wrap(~ test, scales = "free") +
  geom_boxplot(aes(fill = col)) + scale_fill_identity() +
  labs(title = "Duration of time series for each causality test result",
       x = "Results of the tests", y = "Duration of time series",
       fill = "Results of the tests") +
  theme_light() + theme(axis.text.x = element_text(angle = 45, hjust = 0.9))
if (!plots_with_titles) p.figAdd4a = p.figAdd4a + labs(title = NULL, subtitle = NULL)
print(p.figAdd4a)

for (typ in types_plots) {
  ggsave(p.figAdd4a, filename = paste0(dir_out, typ, "/figAdd4a-effect_duration_on_tests-all.", typ),
         device = typ, width = 7, height = 3)
}


p.figAdd4b = ggplot(tmp, aes(x = value_rel, y = duration)) +
  facet_wrap(rel ~ test, scales = "free", ncol=3) +
  geom_boxplot(aes(fill = col)) + scale_fill_identity() +
  labs(title = "Duration of time series for each causality test result",
       x = "Results of the tests", y = "Duration of time series",
       fill = "Results of the tests") +
  theme_light() + theme(axis.text.x = element_text(angle = 45, hjust = 0.9))
if (!plots_with_titles) p.figAdd4b = p.figAdd4b + labs(title = NULL, subtitle = NULL)
print(p.figAdd4b)

for (typ in types_plots) {
  ggsave(p.figAdd4b, filename = paste0(dir_out, typ, "/figAdd4b-effect_duration_on_tests-per_rel.", typ),
         device = typ, width = 7, height = 3 * length(unique(tmp$rel)))
}



# * * * Links between causalities * * *
if (identical(list_of_causality_tested, list(c("sst.z", "prodbest.divTB"),
                                             c("UdivUmsypref", "prodbest.divTB"),
                                             c("prodbest.divTB", "sst.z"),
                                             c("prodbest.divTB", "UdivUmsypref")) ) ) {
  df_biplot = left_join(
    res_causality %>% 
      filter(var_cause %in% c("sst.z", "UdivUmsypref"), var_consequence %in% "prodbest.divTB"), 
    res_test_smap %>% 
      filter(var_cause %in% c("sst.z", "UdivUmsypref"), var_consequence %in% "prodbest.divTB"), 
    by = c("id_timeseries", "var_cause", "var_consequence")
  ) %>%
    mutate(label_all = case_when(
      mean == "Negative" & trend == "Negative" ~ "Negative decreasing",
      mean == "Negative" & trend == "Not significant" ~ "Negative stable",
      mean == "Negative" & trend == "Positive" ~ "Negative increasing",
      mean == "Positive" & trend == "Negative" ~ "Positive decreasing",
      mean == "Positive" & trend == "Not significant" ~ "Positive stable",
      mean == "Positive" & trend == "Positive" ~ "Positive increasing",
      mean == "Not significant" & trend == "Negative" ~ "Neutral decreasing",
      mean == "Not significant" & trend == "Not significant" ~ "Neutral stable",
      mean == "Not significant" & trend == "Positive" ~ "Neutral increasing"
    )) %>% 
    mutate(var_cause = factor(var_cause, levels = c("UdivUmsypref", "sst.z"), labels = c("HRate", "SST")))
  
  
  # * * * New figure 5 * * *
  
  tmp_bi = df_biplot %>% 
    dplyr::select(id_timeseries, var_cause, causality, mean_strength, trend_slope) %>% 
    pivot_wider(names_from = var_cause, values_from = c(causality, mean_strength, trend_slope)) %>% 
    mutate(group = case_when(
      causality_HRate == "Yes" & causality_SST == "Yes" ~ "Both",
      causality_HRate == "Yes" & causality_SST == "No" ~ "Only HRate",
      causality_HRate == "No" & causality_SST == "Yes" ~ "Only SST",
      causality_HRate == "No" & causality_SST == "No" ~ "None"
    ))
  
  color_palette.group = brewer.pal(n = 4, name = "Dark2")
  names(color_palette.group) = c("Both", "Only HRate", "Only SST", "None")
  
  x_range_1 = range(tmp_bi$trend_slope_HRate, na.rm = TRUE)
  x_abs_bound_1 = max(-( x_range_1[1] - diff(x_range_1) * 0.1 ), x_range_1[2] + diff(x_range_1) * 0.1)
  y_range_1 = range(tmp_bi$mean_strength_HRate, na.rm = TRUE)
  y_abs_bound_1 = max(-( y_range_1[1] - diff(y_range_1) * 0.1 ), y_range_1[2] + diff(y_range_1) * 0.1)
  
  p.fig.5c.new = tmp_bi %>% filter(causality_HRate == "Yes") %>% 
    ggplot(aes(x = trend_slope_HRate, y = mean_strength_HRate, color = group)) +
    geom_point(size = 2, alpha = 0.8) +
    ggrepel::geom_text_repel(aes(label = id_timeseries), size = 2, max.overlaps = 15, alpha = 0.6) +
    coord_cartesian(clip = "off", xlim = c(-x_abs_bound_1, x_abs_bound_1), ylim = c(-y_abs_bound_1, y_abs_bound_1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#666") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#666") + 
    annotate("text", x = -Inf, y = Inf, vjust = 1, hjust = 1.9, size = 5, label = "(c)", fontface = "bold") +
    scale_color_manual(values = color_palette.group) +
    labs(x = "Slope of S-map coefficients (HRate causing SProd)",
         y = "Mean S-map coefficient (HRate causing SProd)", 
         color = "Causality") +
    theme_light()
  print(p.fig.5c.new)
  
  for (typ in types_plots) {
    ggsave(p.fig.5c.new, filename = paste0(dir_out, typ, "/fig5c_new-scatter_smap_HRate_causing_SProd.", typ),
           device = typ, width = 6, height = 4.5)
  }
  
  x_range_2 = range(tmp_bi$trend_slope_SST, na.rm = TRUE)
  x_abs_bound_2 = max(-( x_range_2[1] - diff(x_range_2) * 0.1 ), x_range_2[2] + diff(x_range_2) * 0.1)
  y_range_2 = range(tmp_bi$mean_strength_SST, na.rm = TRUE)
  y_abs_bound_2 = max(-( y_range_2[1] - diff(y_range_2) * 0.1 ), y_range_2[2] + diff(y_range_2) * 0.1)
  
  p.fig.5d.new = tmp_bi %>% filter(causality_SST == "Yes") %>% 
    ggplot(aes(x = trend_slope_SST, y = mean_strength_SST, color = group)) +
    geom_point(size = 2, alpha = 0.8) +
    ggrepel::geom_text_repel(aes(label = id_timeseries), size = 2, max.overlaps = 15, alpha = 0.6) +
    coord_cartesian(clip = "off", xlim = c(-x_abs_bound_2, x_abs_bound_2), ylim = c(-y_abs_bound_2, y_abs_bound_2)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#666") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#666") + 
    annotate("text", x = -Inf, y = Inf, vjust = 1, hjust = 2.3, size = 5, label = "(d)", fontface = "bold") +
    scale_color_manual(values = color_palette.group) +
    labs(x = "Slope of S-map coefficients (SST causing SProd)",
         y = "Mean S-map coefficient (SST causing SProd)", 
         color = "Causality") +
    theme_light()
  print(p.fig.5d.new)
  
  for (typ in types_plots) {
    ggsave(p.fig.5d.new, filename = paste0(dir_out, typ, "/fig5d_new-scatter_smap_SST_causing_SProd.", typ),
           device = typ, width = 6, height = 4.5)
  }
  
  
  
  # * * * Other figures * * * 
  
  color.palette.caus = c("Yes" = "#97d89a", "No" = "#de7371", "Not relevant" = "#444444")
  color.palette.all = c(
    "Negative decreasing" = "#8B2300",  # Darker orange-red for negative decreasing
    "Negative stable" = "#FF9933",      # Mid-tone orange for negative stable
    "Negative increasing" = "#FFCC99",  # Light orange for negative increasing
    
    "Positive decreasing" = "#003366",  # Dark blue for positive decreasing
    "Positive stable" = "#3399FF",      # Mid-tone blue for positive stable
    "Positive increasing" = "#99CCFF",  # Light blue for positive increasing
    
    "Neutral decreasing" = "#4C1130",   # Dark purple for neutral decreasing
    "Neutral stable" = "#996699",       # Mid-tone purple for neutral stable
    "Neutral increasing" = "#C99EC9"    # Light purple for neutral increasing
  )
  
  p.fig.Add7a = df_biplot %>%
    filter(causality == "Yes") %>%
    # dplyr::select(id_timeseries, var_cause, var_consequence, causality,
    #               mean_strength, t_test_statistic, t_test_p_val, trend_intercept,
    #               trend_slope, trend_p_val, trend_R2, mean, trend) %>%
    dplyr::select(id_timeseries, var_cause, trend_slope, mean) %>%
    mutate(mean = ifelse(mean == "Not significant", "Neutral", mean)) %>% 
    pivot_wider(names_from = var_cause, values_from = c(trend_slope, mean)) %>%
    ggplot(aes(x = trend_slope_HRate, y = trend_slope_SST)) +
    geom_point(aes(color = mean_HRate, shape = mean_SST), size = 3) +
    ggrepel::geom_text_repel(aes(label = id_timeseries), size = 3) + 
    scale_color_manual(values = c("Negative" = "#FF9933", "Neutral" = "#996699", "Positive" = "#3399FF"), 
                       name = "Sign mean\nHRate") +
    scale_shape_manual(values = c("Negative" = 17, "Neutral" = 16, "Positive" = 15),
                       name = "Sign mean\nSST") +
    labs(x = "Slope of S-map coefficients for HRate causing Productivity",
         y = "Slope of S-map coefficients for SST causing Productivity") +
    theme_light()
  print(p.fig.Add7a)
  for (typ in types_plots) {
    ggsave(p.fig.Add7a, filename = paste0(dir_out, typ, "/figAdd7a-scatter_slopes_means_both_causes.", typ),
           device = typ, width = 6, height = 5)
  }
    
  tmp_df_plot = df_biplot %>%
    filter(causality == "Yes") %>%
    dplyr::select(id_timeseries, var_cause, mean_strength, trend) %>%
    mutate(trend = ifelse(trend == "Positive", "Increasing", 
                          ifelse(trend == "Negative", "Decreasing", 
                                 ifelse(trend == "Not significant", "Stable", trend)))) %>%
    pivot_wider(names_from = var_cause, values_from = c(mean_strength, trend)) %>%
    na.omit()
  x_range = range(tmp_df_plot$mean_strength_HRate, na.rm = TRUE)
  x_abs_bound = max(-( x_range[1] - diff(x_range) * 0.1 ), x_range[2] + diff(x_range) * 0.1)
  y_range = range(tmp_df_plot$mean_strength_SST, na.rm = TRUE)
  y_abs_bound = max(-( y_range[1] - diff(y_range) * 0.1 ), y_range[2] + diff(y_range) * 0.1)
  p.fig.Add7b = ggplot(tmp_df_plot, aes(x = mean_strength_HRate, y = mean_strength_SST)) +
    geom_point(aes(color = trend_HRate, fill = trend_HRate, shape = trend_SST), size = 3) +
    ggrepel::geom_text_repel(aes(label = id_timeseries), size = 3, max.overlaps = 15) +
    coord_cartesian(xlim = c(-x_abs_bound, x_abs_bound), ylim = c(-y_abs_bound, y_abs_bound)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#666") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#666") +
    scale_color_manual(values = c("Decreasing" = "#FF9933", "Stable" = "#996699", "Increasing" = "#3399FF"), 
                       name = "Sign trend\nHRate") +
    scale_fill_manual(values = c("Decreasing" = "#FF9933", "Stable" = "#996699", "Increasing" = "#3399FF"), 
                       name = "Sign trend\nHRate") +
    scale_shape_manual(values = c("Decreasing" = 25, "Stable" = 16, "Increasing" = 24),
                       name = "Sign trend\nSST") +
    guides(shape = guide_legend(override.aes = list(fill = "black"))) + 
    labs(x = "Mean of S-map coefficients for HRate causing Productivity",
         y = "Mean of S-map coefficients for SST causing Productivity") +
    theme_light()
  print(p.fig.Add7b)
  for (typ in types_plots) {
    ggsave(p.fig.Add7b, filename = paste0(dir_out, typ, "/figAdd7b-scatter_slopes_means_both_causes.", typ),
           device = typ, width = 8, height = 7)
  }
  
  library(ggalluvial)
  
  p.fig7a = df_biplot %>% 
    dplyr::select(id_timeseries, var_cause, causality) %>% 
    ggplot(aes(x = var_cause, stratum = causality, alluvium = id_timeseries, y = 1, fill = causality)) +
    scale_x_discrete(expand = c(0.5, 0)) + 
    geom_flow() +
    geom_stratum(alpha = 0.5) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    scale_fill_manual(values = color.palette.caus) +
    scale_y_reverse() +
    labs(x = "Causal Variable", y = "Count", fill = "Causality") +
    theme_minimal() +
    theme(legend.position = "right", axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  print(p.fig7a)
  for (typ in types_plots) {
    ggsave(p.fig7a, filename = paste0(dir_out, typ, "/fig7a-biplot_causality_both_causes.", typ),
           device = typ, width = 7, height = 4)
  }
  
  p.fig7b = df_biplot %>% 
    filter(causality == "Yes") %>% 
    dplyr::select(id_timeseries, var_cause, label_all) %>% 
    ggplot(aes(x = var_cause, stratum = label_all, alluvium = id_timeseries, y = 1, fill = label_all)) +
    scale_x_discrete(expand = c(0.2, 0)) + 
    geom_flow() +
    geom_stratum(alpha = 0.5) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.2) +
    scale_fill_manual(values = color.palette.all) +
    labs(x = "Causal Variable", y = "Count", fill = "Sign and Trend") +
    theme_minimal() +
    theme(legend.position = "right", axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  print(p.fig7b)
  for (typ in types_plots) {
    ggsave(p.fig7b, filename = paste0(dir_out, typ, "/fig7b-biplot_sign_trend_both_causes.", typ),
           device = typ, width = 7, height = 4)
  }
}


# * * * Map of pie charts of causality * * *

for (k in 1:2) {
  
  this_cause = list_of_causality_tested[[k]][1]
  this_consequence = list_of_causality_tested[[k]][2]
  
  df_maps = res_causality %>% 
    filter(var_cause == this_cause, 
           var_consequence == this_consequence) %>%
    left_join(res_test_smap %>% 
                filter(var_cause == this_cause, 
                       var_consequence == this_consequence) %>% 
                dplyr::select(id_timeseries, mean, trend), by = "id_timeseries") %>% 
    dplyr::select(id_timeseries, region, causality, mean, trend) %>% 
    mutate(mean = ifelse(is.na(mean), "No causality", mean),
           trend = ifelse(is.na(trend), "No causality", trend))
  
  color.palette.caus = c("Yes" = "#97d89a", "No" = "#de7371", "Not relevant" = "#444444")
  p.fig6a = ggplot_map_piechart(
    # df_maps,
    df_maps %>% filter(causality != "Not relevant"),
    "id_timeseries",
    var_pie = "causality",
    label_var_pie = "Causality"
  ) + scale_fill_manual(values = color.palette.caus) + 
    # labs(title = paste0("From ", labels_of_variables[this_cause],
    #                     " to ", labels_of_variables[this_consequence], " - Causality")) + 
    annotate("text", x = -Inf, y = Inf, vjust = 2.2, hjust = -0.17, size = 3,
             label = paste0("From ", labels_of_variables[this_cause], " to ", labels_of_variables[this_consequence], " - Causality")) + 
    theme(plot.title = element_blank()) + 
    annotate("text", x = -Inf, y = Inf, label = ifelse(k == 1, "(b)", "(c)"), 
             vjust = 1.5, hjust = -0.1, size = 5, fontface = "bold")
  print(p.fig6a)
  for (typ in types_plots) {
    ggsave(p.fig6a, filename = paste0(dir_out, typ, "/fig4", ifelse(k == 1, "b", "c"), "-map_piechart-", 
                                      this_cause, "_to_", this_consequence, "-causality.", typ),
           device = typ, width = 8, height = 4)
  }
  
  
  
  color.palette.mean.trend = c("No causality" = "#de7371", "Positive" = "royalblue",
                                   "Not significant" = "#444444", "Negative" = "#ef8004")
  p.fig6b = ggplot_map_piechart(
    df_maps %>% filter(causality == "Yes"),
    "id_timeseries",
    var_pie = "mean",
    label_var_pie = "Mean effect"
  ) + scale_fill_manual(values = color.palette.mean.trend) + 
    labs(title = paste0("From ", labels_of_variables[this_cause],
                        " to ", labels_of_variables[this_consequence], " - Sign"))
  print(p.fig6b)
  for (typ in types_plots) {
    ggsave(p.fig6b, filename = paste0(dir_out, typ, "/fig6-map_piechart-", this_cause, 
                                      "_to_", this_consequence, "-sign.", typ),
           device = typ, width = 8, height = 4)
  }
  
  
  p.fig6c = ggplot_map_piechart(
    df_maps %>% filter(causality == "Yes"),
    "id_timeseries",
    var_pie = "trend",
    label_var_pie = "Trend of effect"
  ) + scale_fill_manual(values = color.palette.mean.trend) + 
    labs(title = paste0("From ", labels_of_variables[this_cause],
                        " to ", labels_of_variables[this_consequence], " - Trend"))
  print(p.fig6c)
  for (typ in types_plots) {
    ggsave(p.fig6c, filename = paste0(dir_out, typ, "/fig6-map_piechart-", this_cause, 
                                      "_to_", this_consequence, "-trend.", typ),
           device = typ, width = 8, height = 4)
  }
  
  
  df_maps_all = df_maps %>%
    filter(causality == "Yes") %>%
    mutate(label_all = case_when(
      mean == "Negative" & trend == "Negative" ~ "Negative decreasing",
      mean == "Negative" & trend == "Not significant" ~ "Negative stable",
      mean == "Negative" & trend == "Positive" ~ "Negative increasing",
      mean == "Positive" & trend == "Negative" ~ "Positive decreasing",
      mean == "Positive" & trend == "Not significant" ~ "Positive stable",
      mean == "Positive" & trend == "Positive" ~ "Positive increasing",
      mean == "Not significant" & trend == "Negative" ~ "Neutral decreasing",
      mean == "Not significant" & trend == "Not significant" ~ "Neutral stable",
      mean == "Not significant" & trend == "Positive" ~ "Neutral increasing"
    )) %>% 
    mutate(label_all = factor(label_all, 
                                 levels = c("Negative decreasing", "Negative stable", "Negative increasing",
                                            "Neutral decreasing", "Neutral stable", "Neutral increasing",
                                            "Positive decreasing", "Positive stable", "Positive increasing") ))
  color.palette.all = c(
    "Negative decreasing" = "#8B2300",  # Darker orange-red for negative decreasing
    "Negative stable" = "#FF9933",      # Mid-tone orange for negative stable
    "Negative increasing" = "#FFCC99",  # Light orange for negative increasing
    
    "Positive decreasing" = "#003366",  # Dark blue for positive decreasing
    "Positive stable" = "#3399FF",      # Mid-tone blue for positive stable
    "Positive increasing" = "#99CCFF",  # Light blue for positive increasing
    
    "Neutral decreasing" = "#4C1130",   # Dark purple for neutral decreasing
    "Neutral stable" = "#996699",       # Mid-tone purple for neutral stable
    "Neutral increasing" = "#C99EC9"    # Light purple for neutral increasing
  )
  
  
  
  p.fig6d = ggplot_map_piechart(
    df_maps_all,
    "id_timeseries",
    var_pie = "label_all",
    label_var_pie = "label_all",
    coef_r = 1.25
  ) + scale_fill_manual(values = color.palette.all) +
    # labs(title = paste0("     From ", labels_of_variables[this_cause],
    #                     " to ", labels_of_variables[this_consequence], 
    #                     " - Sign and Trend among stocks with Causality"),
    #      fill = "Sign and Trend\nof effect") +
    labs(fill = "Sign and Trend\nof effect") +
    annotate("text", x = -Inf, y = Inf, vjust = 2.2, hjust = -0.08, size = 3,
             label = paste0("From ", labels_of_variables[this_cause], " to ", labels_of_variables[this_consequence], " - Sign and Trend among stocks with Causality")) + 
    theme(plot.title = element_blank()) +
    theme(legend.position = "right", 
          legend.text = element_text(size = 6), legend.title = element_text(size = 8)) + 
    annotate("text", x = -Inf, y = Inf, label = ifelse(k == 1, "(a)", "(b)"), 
             vjust = 1.5, hjust = -0.1, size = 5, fontface = "bold")
  print(p.fig6d)
  for (typ in types_plots) {
    ggsave(p.fig6d, filename = paste0(dir_out, typ, "/fig6-map_piechart-", this_cause,
                                      "_to_", this_consequence, "-all.", typ),
           device = typ, width = 8, height = 4)
  }
  

}


# * * * * Link Causality HR -> SProd with stock status * * * *

if (identical(list_of_causality_tested, list(c("sst.z", "prodbest.divTB"),
                                             c("UdivUmsypref", "prodbest.divTB"),
                                             c("prodbest.divTB", "sst.z"),
                                             c("prodbest.divTB", "UdivUmsypref")) ) ) {
  base_tmp = res_smap %>% 
    filter(var_cause == "UdivUmsypref", var_consequence == "prodbest.divTB") %>% 
    left_join(df, by = c("id_timeseries" = name_id_timeseries, "time" = name_time)) %>% 
    select(id_timeseries, time, strength, UdivUmsypref, BdivBmsypref)
  
  for (x_variable in c("UdivUmsypref", "BdivBmsypref")) {
    
    tmp = base_tmp %>% 
      filter(!is.na(get(x_variable)), !is.na(strength))
    
    x_label = ifelse(x_variable == "UdivUmsypref", 
                     "Harvest rate relative to MSY preference (U/Umsypref)", 
                     "Biomass relative to MSY preference (B/Bmsypref)")
    x_label_short = ifelse(x_variable == "UdivUmsypref", "U/Umsypref", "B/Bmsypref")
    x_letter = substring(x_variable, 1, 1)
    
    # ALL STOCKS, scatter plot of strength depending on x_variable
    p.figAdd8 <- tmp %>% 
      group_by(id_timeseries) %>%
      mutate(time_since_start = time - min(time)) %>% 
      ungroup() %>%
      ggplot(aes(x = get(x_variable), y = strength, color = time_since_start)) +
      geom_point(alpha= 0.2) + 
      geom_path(aes(group = id_timeseries), alpha = 0.3) +
      geom_smooth(method = "lm", formula = y ~ x, color = "tomato", fill = "tomato", alpha = 0.3) +
      scale_color_viridis_c() +
      geom_vline(xintercept = 1, linetype = "dashed", color = "#444444") +
      labs(x = x_label,
           y = "S-map coefficient (HRate causing SProd)",
           color = "Time since start of timeseries (years)",
           title = "Strength of causality vs stock status") +
      theme_light() + 
      theme(legend.position = "top", legend.key.height = unit(0.2, "cm"))
    
    for (typ in types_plots) { 
      ggsave(p.figAdd8, filename = paste0(dir_out, typ, "/figAdd8-scatter_strength_", x_letter, ".", typ), 
             device = typ, width = 6, height = 4.5) 
    }
    
    # Linear models of strength depending on x_variable for each stock
    list_lms = list()
    df_lms = data.frame(id_timeseries = character(), intercept = numeric(), 
                        slope = numeric(), r_squared = numeric(), p_value = numeric(), 
                        stringsAsFactors = FALSE)
    for (st in unique(tmp$id_timeseries)) {
      tmp_lm = lm(as.formula(paste0("strength ~ ", x_variable)), data = tmp %>% filter(id_timeseries == st))
      list_lms[[st]] = tmp_lm
      df_lms = rbind(df_lms, data.frame(
        id_timeseries = st, intercept = as.numeric(coef(tmp_lm)[1]), slope = as.numeric(coef(tmp_lm)[2]), 
        r_squared = summary(tmp_lm)$r.squared, p_value = summary(tmp_lm)$coefficients[2, 4]
      )) 
    }
    # Save csv
    write.csv(df_lms, file = paste0(dir_out, "csv/linear_models_strengthHRtoSProd_", x_variable, "_per_stock.csv"), row.names = FALSE)
    
    # PER STOCKS, scatter plot of strength depending on x_variable
    tmp2 <- tmp %>%
      left_join(df_lms[, c("id_timeseries", "p_value")], by = "id_timeseries") %>%
      mutate(sig = ifelse(p_value < 0.05, "sig", "nonsig"))
    stocks_split <- split(unique(tmp2$id_timeseries), 
                          ceiling(seq_along(unique(tmp2$id_timeseries))/25))
    for (i in seq_along(stocks_split)) {
      p.figAdd9_part <- tmp2 %>% 
        filter(id_timeseries %in% stocks_split[[i]]) %>% 
        arrange(time) %>% 
        ggplot(aes(x = get(x_variable), y = strength)) +
        facet_wrap(~ id_timeseries, ncol = 5, scales = "free") +
        geom_point(aes(color = time), size = 1.5, alpha = 0.5) +
        geom_path(aes(color = time), linewidth = 0.5) +
        geom_smooth(data = tmp2 %>% filter(p_value < 0.05, id_timeseries %in% stocks_split[[i]]),
                    method = "lm", formula = y ~ x, color = "tomato", se = TRUE) +
        geom_smooth(data = tmp2 %>% filter(p_value >= 0.05, id_timeseries %in% stocks_split[[i]]),
                    method = "lm", formula = y ~ x, color = "#444444", se = TRUE) +
        geom_vline(xintercept = 1, linetype = "dashed", color = "#444444") +
        scale_color_viridis_c() +
        labs(x = x_label,
             y = "S-map coefficient (HRate causing SProd)",
             color = "Year") +
        theme_light()
      for (typ in types_plots) {
        ggsave(filename = paste0(dir_out, typ, "/figAdd9-traj_strength_", x_letter, "_page", i, ".", typ),
               plot = p.figAdd9_part, device = typ, width = 10, height = 10)
      }
    }
    
    # SUMMARY of linear models: scatter plot of slope vs R-squared, colored by p-value
    p.figAdd10 = ggplot(df_lms, aes(x = slope, y = r_squared, color = p_value)) + 
      geom_point(size = 3) + scale_color_viridis_c(option = "plasma", end = 0.8) +
      labs(x = paste0("Slope of the relationship between S-map coefficient and ", x_label_short),  
           y = "R-squared of the relationship", color = "P-value of the slope") + 
      theme_light() + theme(legend.position = "bottom")
    for (typ in types_plots) {
      ggsave(p.figAdd10, filename = paste0(dir_out, typ, "/figAdd10-scatter_slope_r2_", x_letter, ".", typ), 
             device = typ, width = 6, height = 4.5)
    }
    
    # Positivity of slope vs significance
    cat(paste0("Strength HR -> SProd depending on ", x_variable, ": \n"))
    print(table(
      `Trend` = factor(df_lms$slope > 0, levels = c(TRUE, FALSE), labels = c("Positive", "Negative")),
      `Significant` = factor(df_lms$p_value < 0.05, levels = c(TRUE, FALSE), labels = c("Yes", "No"))
    ))
  }
  
  p.fig.Add11 = df %>% 
    left_join(res_causality %>% filter(var_cause == "UdivUmsypref", var_consequence == "prodbest.divTB") %>% 
                select(id_timeseries, causality), by = setNames("id_timeseries", name_id_timeseries)) %>%
    ggplot(aes(x = BdivBmsypref, y = UdivUmsypref, color = causality)) + 
    geom_point(size = 0.7, alpha = 0.2) + 
    geom_vline(xintercept = 1, linetype = "dashed", color = "#444444") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "#444444") +
    geom_smooth(method = "lm", formula = y ~ x, color = "blue", fill = "blue", alpha = 0.3) +
    scale_color_manual(values = c("Yes" = "#97d89a", "No" = "#de7371", "Not relevant" = "#444444")) + 
    labs(x = "Biomass relative to MSY preference (B/Bmsypref)", 
         y = "Harvest rate relative to MSY preference (U/Umsypref)", 
         color = "Causality of HRate causing SProd") +
    theme_light()
  
  for (typ in types_plots) { 
    ggsave(p.fig.Add11, filename = paste0(dir_out, typ, "/figAdd11-scatter_UdivUmsypref_vs_BdivBmsypref.", typ), 
           device = typ, width = 6, height = 4.5)
    ggsave(p.fig.Add11 + xlim(0, 3) + ylim(0, 3), filename = paste0(dir_out, typ, "/figAdd11-zoom-scatter_UdivUmsypref_vs_BdivBmsypref.", typ), 
           device = typ, width = 6, height = 4.5)
  }
}


# Traits ------------------------------------------------------------------

traits = readRDS("data/traits/traits_export_simplif.rds")

trait_labels <- c(
  Loo = "Asymptotic length (Loo)",
  K = "Growth coefficient (K)",
  Winfinity = "Asymptotic weight (Woo)",
  tmax = "Maximum age (tmax)",
  tm = "Age at maturity (tm)",
  M = "Natural mortality (M)",
  Lm = "Length at maturity (Lm)",
  Temperature = "Temperature (°C)",
  FoodTroph = "Trophic level",
  mean_B_prshf = "Mean biomass",
  mean_U_prshf = "Mean harvest rate",
  U_change_prshf = "Change in harvest rate",
  mean_ER_prshf = "Mean exploitation rate",
  ER_change_prshf = "Change in exploitation rate",
  sum_C_prshf = "Total catch"
)


names_traits = colnames(traits)[3:ncol(traits)]

for (tr in names_traits) {
  if (is.character(traits[[tr]])) {
    traits[[tr]] = as.factor(traits[[tr]])
  }
}

# Types of each column
types_cols = sapply(traits, class)
numeric_traits = names_traits[types_cols[names_traits] %in% c("numeric", "integer")]
factor_traits = names_traits[types_cols[names_traits] %in% c("factor", "character")]

# Link causality, strength and traits
df_res_traits_num = res_causality %>% 
  left_join(res_test_smap, by = c("id_timeseries", "var_cause", "var_consequence")) %>% 
  left_join(traits, by = c("id_timeseries" = name_id_timeseries)) %>% 
  pivot_longer(cols = all_of(numeric_traits), names_to = "trait", values_to = "trait_value") %>% 
  mutate(rel = paste0(labels_of_variables[var_cause], " to ", labels_of_variables[var_consequence]))

df_res_traits_fac = res_causality %>% 
  left_join(res_test_smap, by = c("id_timeseries", "var_cause", "var_consequence")) %>% 
  left_join(traits, by = c("id_timeseries" = name_id_timeseries)) %>% 
  pivot_longer(cols = all_of(factor_traits), names_to = "trait", values_to = "trait_value") %>% 
  mutate(rel = paste0(labels_of_variables[var_cause], " to ", labels_of_variables[var_consequence]))


for (k in 1:2) {
  
  this_cause = list_of_causality_tested[[k]][1]
  this_consequence = list_of_causality_tested[[k]][2]

  # Quantitative trait depending on causality
  
  p.figAdd5a = df_res_traits_num %>% 
    filter(var_cause == this_cause, 
           var_consequence == this_consequence) %>% 
    ggplot(aes(x = causality, y = trait_value, fill = causality)) +
    facet_wrap(~ trait, scales = "free_y", labeller = labeller(trait = trait_labels)) +
    geom_boxplot() +
    scale_fill_manual(values = color.palette.caus) + 
    labs(title = paste0("From ", labels_of_variables[this_cause],
                        " to ", labels_of_variables[this_consequence],
                        " - Causality"),
         x = "Causality", y = "Trait value") +
    theme_light() + theme(strip.text = element_text(size = 9)) + guides(fill = "none")
  print(p.figAdd5a)
  for (typ in types_plots) {
    ggsave(p.figAdd5a, filename = paste0(dir_out, typ, "/figAdd5-traits-", this_cause, 
                                      "_to_", this_consequence, "-causality.", typ),
           device = typ, width = 9, height = 7)
  }
  
  # Quantitative trait depending on mean of the effect
  p.figAdd5b = df_res_traits_num %>% 
    filter(var_cause == this_cause, 
           var_consequence == this_consequence) %>% 
    filter(!is.na(mean)) %>% 
    ggplot(aes(x = mean, y = trait_value, fill = mean)) +
    facet_wrap(~ trait, scales = "free_y", labeller = labeller(trait = trait_labels)) +
    geom_boxplot() +
    scale_fill_manual(values = color.palette.mean.trend) + 
    labs(title = paste0("From ", labels_of_variables[this_cause],
                        " to ", labels_of_variables[this_consequence],
                        " - Sign"),
         x = "Mean effect", y = "Trait value") +
    theme_light() + theme(strip.text = element_text(size = 9)) + guides(fill = "none")
  print(p.figAdd5b)
  for (typ in types_plots) {
    ggsave(p.figAdd5b, filename = paste0(dir_out, typ, "/figAdd5-traits-", this_cause, 
                                         "_to_", this_consequence, "-sign.", typ),
           device = typ, width = 9, height = 7)
  }
  
  # Quantitative trait depending on trend of the effect
  p.figAdd5c = df_res_traits_num %>% 
    filter(var_cause == this_cause, 
           var_consequence == this_consequence) %>% 
    filter(!is.na(trend)) %>% 
    ggplot(aes(x = trend, y = trait_value, fill = trend)) +
    facet_wrap(~ trait, scales = "free_y", labeller = labeller(trait = trait_labels)) +
    geom_boxplot() +
    scale_fill_manual(values = color.palette.mean.trend) + 
    labs(title = paste0("From ", labels_of_variables[this_cause],
                        " to ", labels_of_variables[this_consequence],
                        " - Trend"),
         x = "Trend of the effect", y = "Trait value") +
    theme_light() + theme(strip.text = element_text(size = 9))
  print(p.figAdd5c)
  for (typ in types_plots) {
    ggsave(p.figAdd5c, filename = paste0(dir_out, typ, "/figAdd5-traits-", this_cause, 
                                         "_to_", this_consequence, "-trend.", typ),
           device = typ, width = 9, height = 7)
  }
  
  # Quantitative trait depending on trend of the effect
  df_maps_all = df_maps %>%
    filter(causality == "Yes") %>%
    mutate(label_all = case_when(
      mean == "Negative" & trend == "Negative" ~ "Negative decreasing",
      mean == "Negative" & trend == "Not significant" ~ "Negative stable",
      mean == "Negative" & trend == "Positive" ~ "Negative increasing",
      mean == "Positive" & trend == "Negative" ~ "Positive decreasing",
      mean == "Positive" & trend == "Not significant" ~ "Positive stable",
      mean == "Positive" & trend == "Positive" ~ "Positive increasing",
      mean == "Not significant" & trend == "Negative" ~ "Neutral decreasing",
      mean == "Not significant" & trend == "Not significant" ~ "Neutral stable",
      mean == "Not significant" & trend == "Positive" ~ "Neutral increasing"
    )) %>% 
    mutate(label_all = factor(label_all, 
                              levels = c("Negative decreasing", "Negative stable", "Negative increasing",
                                         "Neutral decreasing", "Neutral stable", "Neutral increasing",
                                         "Positive decreasing", "Positive stable", "Positive increasing") ))
  p.figAdd5d = df_res_traits_num %>% 
    filter(var_cause == this_cause, 
           var_consequence == this_consequence) %>% 
    filter(!is.na(trend))  %>%
    mutate(label_all = case_when(
      mean == "Negative" & trend == "Negative" ~ "Negative decreasing",
      mean == "Negative" & trend == "Not significant" ~ "Negative stable",
      mean == "Negative" & trend == "Positive" ~ "Negative increasing",
      mean == "Positive" & trend == "Negative" ~ "Positive decreasing",
      mean == "Positive" & trend == "Not significant" ~ "Positive stable",
      mean == "Positive" & trend == "Positive" ~ "Positive increasing",
      mean == "Not significant" & trend == "Negative" ~ "Neutral decreasing",
      mean == "Not significant" & trend == "Not significant" ~ "Neutral stable",
      mean == "Not significant" & trend == "Positive" ~ "Neutral increasing"
    )) %>% 
    mutate(label_all = factor(label_all, 
                              levels = c("Negative decreasing", "Negative stable", "Negative increasing",
                                         "Neutral decreasing", "Neutral stable", "Neutral increasing",
                                         "Positive decreasing", "Positive stable", "Positive increasing") )) %>% 
    ggplot(aes(x = label_all, y = trait_value, fill = label_all)) +
    facet_wrap(~ trait, scales = "free_y", labeller = labeller(trait = trait_labels)) +
    geom_boxplot() +
    scale_fill_manual(values = color.palette.all) + 
    labs(title = paste0("From ", labels_of_variables[this_cause],
                        " to ", labels_of_variables[this_consequence],
                        " - Sign and Trend"),
         x = "Sign and Trend of the effect", y = "Trait value") +
    theme_light() + guides(fill = "none") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          strip.text = element_text(size = 9))
  print(p.figAdd5d)
  for (typ in types_plots) {
    ggsave(p.figAdd5d, filename = paste0(dir_out, typ, "/figAdd5-traits-", this_cause, 
                                         "_to_", this_consequence, "-all.", typ),
           device = typ, width = 9, height = 7)
  }
  
  
  l = 4
  p.figAdd6a = df_res_traits_fac %>% 
    filter(var_cause == this_cause, 
           var_consequence == this_consequence) %>% 
    filter(trait == factor_traits[l]) %>% 
    ggplot(aes(x = causality, fill = trait_value)) + 
    geom_bar(position = "fill") +
    labs(title = paste0("From ", labels_of_variables[this_cause],
                        " to ", labels_of_variables[this_consequence],
                        " - Causality"),
         x = "Causality", y = "Proportion", fill = factor_traits[l]) +
    theme_light()
  print(p.figAdd6a)
  for (typ in types_plots) {
    ggsave(p.figAdd6a, filename = paste0(dir_out, typ, "/figAdd6-traits_quali-", this_cause, 
                                         "_to_", this_consequence, "-causality.", typ),
           device = typ, width = 4, height = 4)
  }
  
  
  p.figAdd6b = df_res_traits_fac %>% 
    filter(var_cause == this_cause, 
           var_consequence == this_consequence) %>% 
    filter(trait == factor_traits[l]) %>% 
    filter(!is.na(mean)) %>% 
    ggplot(aes(x = mean, fill = trait_value)) + 
    geom_bar(position = "fill") +
    labs(title = paste0("From ", labels_of_variables[this_cause],
                        " to ", labels_of_variables[this_consequence],
                        " - Sign"),
         x = "Mean effect", y = "Proportion", fill = factor_traits[l]) +
    theme_light()
  print(p.figAdd6b)
  for (typ in types_plots) {
    ggsave(p.figAdd6b, filename = paste0(dir_out, typ, "/figAdd6-traits_quali-", this_cause, 
                                         "_to_", this_consequence, "-sign.", typ),
           device = typ, width = 4, height = 4)
  }
  
  p.figAdd6c = df_res_traits_fac %>% 
    filter(var_cause == this_cause, 
           var_consequence == this_consequence) %>% 
    filter(trait == factor_traits[l]) %>% 
    filter(!is.na(trend)) %>% 
    ggplot(aes(x = trend, fill = trait_value)) + 
    geom_bar(position = "fill") +
    labs(title = paste0("From ", labels_of_variables[this_cause],
                        " to ", labels_of_variables[this_consequence],
                        " - Trend"),
         x = "Trend of the effect", y = "Proportion", fill = factor_traits[l]) +
    theme_light()
  print(p.figAdd6c)
  for (typ in types_plots) {
    ggsave(p.figAdd6c, filename = paste0(dir_out, typ, "/figAdd6-traits_quali-", this_cause, 
                                         "_to_", this_consequence, "-trend.", typ),
           device = typ, width = 4, height = 4)
  }
}


# res_causality %>% 
#   left_join(res_test_smap, by = c("id_timeseries", "var_cause", "var_consequence")) %>% 
#   mutate(rel = paste0(labels_of_variables[var_cause], " to ", labels_of_variables[var_consequence])) %>%
#   dplyr::select(id_timeseries, rel, causality, mean, trend) %>%
#   group_by(id_timeseries) %>% 
#   mutate(keep = (sum(causality == "Yes" & rel == "SST to SProd") > 0) & 
#                 (sum(causality == "No" & rel == "HRate to SProd") > 0)) %>% 
#   ungroup() %>%
#   filter(keep) %>% 
#   pull(id_timeseries) %>% 
#   unique() %>% 
#   paste0(collapse="\", \"") %>% 
#   cat()
