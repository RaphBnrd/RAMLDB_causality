
rm(list = ls())

# Set up packages ---------------------------------------------------------

library(shiny)

library(dplyr)
library(tidyverse)

library(plotly)


# Import objects ----------------------------------------------------------


path_dataframe_input = "data/RAM_timeseries_clean.csv"
df = read.csv(path_dataframe_input)

name_id_timeseries = "stockid"
name_time = "year"


dir_out = "out/20241003-full/"

file_save_out_simplex_tp1 = paste0(dir_out, "computations/01_a-out_simplex_tp1.csv")
file_save_out_simplex_tp1_summary_opti_E = paste0(dir_out, "computations/01_b-out_simplex_tp1_summary_opti_E.csv")

file_save_out_ccm_best_E_and_tp_summaries = paste0(dir_out, "computations/02_a-out_ccm_best_E_and_tp-summaries.csv")
file_save_out_ccm_best_E_and_tp_assessment = paste0(dir_out, "computations/02_b-out_ccm_best_E_and_tp-assessment.csv")
file_save_out_strength_causality = paste0(dir_out, "computations/03_a-out_strength_causality.csv")
file_save_out_strength_causality_details_smap = paste0(dir_out, "computations/03_b-out_strength_causality_details_smap.csv")

file_save_out_granger_causality = paste0(dir_out, "computations/04_a-out_granger_causality.csv")
file_save_out_granger_causality_details = paste0(dir_out, "computations/04_b-out_granger_causality_details.csv")

import_file = function(path_file) {
  
  if (!file.exists(path_file)) {
    warning("The file ", path_file, " does not exist... The function returns NULL.")
    return(NULL)
  } 
  
  if (grepl(".csv", path_file)) { # CSV file
    return(read.csv(path_file))
  } else if (grepl(".RData", path_file)) { # RData file
    load(path_file)
    return(get(ls()[ls() != "path_file"]))
  } else if (grepl(".rds", path_file)) { # RDS file
    return(readRDS(path_file))
  } else {
    stop("The file ", path_file, " is not a csv or RData file.")
  }
  
}

res_simplex_tp1 = import_file(file_save_out_simplex_tp1)
res_simplex_tp1_summary_opti_E = import_file(file_save_out_simplex_tp1_summary_opti_E)

res_ccm_best_E_summaries = import_file(file_save_out_ccm_best_E_and_tp_summaries)
colnames(res_ccm_best_E_summaries) = gsub("^X(\\d+)\\.$", "\\1%", colnames(res_ccm_best_E_summaries))
res_ccm_best_E_assessment = import_file(file_save_out_ccm_best_E_and_tp_assessment)

res_strength_causality = import_file(file_save_out_strength_causality)
res_strength_causality_details_smap = import_file(file_save_out_strength_causality_details_smap)

res_granger_causality = import_file(file_save_out_granger_causality)
res_granger_causality_detailed = import_file(file_save_out_granger_causality_details)



all_ids = unique(res_simplex_tp1$id_timeseries)
all_ids_plots = df %>% 
  group_by_at(vars(all_of(name_id_timeseries))) %>%
  summarise(n = n()) %>%
  filter(n >= 40) %>%
  pull(all_of(name_id_timeseries)) %>% unique()

all_variables = unique(res_ccm_best_E_assessment$var_lib)
labels_of_variables = c(
  "sst.z" = "SST", 
  "prodbest.div" = "Surplus of productivity", 
  "UdivUmsypref" = "Harvest rate"
)
for (var in setdiff(all_variables, names(labels_of_variables))) {
  labels_of_variables[var] = var
}


method_select_E_prefered = "positive_range_top_20_percent"
method_select_E_default = "max_rho" # if the prefered method provides NA




# Dataframe used for plots ------------------------------------------------

# df_plot_strength = df.summary.test.causality %>% rename(E_CCM = E) %>% 
#   left_join(df %>% group_by(stockid) %>% summarise(n.obs = n(), .groups = "drop"),
#             by = c("stockid" = "stockid")) %>%
#   left_join(opti.simplex.tp1 %>% group_by(stockid, variable) %>%
#               filter( if (any(method_E == method_E_for_plots & !is.na(E))) { method_E == method_E_for_plots
#               } else { method_E == method_E_for_plots_default } ) %>%
#               ungroup() %>% 
#               rename(E_simplex = E, rho_simplex = rho),
#             by = c("stockid" = "stockid", "var_lib" = "variable")) %>% 
#   filter(n.obs >= 40) %>% 
#   mutate(pos_rho_simp = rho_simplex >= 0) %>% 
#   mutate(status = case_when(
#     !pos_rho_simp ~ "Negative rho\nin simplex", 
#     causality & pos_rho_simp ~ "Causality", 
#     !causality & pos_rho_simp ~ "No causality"
#   )) %>%
#   mutate(label_pair = paste0(labels_of_variables[var_lib], "\nxmap\n", labels_of_variables[var_target])) %>% 
#   filter(label_pair %in% unlist(lapply(xmap_filter_for_plot, function(x) {
#     paste0(labels_of_variables[x[1]], "\nxmap\n", labels_of_variables[x[2]])
#   })) ) %>% 
#   mutate(kept = rho_simplex >= 0) %>% 
#   filter(causality, kept)
# 
# 
# 
# df_plot_strength_full = df.jacobian.smap %>% 
#   left_join(df %>% group_by(stockid) %>% summarise(n.obs = n(), .groups = "drop"),
#             by = c("stockid" = "stockid")) %>%
#   left_join(df.summary.test.causality %>% select(var_lib, var_target, stockid, causality) %>%
#               rename(consequence = var_lib, cause = var_target),
#             by = c("stockid" = "stockid", "consequence" = "consequence", "cause" = "cause")) %>%
#   left_join(opti.simplex.tp1 %>% group_by(stockid, variable) %>%
#               filter( if (any(method_E == method_E_for_plots & !is.na(E))) { method_E == method_E_for_plots
#               } else { method_E == method_E_for_plots_default } ) %>%
#               ungroup() %>% 
#               rename(E_simplex = E, rho_simplex = rho, consequence = variable),
#             by = c("stockid" = "stockid", "consequence" = "consequence")) %>% 
#   filter(n.obs >= 40) %>% 
#   mutate(pos_rho_simp = rho_simplex >= 0) %>% 
#   mutate(label_pair = paste0(labels_of_variables[consequence], "\nxmap\n", labels_of_variables[cause])) %>% 
#   filter(label_pair %in% unlist(lapply(xmap_filter_for_plot, function(x) {
#     paste0(labels_of_variables[x[1]], "\nxmap\n", labels_of_variables[x[2]])
#   })) ) %>% 
#   mutate(kept = rho_simplex >= 0)



# Draft -------------------------------------------------------------------






# Rshiny app --------------------------------------------------------------



# Define UI for the application

ui <- fluidPage(
  
  titlePanel("Explore EDM analysis - RAMLDB"),
  
  
  # The sidebar will contain the main inputs
  
  div(class = "sidebar",
      
      numericInput("tp", "Time prediction", value = 0, 
                   min = min(res_ccm_best_E_assessment$tp), max = max(res_ccm_best_E_assessment$tp)),
      selectInput("var_cause", "Cause", choices = as.vector(labels_of_variables[all_variables]), 
                  multiple = FALSE, selected = labels_of_variables[all_variables[2]]),
      selectInput("var_consequence", "Consequence", choices = as.vector(labels_of_variables[all_variables]), 
                  multiple = FALSE, selected = labels_of_variables[all_variables[1]]),
      radioButtons("with_or_without_causality", "Which id to plot:",
                   c("With causality" = "with", "Without causality" = "without")),
      uiOutput("ids_with_causality_UI"),
      uiOutput("ids_without_causality_UI")
  ),
  
  
  # The main panel contains the plots in different tabs
  
  div(class = "main-panel",
      mainPanel(
        tabsetPanel(
          id = "tabs",
          type = "pills",
          
          tabPanel("Global overview",
                   tags$h1("Causality assessment"),
                   plotlyOutput("plot_summary_causality", height = 800),
                   tags$h1("Strength of causality"),
                   plotlyOutput("plot_summary_strength_box", height = 800)
          ),
          
          tabPanel("Summary of this causality",
                   tags$h1("Strength of causality"),
                   plotlyOutput("plot_summary_strength_table_this_caus", height = 800),
                   tags$h1("Forecasting skill and tp"),
                   plotlyOutput("plot_ccm_tp_this_with_causality", height = 800),
                   plotlyOutput("plot_ccm_tp_this_without_causality", height = 800),
          ),
          
          tabPanel("Single id",
                   tags$h1("Time series used"),
                   plotlyOutput("plot_timeseries", height = 500),
                   tags$h1("Simplex projection"),
                   plotlyOutput("plot_simplex_projection", width = 700),
                   tags$h1("Convergent cross-mapping"),
                   htmlOutput("text_ccm"),
                   plotlyOutput("plot_ccm", height = 600, width = 700),
                   tags$h1("Strength of causality along time"),
                   plotlyOutput("plot_strength_caus_time", height = 500),
          )
          
        )
      )
  ),
  # The CSS style
  tags$style(HTML("
    .sidebar {position: fixed; top: 10%; height: 90%; left: 0; width: 20%; overflow-y: auto; 
            padding: 10px; background-color: #accfe0; border-radius: 0px 10px 0px 0px;}
    .main-panel {margin-left: 20%; padding: 10px; overflow-y: auto;}
  "))
)



# Define server logic required for the application

server <- function(input, output) {
  
  
  
  #   * * * * * * * * * * * * * * *
  # * * * * REACTIVE VARIABLES * * * *
  #   * * * * * * * * * * * * * * *
  
  
  # Variables
  key_var_cause = reactive({ names(labels_of_variables)[which(labels_of_variables == input$var_cause)] })
  key_var_consequence = reactive({ names(labels_of_variables)[which(labels_of_variables == input$var_consequence)] })
  
  
  # . . . ADAPTATIVE INPUTS . . .
  
  ids_with_causality_choices = reactive({
    res_ccm_best_E_assessment %>% 
      filter(var_lib == key_var_consequence() & var_target == key_var_cause(), causality, tp == input$tp) %>%
      pull(id_timeseries)
  })
  output$ids_with_causality_UI = renderUI({
    selectInput("ids_with_causality", paste0("IDs with causality (tp = ", input$tp, ")"),
                choices = ids_with_causality_choices(), multiple = FALSE)
    
  })
  
  ids_without_causality_choices = reactive({
    res_ccm_best_E_assessment %>% 
      filter(var_lib == key_var_consequence() & var_target == key_var_cause(), !causality, tp == input$tp) %>%
      pull(id_timeseries)
  })
  output$ids_without_causality_UI = renderUI({
    selectInput("ids_without_causality", paste0("IDs without causality (tp = ", input$tp, ")"),
                choices = ids_without_causality_choices(), multiple = FALSE)
    
  })
  
  
  
  
  
  #   * * * * * * * * * * * * * *
  # * * * * GLOBAL OVERVIEW * * * *
  #   * * * * * * * * * * * * * *
  
  
  # Histogram of causality assessment
  
  output$plot_summary_causality <- renderPlotly({
    
    id_removed = res_simplex_tp1_summary_opti_E %>% filter(id_timeseries %in% all_ids_plots) %>% 
      filter(is.na(E_opti_simplex), method_E_opti_simplex == method_select_E_prefered) %>% 
      pull(id_timeseries) %>% as.character() # we remove the stocks where the forecasting skill is always negative
    
    df_plot_count = res_ccm_best_E_assessment %>% rename(E_CCM = E) %>% 
      left_join(df %>% group_by_at(vars(all_of(name_id_timeseries))) %>% summarise(n.obs = n(), .groups = "drop"),
                by = c("id_timeseries" = name_id_timeseries)) %>%
      filter(n.obs >= 40) %>% 
      filter(tp == input$tp) %>%
      mutate(remove = ifelse(id_timeseries %in% id_removed, TRUE, FALSE)) %>% 
      mutate(status = case_when(
        remove ~ "NA", 
        causality & !remove ~ "Causality", 
        !causality & !remove ~ "No causality"
      )) %>%
      mutate(label_pair = paste0(labels_of_variables[var_lib], "\nxmap\n", labels_of_variables[var_target])) %>% 
      group_by(label_pair, status) %>%
      summarise(count = n(), .groups = 'drop')
    
    plot_ly(df_plot_count, x = ~label_pair, y = ~count, type = 'bar', 
            marker = list(line = list(color = "#666666", width = 1)), 
            color = ~factor(status, levels = c("Causality", "No causality", "NA")), 
            colors = c("Causality" = "#97d89a", "No causality" = "#de7371", "NA" = "#BBBBBB"),
            text = ~count, textposition = 'inside', insidetextanchor = 'middle', 
            textfont = list(color = 'black')
    ) %>%
      layout(title = paste0("Causality assessment (tp = ", input$tp, ")" ),
             xaxis = list(title = "", tickangle = -90), yaxis = list(title = "Count"), 
             barmode = "stack", legend = list(title = list(text = "Assessment")), 
             margin = list(b = 150))
    
  })
  
  
  # Boxplot of the strength of the causality
  
  output$plot_summary_strength_box <- renderPlotly({
    
    id_removed = res_simplex_tp1_summary_opti_E %>% filter(id_timeseries %in% all_ids_plots) %>% 
      filter(is.na(E_opti_simplex), method_E_opti_simplex == method_select_E_prefered) %>% 
      pull(id_timeseries) %>% as.character() # we remove the stocks where the forecasting skill is always negative
    
    df_plot = res_strength_causality %>% rename(E_CCM = E) %>%
      filter(id_timeseries %in% all_ids_plots & !id_timeseries %in% id_removed) %>%
      left_join(res_ccm_best_E_assessment, 
                by = c("id_timeseries" = "id_timeseries", "var_cause" = "var_target", "var_consequence" = "var_lib", "tp" = "tp"))
    
    df_plot %>%
      left_join(res_ccm_best_E_assessment %>% 
                  filter(id_timeseries %in% all_ids_plots & !id_timeseries %in% id_removed) %>%
                  group_by(var_lib, var_target, tp) %>%
                  summarise(n = sum(causality), .groups = "drop"),
                by = c("var_cause" = "var_target", "var_consequence" = "var_lib", "tp" = "tp")) %>%
      mutate(label_pair = paste0(labels_of_variables[var_consequence], "\nxmap\n", labels_of_variables[var_cause])) %>% 
      mutate(label_pair_full = paste0(label_pair, "\n(n = ", n, ")")) %>% 
      
      filter(tp == input$tp, causality) %>%
    
      plot_ly() %>%
      add_trace(x = ~label_pair_full, y = ~this_mean_strength,
                type = "box", boxpoints = "outliers", marker = list(opacity = 0.7),
                line = list(width = 1.5), showlegend = FALSE) %>%
      add_trace(x = ~label_pair_full, y = ~this_mean_strength,
                type = "scatter", mode = "markers", marker = list(color = '#444444', opacity = 0.7, size = 7),
                text = ~paste("ID:", id_timeseries, "<br>Strength=", this_mean_strength),
                hoverinfo = "text", showlegend = FALSE) %>%
      layout(title = paste0("Average Strength of the Causality (tp = ", input$tp, ")" ),
             xaxis = list(title = "", tickangle = -90),
             yaxis = list(title = "Coefficient in the Jacobian (S-map)"),
             shapes = list(list(type = "line", x0 = -0.5, x1 = 1.5, y0 = 0, y1 = 0,
                                line = list(dash = "dash", color = "gray", width = 1)) ),
             margin = list(l = 70, t = 50, r = 20, b = 150) )
    
  })
  
  
  
  
  #   * * * * * * * * * * * * * 
  # * * * * THIS CAUSALITY * * * *
  #   * * * * * * * * * * * * *
  
  
  # Table of the sign and trend of the strength of causality
  
  output$plot_summary_strength_table_this_caus <- renderPlotly({
    
    id_removed = res_simplex_tp1_summary_opti_E %>% filter(id_timeseries %in% all_ids_plots) %>% 
      filter(is.na(E_opti_simplex), method_E_opti_simplex == method_select_E_prefered) %>% 
      pull(id_timeseries) %>% as.character() # we remove the stocks where the forecasting skill is always negative
    
    df_plot = res_strength_causality %>% rename(E_CCM = E) %>%
      filter(id_timeseries %in% all_ids_plots & !id_timeseries %in% id_removed) %>%
      left_join(res_ccm_best_E_assessment, 
                by = c("id_timeseries" = "id_timeseries", "var_cause" = "var_target", "var_consequence" = "var_lib", "tp" = "tp")) %>%
      
      left_join(res_ccm_best_E_assessment %>% 
                  filter(id_timeseries %in% all_ids_plots & !id_timeseries %in% id_removed) %>%
                  group_by(var_lib, var_target, tp) %>%
                  summarise(n = sum(causality), .groups = "drop"),
                by = c("var_cause" = "var_target", "var_consequence" = "var_lib", "tp" = "tp")) %>%
      mutate(label_pair = paste0(labels_of_variables[var_consequence], "\nxmap\n", labels_of_variables[var_cause])) %>% 
      mutate(label_pair_full = paste0(label_pair, "\n(n = ", n, ")")) %>% 
      
      filter(tp == input$tp, causality, var_cause == key_var_cause(), var_consequence == key_var_consequence()) %>% 
      # filter(tp == 0, causality, var_cause == "sst.z", var_consequence == "prodbest.div") %>% 
      
      mutate(
        sign = case_when(
          this_t_test_p_val < 0.05 & this_mean_strength > 0 ~ "Positive",
          this_t_test_p_val < 0.05 & this_mean_strength < 0 ~ "Negative",
          TRUE ~ "Not significant"
        ),
        trend = case_when(
          this_trend_p_val < 0.05 & this_trend_slope > 0 ~ "Positive",
          this_trend_p_val < 0.05 & this_trend_slope < 0 ~ "Negative",
          TRUE ~ "Not significant"
        )
      ) %>% 
      mutate(sign = factor(sign, levels = c("Negative", "Not significant", "Positive")),
             trend = factor(trend, levels = c("Negative", "Not significant", "Positive")))
    
    df_plot %>% 
      group_by(sign, trend) %>% 
      summarise(n = n(), .groups = "drop") %>% 
      plot_ly() %>%
      add_trace(x = ~sign, y = ~trend, z = ~n, type = "heatmap", 
                colorscale = list(c(0, "#a1b4c4"), c(1, "#0972cb")) ) %>% 
      add_annotations(x = ~sign, y = ~trend, text = ~n, showarrow = FALSE,
                      color = ~ifelse(n > 0, "white", "black"), font = list(size = 16)) %>%
      
      layout(title = "Sign and trend of the strength of causality",
             xaxis = list(title = "Sign of the strength of causality"),
             yaxis = list(title = "Trend of the strength of causality"),
             margin = list(l = 70, t = 50, r = 20, b = 150) )
    
  })
  
  
  
  
  output$plot_ccm_tp_this_with_causality <- renderPlotly({
    
    id_removed = res_simplex_tp1_summary_opti_E %>% filter(id_timeseries %in% all_ids_plots) %>% 
      filter(is.na(E_opti_simplex), method_E_opti_simplex == method_select_E_prefered) %>% 
      pull(id_timeseries) %>% as.character() # we remove the stocks where the forecasting skill is always negative
    
    id_with_causality_this_tp = res_ccm_best_E_assessment %>% 
      filter(var_lib == key_var_consequence(), var_target == key_var_cause(), causality, tp == input$tp) %>%
      # filter(var_lib == "prodbest.div", var_target == "sst.z", causality, tp == 0) %>%
      pull(id_timeseries)
    
    res_ccm_best_E_assessment %>%
      filter(id_timeseries %in% all_ids_plots & !id_timeseries %in% id_removed & id_timeseries %in% id_with_causality_this_tp) %>%
      filter(var_lib == key_var_consequence(), var_target == key_var_cause()) %>%
      # filter(var_lib == "prodbest.div", var_target == "sst.z") %>% 
      
      plot_ly() %>%
      add_trace(x = ~tp, y = ~max_median_rho_ccm_with_libs, split = ~id_timeseries,
                type = "scatter", mode = "lines+markers", hoverinfo = "text",
                text = ~paste("ID: ", id_timeseries, "<br>Max rho: ", max_median_rho_ccm_with_libs),
                marker = list(size = 7, opacity = 0.6), line = list(width = 1, opacity = 0.6)) %>%
      layout(title = "Forecasting skill vs library size - IDs WITH causality",
             xaxis = list(title = "Time horizon of the prediction"),
             yaxis = list(title = "Maximum Forecasting skill (on the median per library size)"),
             margin = list(t = 50, b = 70))
    
  })
  
  
  output$plot_ccm_tp_this_without_causality <- renderPlotly({
    
    id_removed = res_simplex_tp1_summary_opti_E %>% filter(id_timeseries %in% all_ids_plots) %>% 
      filter(is.na(E_opti_simplex), method_E_opti_simplex == method_select_E_prefered) %>% 
      pull(id_timeseries) %>% as.character() # we remove the stocks where the forecasting skill is always negative
    
    id_without_causality_this_tp = res_ccm_best_E_assessment %>% 
      filter(var_lib == key_var_consequence(), var_target == key_var_cause(), !causality, tp == input$tp) %>%
      # filter(var_lib == "prodbest.div", var_target == "sst.z", !causality, tp == 0) %>%
      pull(id_timeseries)
    
    res_ccm_best_E_assessment %>%
      filter(id_timeseries %in% all_ids_plots & !id_timeseries %in% id_removed & id_timeseries %in% id_without_causality_this_tp) %>%
      filter(var_lib == key_var_consequence(), var_target == key_var_cause()) %>%
      # filter(var_lib == "prodbest.div", var_target == "sst.z") %>% 
      
      plot_ly() %>%
      add_trace(x = ~tp, y = ~max_median_rho_ccm_with_libs, split = ~id_timeseries,
                type = "scatter", mode = "lines+markers", hoverinfo = "text",
                text = ~paste("ID: ", id_timeseries, "<br>Max rho: ", max_median_rho_ccm_with_libs),
                marker = list(size = 7, opacity = 0.6), line = list(width = 1, opacity = 0.6)) %>%
      layout(title = "Forecasting skill vs library size - IDs WITHOUT causality",
             xaxis = list(title = "Time horizon of the prediction"),
             yaxis = list(title = "Maximum Forecasting skill (on the median per library size)"),
             margin = list(t = 50, b = 70))
    
  })
  
  
  
  
  #   * * * * * * * * * * * *
  # * * * * SINGLE STOCK * * * *
  #   * * * * * * * * * * * *
  
  
  # Time series for the Simplex and S-map (variable along time)
  
  output$plot_timeseries <- renderPlotly({
    
    if (input$with_or_without_causality == "with") {
      this_id = input$ids_with_causality
    } else {
      this_id = input$ids_without_causality
    }
    
    this_df = df %>% filter(!!sym(name_id_timeseries) == this_id)
    
    plot_ly() %>%
      add_trace(x = ~this_df$year, y = ~this_df[[key_var_cause()]], 
                type = "scatter", mode = "markers+lines",
                yaxis = "y", name = paste0(input$var_cause, " (cause)")) %>%
      add_trace(x = ~this_df$year, y = ~this_df[[key_var_consequence()]], 
                type = "scatter", mode = "markers+lines",
                yaxis = "y2", name = paste0(input$var_consequence, " (consequence))")) %>%
      layout(title = paste0("Time series - ", this_id), 
             xaxis = list(title = "Year"), 
             yaxis = list(title = input$var_cause, side = "left", showgrid = TRUE, zeroline = TRUE),
             yaxis2 = list(title = input$var_consequence, side = "right", overlaying = "y", 
                           showgrid = TRUE, zeroline = TRUE, automargin = TRUE),
             margin = list(t = 50, b = 70),
             legend = list(orientation = "h", x = 0.5, y = -0.25, xanchor = "center", yanchor = "bottom"))
  })
  
  
  
  # Simplex projection (rho along E)
  
  output$plot_simplex_projection <- renderPlotly({
    
    if (input$with_or_without_causality == "with") {
      this_id = input$ids_with_causality
    } else {
      this_id = input$ids_without_causality
    }
    
    var_plot = key_var_consequence()
    
    this_df_plot = res_simplex_tp1 %>% 
      filter(id_timeseries == this_id, variable == var_plot) %>% 
      left_join(
        res_simplex_tp1_summary_opti_E %>%
          filter(id_timeseries == this_id, variable == var_plot) %>%
          pivot_wider(names_from = method_E_opti_simplex, values_from = rho_E_opti_simplex) %>%
          rowwise() %>%
          mutate(label = paste(names(.[4:ncol(.)])[!is.na(c_across(4:ncol(.)))], collapse = "\n> ") ) %>%
          ungroup() %>% 
          select(id_timeseries, variable, E_opti_simplex, label),
        by = c("id_timeseries" = "id_timeseries", "variable" = "variable", "E" = "E_opti_simplex")
      ) %>% 
      mutate(label = ifelse(is.na(label), "", paste0("\n> ", label)) ) %>% 
      mutate(label = paste0("E=", E, "\nrho=", round(rho, 2),
                            gsub("_", " ", gsub("_percent", "%", label))) )
    
    vlines <- lapply(res_simplex_tp1_summary_opti_E %>%
                       filter(id_timeseries == this_id, variable == var_plot) %>% 
                       pull(E_opti_simplex) %>% unique(), 
                     function(e) {
                       list(type = "line", x0 = e, x1 = e, y0 = 0, y1 = 1, xref = "x", yref = "paper", 
                            line = list(color = "gray", dash = "dash", width = 1))
                     })
    
    
    plot_ly(data = this_df_plot, x = ~E, y = ~rho, type = "scatter", mode = "lines+markers", 
            marker = list(size = 7), line = list(width = 2),
            name = "Forecasting Skill", text = ~label, hoverinfo = "text+name") %>%
      layout(title = list(text = paste0("Simplex projection tp=1<br>", labels_of_variables[var_plot], " - ", this_id)),
             xaxis = list(title = "Embedding dimension E"), yaxis = list(title = "Forecasting skill rho"),
             shapes = vlines, margin = list(l = 60, r = 20, t = 50, b = 60), hovermode = "closest")
    
  })
  
  
  
  # Text of the test on the CCM
  
  output$text_ccm <- renderUI({
    
    if (input$with_or_without_causality == "with") {
      this_id = input$ids_with_causality
    } else {
      this_id = input$ids_without_causality
    }
    
    E = res_simplex_tp1_summary_opti_E %>% 
      filter(id_timeseries == this_id, variable == key_var_consequence(), method_E_opti_simplex == method_select_E_prefered) %>%
      # filter(id_timeseries == this_id, variable == "prodbest.div", method_E_opti_simplex == method_select_E_prefered) %>% 
      pull(E_opti_simplex)
    this_method_E = method_select_E_prefered
    if (is.na(E)) {
      E = res_simplex_tp1_summary_opti_E %>% 
        filter(id_timeseries == this_id, variable == key_var_consequence(), method_E_opti_simplex == method_select_E_default) %>%
        # filter(id_timeseries == this_id, variable == "prodbest.div", method_E_opti_simplex == method_select_E_default) %>%
        pull(E_opti_simplex)
      this_method_E = method_E_for_plots_default
    }
    E_str = as.character(E)
    
    this_res = res_ccm_best_E_assessment %>% 
      filter(id_timeseries == this_id, var_lib == key_var_consequence(), var_target == key_var_cause(), tp == input$tp)
      # filter(id_timeseries == this_id, var_lib == "prodbest.div", var_target == "sst.z", tp == 0)
    
    HTML(paste0("Optimal E = ", E_str, "  (with the method: ", this_method_E, ")<br><br>",
                
                "Kendall's tau: ", round(this_res %>% pull(kendall_tau), 4), "<br>",
                "Kendall's p-value: ", ifelse(this_res %>% pull(kendall_p_val) < 0.001, "<0.001", round(this_res %>% pull(kendall_p_val), 4)), "<br>",
                "Significance: ", ifelse(this_res %>% pull(kendall_p_val) < 0.05 & this_res %>% pull(kendall_tau) > 0, 
                                         "Significantly increasing", "Not significantly increasing"), "<br><br>",
                
                "Original above null for ", round(this_res %>% pull(rate.libsizes_50..origin_above_95..null)*100, 2), " % of the library sizes<br><br>",
                # "Original under null for the following library sizes: ", 
                # paste(names(which(!this_test_CCM[[E_str]]$vect_compare.origin.null)), collapse = ", "), "<br><br>",
                
                "Causality: ", ifelse(this_res %>% pull(causality), "yes", "no"),
                "<br><br>"))
    
  })
  
  
  
  # Convergent cross-mapping (rho along library sizes)
  
  output$plot_ccm <- renderPlotly({ 
    
    if (input$with_or_without_causality == "with") {
      this_id = input$ids_with_causality
    } else {
      this_id = input$ids_without_causality
    }
    
    E = res_simplex_tp1_summary_opti_E %>%
      filter(id_timeseries == this_id, variable == key_var_consequence(), method_E_opti_simplex == method_select_E_prefered) %>%
      # filter(id_timeseries == this_id, variable == "prodbest.div", method_E_opti_simplex == method_select_E_prefered) %>%
      pull(E_opti_simplex)
    if (is.na(E)) {
      E = res_simplex_tp1_summary_opti_E %>%
        filter(id_timeseries == this_id, variable == key_var_consequence(), method_E_opti_simplex == method_select_E_default) %>%
        pull(E_opti_simplex)
    }
    E_str = as.character(E)
    
    this_df_CCM = res_ccm_best_E_summaries %>% 
      filter(id_timeseries == this_id, var_lib == key_var_consequence(), var_target == key_var_cause(), tp == input$tp)
      # filter(id_timeseries == this_id, var_lib == "prodbest.div", var_target == "sst.z", tp == 0)
    
    values_origin = c("5%", "50%", "95%")
    values_null = c("5%", "50%", "95%")
    
    df_plot = this_df_CCM %>% 
      dplyr::select(c("var_lib", "var_target", "E", "dataset", "lib_size", unique(c(values_origin, values_null))))
    
    y_range = c(min(0, min(df_plot[[values_origin[1]]]), min(df_plot[[values_null[1]]])),
                max(1, max(df_plot[[values_origin[3]]]), max(df_plot[[values_null[3]]])))
    
    suffix_title = ""
    
    subtitle = paste0("Grey: null model (", values_origin[2], " quantile and ",
                      values_origin[1], "-", values_origin[3], " quantile)  ;  ",
                      "Red: original data (", values_null[2], " quantile and ",
                      values_null[1], "-", values_null[3], " quantiles)")
    
    plot_ly(x = ~lib_size) %>%
      add_lines(data = df_plot %>% filter(dataset == "null"), y = ~get(values_null[2]), line = list(color = "#4D4D4D"),
                name = paste0("Null model (", values_null[2], ")")) %>%
      add_ribbons(data = df_plot %>% filter(dataset == "null"), y = ~get(values_null[2]), ymin = ~get(values_null[1]), 
                  ymax = ~get(values_null[3]), fillcolor = "#4D4D4D", opacity = 0.3, line = list(color = "#4D4D4D"),
                  name = paste0("Null model (", values_null[1], "-", values_null[3], ")")) %>%
      add_lines(data = df_plot %>% filter(dataset == "origin"), y = ~get(values_origin[2]), line = list(color = "#EE2C2C"), 
                name = paste0("Original data (", values_origin[2], ")")) %>%
      add_lines(data = df_plot %>% filter(dataset == "origin"), y = ~get(values_origin[1]), line = list(color = "#EE2C2C", dash = "dash"),
                name = paste0("Original data (", values_origin[1], ")")) %>%
      add_lines(data = df_plot %>% filter(dataset == "origin"), y = ~get(values_origin[3]), line = list(color = "#EE2C2C", dash = "dash"),
                name = paste0("Original data (", values_origin[3], ")")) %>%
      layout(title = paste0("Predictive skill vs library size - ", this_id, "\n",
                            input$var_consequence, " XMAP ", input$var_cause),
             xaxis = list(title = "Library size"), yaxis = list(title = "Forecasting skill (rho)", range = y_range),
             margin = list(t = 50, b = 70), legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.15))
    
  })
  
  
  
  
  # Strength of causality along time
  
  output$plot_strength_caus_time <- renderPlotly({
    
    if (input$with_or_without_causality == "with") {
      this_id = input$ids_with_causality
    } else {
      this_id = input$ids_without_causality
    }
    
    name_cause_delayed = paste0(key_var_cause(), "_t")
    if (input$tp > 0) {
      name_cause_delayed = paste0(name_cause_delayed, "_plus_", input$tp)
    } else if (input$tp < 0) {
      name_cause_delayed = paste0(name_cause_delayed, "_minus_", -input$tp)
    }
    
    this_df_clean = res_strength_causality_details_smap %>% 
      filter(id_timeseries == this_id, var_cause == key_var_cause(), var_consequence == key_var_consequence(), tp == input$tp,
             var_cause_jacobian == name_cause_delayed, var_consequence_jacobian == paste0(key_var_consequence(), "_t_plus_1")) %>% 
      # filter(id_timeseries == this_id, var_cause == "sst.z", var_consequence == "prodbest.div", tp == -1, 
      #        var_cause_jacobian == "sst.z_t_minus_1", var_consequence_jacobian == "prodbest.div_t_plus_1") %>% 
      filter(!is.na(value))
    
    # Fit a linear model for value ~ t
    lm_fit <- lm(value ~ t, data = this_df_clean)
    slope <- coef(lm_fit)[2]  # Slope of the linear model
    intercept <- coef(lm_fit)[1]  # Intercept
    p_value <- summary(lm_fit)$coefficients[2, 4]  # p-value for the slope
    
    plot_ly() %>%
      add_trace(x = ~this_df_clean$t, y = ~this_df_clean$value, 
                type = "scatter", mode = "markers+lines", 
                marker = list(size = 13, opacity = 0.6), line = list(width = 1, opacity = 0.6),
                name = "Strength of causality") %>%
      # Add a horizontal dashed line for the mean value
      add_trace(x = c(min(this_df_clean$t), max(this_df_clean$t)), y = rep(mean(this_df_clean$value, na.rm = TRUE), 2),
                type = "scatter", mode = "lines", line = list(color = "#ee8c2f", dash = "dot", width = 2),
                name = "Mean", hoverinfo = "text", text = paste("Mean: ", round(mean(this_df_clean$value, na.rm = TRUE), 4)) ) %>%
      # Fitted values from the linear model
      add_trace(x = ~this_df_clean$t, y = fitted(lm_fit), type = "scatter", mode = "lines", 
                line = list(color = ifelse(p_value >= 0.05, "#BBBBBB", ifelse(slope > 0, "#97d89a", "#de7371")),
                            width = 2, dash = "dash"), name = "Linear Fit", hoverinfo = "text",
                text = ~paste("Slope: ", round(slope, 4), "<br>p-value: ", formatC(p_value, format = "e", digits = 2)) ) %>%
      layout(title = paste0("Strength of causality - ", this_id), 
      # layout(title = paste0("Strength of causality - ", this_id, 
                            # "\nFrom ", input$var_cause, " to ", input$var_consequence),
             xaxis = list(title = "Year"), 
             yaxis = list(title = "Strength of causality (S-map)", side = "left", showgrid = TRUE, zeroline = TRUE),
             margin = list(t = 50, b = 70),
             legend = list(orientation = "h", x = 0.5, y = -0.4, xanchor = "center", yanchor = "bottom"))
    
  })
  
  
  
}



# Run the application 

# shinyApp(ui = ui, server = server)
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))




