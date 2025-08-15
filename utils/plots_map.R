library(ggplot2)
library(dplyr)
library(sf)        # For handling shapefiles
library(ggforce)   # For drawing pies
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

library(tidyverse)

# devtools::install_github("GuangchuangYu/scatterpie")
library(scatterpie)


# Dataset

# df_test = read.csv("data/datasets/df_timeseries_full-v4.66.csv") %>%
#   filter(!is.na(sst.z), !is.na(prodbest.divTB), !is.na(UdivUmsypref))
# id_timeseries = "stockid"
# var_pie = "FisheryType"
# label_var_pie = "Fishery Type"

ggplot_map_piechart = function(data, id_timeseries, var_pie, label_var_pie = NULL, coef_r = 1) {
  
  if (is.null(label_var_pie)) {
    label_var_pie = var_pie
    }
  
  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  region_coords = data.frame(
    region = c("Other", "US Alaska", "Canada West Coast", "US West Coast", "US Southeast and Gulf",
               "US East Coast", "South America", "Canada East Coast", "Atlantic Ocean", "Mediterranean-Black Sea",
               "European Union", "Southern Africa", "Europe non EU", "Indian Ocean", "Japan",
               "Australia", "Pacific Ocean", "New Zealand"),
    lon = c(-200, -160, -135, -125, -90,   -75, -60, -60, -30, 15,    0,  15, 22,  80, 140,   140, 160, 170),
    lat = c( -70,   60,   55,   35,  25,    38, -20,  50,  20, 37,   50, -35, 52, -10,  35,   -30,   10, -40)
  ) %>% 
    filter(region %in% c("Other", unique(data$region)))
  
  data_legend = data.frame(
    region = c("US Alaska", "Canada West Coast", "US West Coast", "US Southeast and Gulf",
               "US East Coast", "South America", "Canada East Coast", "Atlantic Ocean", "Mediterranean-Black Sea",
               "European Union", "Southern Africa", "Europe non EU", "Indian Ocean", "Japan",
               "Australia", "Pacific Ocean", "New Zealand"),
    lon_legend = c(-177, -120, -155, -90,   -60, -83, -60, -30, 35,  -17,  15, 37,  67, 140,   126, 146, 170),
    lat_legend = c(  65,   61,   23,  12,    28, -29,  62,   6, 28,   57, -23, 57,  -5,  47,   -25,  16, -54)
  ) %>% 
    filter(region %in% unique(data$region))
  
  order_var_pie = levels(factor(data[[var_pie]]))
  
  plot_data <- data %>%
    group_by_at(id_timeseries) %>%
    slice_tail(n = 1) %>% # keep only the last value of the time series
    ungroup() %>%
    group_by(region, !!sym(var_pie)) %>%
    summarize(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = !!sym(var_pie), values_from = count, values_fill = 0) %>% 
    left_join(region_coords, by = "region")
  
  groups = order_var_pie
  
  nbr_stocks_per_region = data.frame(
    region = unique(plot_data$region),
    nbr_stocks = rowSums(dplyr::select(plot_data, -region, -lon, -lat))
  )
  
  plot_data2 = left_join(plot_data, nbr_stocks_per_region, by = "region") %>% 
    mutate(r = sqrt(nbr_stocks) * 2 * coef_r)
  
  plot_data2 = plot_data2 %>% 
    mutate(region_nbr = paste0(region, "\n(n = ", nbr_stocks, ")"))#,
           #!!sym(var_pie) := factor(!!sym(var_pie), levels = order_var_pie))
  
  data_legend = data_legend %>% 
    left_join(nbr_stocks_per_region, by = "region") %>%
    mutate(region_legend = paste0(region, "\n(n = ", nbr_stocks, ")"))
  # print(data_legend)
  # print(plot_data2 %>% filter(region == "Other"))
  
  # Plot
  margin_rect = 5*(coef_r-1)
  p = ggplot(data = world) +
    geom_sf(fill = "gray90", color = "white") +
    geom_scatterpie(data = plot_data2, 
                    aes(x = lon, y = lat, group = region, r = r),  # Adjust `r` for pie size
                    cols = groups,  # Use correctly extracted column names
                    color = NA, alpha = 0.8) +
    # geom_text_repel(data = plot_data2, 
    #                 aes(x = lon, y = lat, label = region_nbr),  # Position labels correctly above the pie charts
    #                 size = 2, box.padding = 0.5, max.overlaps = 10,
    #                 segment.color = NA) + 
    geom_text(data = data_legend, aes(x = lon_legend, y = lat_legend, label = region_legend), size = 2) +
    coord_sf() +
    annotate("rect", xmin = -221-3*margin_rect, xmax = -159+3*margin_rect, ymin = -35-margin_rect, ymax = 5+margin_rect, 
             fill = "white", color = "black") +
    geom_scatterpie_legend(plot_data2$r, x = -200, y = -20, size = 3,
                           breaks = sqrt(c(5, 15, 30))*2, labeller = function(x) (x/2)**2) +
    # Add title for legend
    annotate("text", x = -190, y = -3, label = "Number of Stocks", size = 3, hjust = 0.5, vjust = 0) +
    labs(title = paste0("Distribution of ", label_var_pie, " by Region"),
         fill = label_var_pie) +  # Title for the fill legend
    theme_minimal() + 
    theme(legend.position = "bottom", 
          axis.title = element_blank())
  
  return(p)
  
}
