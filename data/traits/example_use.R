rm(list = ls())

library(dplyr)
library(tidyverse)
library(ggplot2)

library(FishLife)
library(rfishbase)

# # install package from https://github.com/James-Thorson-NOAA/FishLife
# devtools::install_github("James-Thorson-NOAA/FishLife")

# installed.packages()[sort(installed.packages()[,1]),c(1,3)]


# version_RAM = "v4.64"
version_RAM = "v4.66"

load(paste0("data/RAMLDB v4.64/R Data/DBdata[asmt][", version_RAM, "].RData"))
rm(list = ls()[!ls() %in% c("bioparams", "timeseries_values_views", "timeseries", "stock")])

# source("data/traits/traits_function.R")
source("data/traits/traits_fun_simplif.R")




# Load data .rds
traits_ini <- readRDS("data/traits/traj_SProd.rds")
# traj_SProd <- traits_ini

traits = traits_fun(traits_ini)

saveRDS(traits, paste0("data/traits/traits_export_simplif-", version_RAM, ".rds"))


# library(GGally)
# ggpairs(traits, columns = 3:10, title = "Traits of fish species")
# ggsave("out/traits_ggpairs.png", width = 10, height = 10)



# df.mat = maturity(stock$scientificname)
# summary(df.mat)

