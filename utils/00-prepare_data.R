# This script prepares the data for the analysis. 
# It creates the dataframe containing the timeseries data, with
# one row per stock per year, and the following columns:
# - stockid: the stock identifier
# - year: the year
# - timeseries of interest, here: 
#     . TB: Total biomass
#     . TC: Total catch
#     . UdivUmsypref: the ratio of the harvest rate to the harvest rate at the maximum sustainable yield
#     . sst: sea surface temperature (yearly average on the fishstock boundaries)
#     . prod: surplus of productivity (prod(t) = TB(t+1) - TB(t) + TC(t))
#     . prodbest: surplus of productivity (prodbest(t) = TBbest(t+1) - TBbest(t) + TCbest(t))
# 

library(tidyverse)
rm(list = ls())


# * * * Load the RAM Legacy Database * * *

version_RAM = "v4.64"
version_RAM = "v4.66"
load(paste0("./data/RAMLDB ", version_RAM, "/R Data/DBdata[asmt][", version_RAM, "].RData"))


# * * * Load the SST data on fishstock boundaries * * *

# It has been computed using HAD for SST and Christopher Free’s website for the fishstock boundaries
# df.all <- read.csv("../fishstocks_sensitivity/data/data_SST/1e_all_had_sst.csv")
# df.mean <- read.csv("../fishstocks_sensitivity/data/data_SST/1e_mean_had_sst.csv")
# df.monthly <- read.csv("../fishstocks_sensitivity/data/data_SST/1e_monthly_had_sst.csv")
# df.seasonal <- read.csv("../fishstocks_sensitivity/data/data_SST/1e_seasonal_had_sst.csv")
SST.yearly <- read.csv("./data/SST_on_fishstocks/1e_yearly_had_sst.csv")

# For each element in SST.yearly$assessid extract the substr between the first and the second "-"
SST.yearly$stockid = sapply(strsplit(SST.yearly$assessid, "-"), function(x) x[2])
# If the assessid starts with an element of other_start_pattern, select the substr between the second and the third "-"
other_start_pattern = c("CNR-IAMC-", "DFO-ARCTIC-", "DFO-MAR-", "DFO-NFLD-", "DFO-PAC-", 
                        "DFO-QUE-", "DFO-SG-", "EWG-BS-", "FAO-DRSS-", "FAO-SPNWA-", "NAFO-SC-")
SST.yearly$stockid = ifelse(grepl(paste(other_start_pattern, collapse="|"), SST.yearly$assessid), 
                            sapply(strsplit(SST.yearly$assessid, "-"), function(x) x[3]), SST.yearly$stockid)
SST.yearly = SST.yearly %>% 
  left_join(timeseries_values_views %>% dplyr::select(stockid, stocklong) %>% distinct(),
            by = "stockid")


# * * * Merge the data * * *

# Join the SST with the timeseries data (fill with NA if an element of SST.yearly is not in timeseries_values_views)
df.ini <- full_join(timeseries_values_views, SST.yearly, 
                    by = c("stockid" = "stockid", "year" = "year", "stocklong" = "stocklong"))


# * * * Clean and process the datas-frame * * *

# Compute the surplus of productivity

# First we check if the years are consecutive (within each stock)
df.ini = df.ini %>% 
  rename(sst = sst_c) %>%
  arrange(stockid, year) %>% 
  group_by(stockid) %>% 
  mutate(year.diff = lead(year) - year) %>% 
  ungroup()
id.problem = df.ini %>% 
  filter(year.diff != 1) %>% 
  dplyr::select(stockid, year, year.diff) %>% 
  distinct() %>% 
  pull(stockid) %>%
  unique() # The following ID have empty data, and 2 assessements but empty... So we remove it

# It should be id.problem.expected
id.problem.expected = c("HADNS", "HERR30", "HERRNS", "SOLEIIIa")
if (!identical(id.problem, id.problem.expected)) {
  stop("The stockid with non-consecutive years are not the one expected")
} else {
  print("The stockid with non-consecutive years are the one expected")
}

print("Here is the summary rows with a problem (we only see NA's): ")
df.ini %>% 
  filter(stockid %in% id.problem) %>% 
  arrange(stockid, year) %>%
  summary() # We see only NA's here...

# For the stocks without problem, we compute the surplus of productivity
df.ini = df.ini %>% 
  filter(!stockid %in% id.problem) %>%
  mutate(prod = ifelse(year.diff == 1, lead(TB) - TB + TC, NA), # If it is the case, prod(t) = TB(t+1) - TB(t) + TC(t)
         prodbest = ifelse(year.diff == 1, lead(TBbest) - TBbest + TCbest, NA)) %>% 
  ungroup()

df.ini = df.ini %>%
  group_by(stockid) %>%
  mutate( # standardize the data for each stock
    prod.divTB = prod / mean(TB, na.rm = TRUE),
    prodbest.divTB = prodbest / mean(TBbest, na.rm = TRUE),
    sst.z = (sst - mean(sst, na.rm = TRUE)) / sd(sst, na.rm = TRUE)
  ) %>%
  ungroup()


# * * * Add informations on the stocks * * *

df.stock = stock %>% 
  dplyr::select(stockid, scientificname, commonname ,areaid, region) %>% 
  distinct()
df.taxonomy = taxonomy %>% 
  dplyr::select(scientificname, kingdom, phylum, classname, ordername, family, genus, FisheryType) %>%
  distinct()
df.info = df.stock %>% 
  left_join(df.taxonomy, by = c("scientificname" = "scientificname"))

df.ini = df.ini %>% 
  left_join(df.info, by = "stockid")


# * * * Save the data * * *

# Get the datetime of now
now = format(Sys.time(), "%Y%m%d_%H%M%S")

# As a csv
write.csv(df.ini, 
          paste0("./data/datasets/df_timeseries_full-", version_RAM, ".csv"),
          # paste0("./data/datasets/", now, "-df_timeseries_full-", version_RAM, ".csv"),
          row.names = FALSE)
# As a RData
save(df.ini, 
     file = paste0("./data/datasets/df_timeseries_full-", version_RAM, ".RData"))
     # file = paste0("./data/datasets/", now, "-df_timeseries_full-", version_RAM, ".RData"))

