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


# Load the RAM Legacy Database --------------------------------------------

# version_RAM = "v4.64"
version_RAM = "v4.66"

load(paste0(
  "data/RAMLDB ", version_RAM, "/R Data/DBdata[asmt][", version_RAM, "].RData"
))


# Load the SST data on fishstock boundaries -------------------------------

# It has been computed using HAD for SST and Christopher Free’s website for the fishstock boundaries
SST.yearly <- read.csv("data/SST_on_fishstocks-1e_yearly_had_sst.csv")

# For each element in SST.yearly$assessid extract the substr between the first and the second "-"
SST.yearly$stockid = sapply(strsplit(SST.yearly$assessid, "-"), function(x) x[2])
# If the assessid starts with an element of other_start_pattern, select the substr between the second and the third "-"
other_start_pattern = c(
  "CNR-IAMC-", "DFO-ARCTIC-", "DFO-MAR-", "DFO-NFLD-", "DFO-PAC-", "DFO-QUE-", 
  "DFO-SG-", "EWG-BS-", "FAO-DRSS-", "FAO-SPNWA-", "NAFO-SC-"
)
SST.yearly$stockid = ifelse(
  grepl(paste(other_start_pattern, collapse="|"), SST.yearly$assessid), 
  sapply(strsplit(SST.yearly$assessid, "-"), function(x) x[3]), 
  SST.yearly$stockid
)
SST.yearly = SST.yearly %>% 
  left_join(
    timeseries_values_views %>% 
      dplyr::select(stockid, stocklong) %>% 
      distinct(),
    by = "stockid"
  )


# Merge the data ----------------------------------------------------------

# Join the SST with the timeseries data (fill with NA if an element of SST.yearly is not in timeseries_values_views)
df.ini <- full_join(
  timeseries_values_views, 
  SST.yearly, 
  by = c("stockid", "year", "stocklong")
)


# Clean and process the dataframes ----------------------------------------

# Compute the surplus of productivity

# First we check if the years are consecutive (within each stock)
df.ini = df.ini %>% 
  rename(sst = sst_c) %>%
  arrange(stockid, year) %>% 
  group_by(stockid) %>% 
  mutate(year.diff = lead(year) - year) %>% 
  ungroup()
# The following ID have empty data, and 2 assessments but empty... So we remove it
id.problem = df.ini %>% 
  filter(year.diff != 1) %>% 
  dplyr::select(stockid, year, year.diff) %>% 
  distinct() %>% 
  pull(stockid) %>%
  unique()

# id.problem should be those manually identified manually in id.problem.expected
id.problem.expected = c("HADNS", "HERR30", "HERRNS", "SOLEIIIa")
if (!identical(id.problem, id.problem.expected)) {
  stop("The stockids with non-consecutive years are not the ones expected")
} else {
  message("The stockids with non-consecutive years are the ones expected")
}

message("Here is the summary rows with a problem (we only see NA's): ")
# df.ini %>% 
#   filter(stockid %in% id.problem) %>% 
#   arrange(stockid, year) %>%
#   summary() # We see only NA's here...

# For the stocks without problem, we compute the surplus of productivity
df.ini = df.ini %>% 
  filter(!stockid %in% id.problem) %>%
  # prod(t) = TB(t+1) - TB(t) + TC(t)
  mutate(prod = ifelse(year.diff == 1, lead(TB) - TB + TC, NA),
         prodbest = ifelse(year.diff == 1, lead(TBbest) - TBbest + TCbest, NA)) %>% 
  ungroup()

df.ini = df.ini %>%
  group_by(stockid) %>%
  mutate( # standardize the data for each stock (divide by TB or z-score for SST)
    prod.divTB = prod / mean(TB, na.rm = TRUE),
    prodbest.divTB = prodbest / mean(TBbest, na.rm = TRUE),
    sst.z = (sst - mean(sst, na.rm = TRUE)) / sd(sst, na.rm = TRUE)
  ) %>%
  ungroup()

# Check if year.diff is either NA or 1
if (!all(is.na(df.ini$year.diff) | df.ini$year.diff == 1)) {
  stop("There are some year.diff that are not NA or 1")
} else {
  message("All year.diff are either NA or 1")
}
# If so, drop the year.diff column
df.ini = df.ini %>% 
  select(-year.diff)


# Add information on the stocks -------------------------------------------

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


# Save the data -----------------------------------------------------------

# As a csv
write.csv(
  df.ini, 
  paste0("data/timeseries_clean-", version_RAM, ".csv"),
  row.names = FALSE
)

# As a RData
save(df.ini, file = paste0("data/timeseries_clean-", version_RAM, ".RData"))
