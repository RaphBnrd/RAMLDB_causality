library(tidyverse)

rm(list = ls())




# Import data -------------------------------------------------------------

load("../fishstocks_sensitivity/data/RAMLDB v4.64/R Data/DBdata[asmt][v4.64].RData")
# df.all <- read.csv("../fishstocks_sensitivity/data/data_SST/1e_all_had_sst.csv")
# df.mean <- read.csv("../fishstocks_sensitivity/data/data_SST/1e_mean_had_sst.csv")
# df.monthly <- read.csv("../fishstocks_sensitivity/data/data_SST/1e_monthly_had_sst.csv")
# df.seasonal <- read.csv("../fishstocks_sensitivity/data/data_SST/1e_seasonal_had_sst.csv")
SST.yearly <- read.csv("../fishstocks_sensitivity/data/data_SST/1e_yearly_had_sst.csv")

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

# Join the SST with the timeseries data (fill with NA if an element of SST.yearly is not in timeseries_values_views)
df.ini <- full_join(timeseries_values_views, SST.yearly, 
                    by = c("stockid" = "stockid", "year" = "year", "stocklong" = "stocklong"))



# Compute the productivity
df.ini = df.ini %>% 
  rename(sst = sst_c) %>%
  arrange(stockid, year) %>% 
  group_by(stockid) %>% 
  mutate(year.diff = lead(year) - year)
# We will check if the years are consecutive
df.ini %>% 
  filter(year.diff != 1) %>% 
  select(stockid, year, year.diff) %>% 
  distinct() %>% 
  pull(stockid) %>%
  unique() # The following ID have empty data, and 2 assessements but empty... So we remove it
id.problem = c("HADNS", "HERR30", "HERRNS", "SOLEIIIa")
df.ini %>% 
  filter(stockid %in% id.problem) %>% 
  arrange(stockid, year) %>%
  summary() # We see only NA's here...


df.ini = df.ini %>% 
  filter(!stockid %in% id.problem) %>%
  mutate(prod = ifelse(year.diff == 1, lead(TB) - TB + TC, NA), # If it is the case, Prod(t) = TB(t+1) - TB(t) + TC(t)
         prodbest = ifelse(year.diff == 1, lead(TBbest) - TBbest + TCbest, NA)) %>% 
  ungroup()

# df.ini = df.ini %>%
#   mutate( # standardize the data for each stock
#     prod.norm = ifelse(is.na(prod), NA, (prod - mean(prod, na.rm = TRUE)) / sd(prod, na.rm = TRUE)),
#     TB.norm = ifelse(is.na(TB), NA, (TB - mean(TB, na.rm = TRUE)) / sd(TB, na.rm = TRUE)),
#     TC.norm = ifelse(is.na(TC), NA, (TC - mean(TC, na.rm = TRUE)) / sd(TC, na.rm = TRUE)),
#     UdivUmsypref.norm = ifelse(is.na(UdivUmsypref), NA, (UdivUmsypref - mean(UdivUmsypref, na.rm = TRUE)) / sd(UdivUmsypref, na.rm = TRUE)),
#     sst.norm = ifelse(is.na(sst), NA, (sst - mean(sst, na.rm = TRUE)) / sd(sst, na.rm = TRUE))
#   )

# df = df.ini %>% 
#   filter(!is.na(sst), !is.na(TB), !is.na(TC), !is.na(UdivUmsypref))


# ADD INFORMATIONS ON THE STOCKS

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






# Save the data
save(df.ini, file = "data/data_timeseries.RData")



