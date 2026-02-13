# RAMLDB_causality

## Packages

The analyses are performed in `R` version 4.4.2. 

The main package used is `rEDM` package version 1.15.4. It is availabele in [this GitHub repository](https://github.com/ha0ye/rEDM) and can be downloaded using `devtools::install_github("ha0ye/rEDM")` in `R`console. 

Overall, versions of the packages used in the analysis are as follows:
 - `tidyverse`: 2.0.0
 - `tidyr`: 1.3.1
 - `jsonlite`: 2.0.0
 - `progress`: 1.2.3
 - `ggplot2`: 4.0.0
 - `ggtext`: 0.1.2
 - `ggrepel`: 0.9.6
 - `RColorBrewer`: 1.1.3
 - `rEDM`: 1.15.4
 - `Kendall`: 2.2.1


## Preprocess dataset

### Fisheries data

The fisheries data is from the [RAMLDB database](https://www.ramlegacy.org/database/). In particular, we use the version 4.66, which is the latest version as of February 2026, available at https://zenodo.org/records/14043031. 

The unzipped folder `RAMLDB v4.66` should be placed in the `data` folder of this repository. 

SST data are taken from the [Met Office HadISST1 dataset](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2002jd002670) and matched to the RAMLDB data using stock boundaries available at https://chrismfree.com/ram-legacy-stock-boundary-database/ (yearly average over the stock geographical boundary). The resulting SST per year per stock is available in `data/SST_on_fishstocks-1e_yearly_had_sst.csv`.

The preprocessing is performed using the `utils/00-prepare_data.R` script. It consists in building a dataframe containing: 
- Stock identifier: `stockid`
- Stock information: `stocklong`, `scientificname`, `commonname`, `areaid`, `region`, `FisheryType`, `assessid`
- Taxonomic information: `kingdom`, `phylum`, `classname`, `ordername`, `family`, `genus`
- Time: `year`
- Timeseries from RAMLDB: `TBbest`, `TCbest`, `ERbest`, `BdivBmsypref`, `UdivUmsypref`, `BdivBmgtpref`, `UdivUmgtpref`, `TB`, `SSB`, `TN`, `R`, `TC`, `TL`, `RecC`, `F`, `ER`, `TBdivTBmsy`, `SSBdivSSBmsy`, `NdivNmsy`, `FdivFmsy`, `ERdivERmsy`, `CdivMSY`, `CdivMEANC`, `TBdivTBmgt`, `SSBdivSSBmgt`, `NdivNmgt`, `survBdivsurvBmgt`, `FdivFmgt`, `ERdivERmgt`, `Cpair`, `TAC`, `Cadvised`, `survB`, `CPUE`, `EFFORT`, `prod`, `prodbest`, `prod.divTB`, `prodbest.divTB`, `sst.z`.
- Timeseries of SST (initial or standardized): `sst`, `sst.z`.

Each row in the dataframe corresponds to a stock-year.

The preprocessed data is available as csv or RData files: `data/timeseries_clean-v4.66.csv` and `data/timeseries_clean-v4.66.RData`.

### Traits data

Traits RDS data are available in `data/traits/traits_export_simplif.rds`. 

## Utils

## Main analysis

## Comparison with Pierre et al. 's analysis
