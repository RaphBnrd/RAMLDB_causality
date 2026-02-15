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

The `plots_map.R` and `plots_timeseries.R` scripts contain functions to easily plot the dataset and the results of the analyses.

## Main analysis

The main analysis is performed in the `main.R` script. All the outputs of the analysis are stored in the `out/[suffix_name_exe]-[date_time_exe]/` folder.

> **The overall results of the causality analysis (CCM and S-map) are exported in `[output_folder]/csv/all_res.csv`.**
> This csv contains the results of the CCM and S-map analyses for each pair of variables and each stock. It contains the following columns:
> - `stockid`: stock identifier
> - `stocklong`: stock name
> - `scientificname`: scientific name of the species
> - `var_cause`: name of the variable considered as cause
> - `var_consequence`: name of the variable considered as consequence
> - `causality`: result of the causality test ("Yes", "No", or "Not relevant" if the Simplex projection got negative forecast skill for each embedding dimension tested)
> - `CCM_test_above_surrogates`: result of the test comparing the CCM results with the surrogate data (TRUE or FALSE)
> - `CCM_kendall_tau`: Kendall tau of the test on the original data
> - `CCM_kendall_pval`: p-value of the test on the original data
> - `S_map_mean_coef`: mean coefficient of the S-map analysis
> - `S_map_mean_ttest_pval`: p-value of the t-test on the S-map coefficients (testing whether the S-map coefficients are significantly different from 0)
> - `S_map_trend_slope`: linear slope of the trend in the S-map coefficients over time
> - `S_map_trend_pval`: p-value of the test on the trend in the S-map coefficients over time
> - `S_map_trend_R2`: R2 of the linear regression on the trend in the S-map coefficients over time

The sections of the code are explained below.

### Parameters of execution

You can set the random seed for reproducibility in this section of the script.

`suffix_name_exe` and `date_time_exe` are used to build the path to store the results of the analysis (subfolder of `out` folder). 

`list_of_causality_tested` is a list of the pairs of variables for which the causality will be tested (`list(c("cause1", "consequence1"), c("cause2", "consequence2"))`).

The execution of some section of the code can be long (e.g. ~20 min for the CCM analysis and ~1 min for the S-map analysis). Since the results are stored in the `out` folder, it is possible to skip some sections by importing previously stored results. This can be done by setting `import_CCM` and/or `import_smap` to `TRUE`. The path to the stored results is built from `suffix_name_exe` and `date_time_exe`. 

`path_dataframe_input` is the path to the preprocessed dataframe (e.g. `data/timeseries_clean-v4.66.csv`, see previous section).

`name_id_timeseries` is the name of the column in the dataframe that contains the stock identifier (e.g. `stockid`). `name_time` is the name of the column that contains the time (e.g. `year`). 

`ids_single_plots` is a vector of stockid for which single plots will be generated (e.g. time series of the stock, CCM results, etc.). 

`plots_with_titles` is a boolean to decide whether the generated plots should have titles or not. `types_plots`is a vector of the types of plots to generate (e.g. `c("pdf", "png")`).

Other parameters are automatically set-up in the code and are not necessary to be modified by the user.

### Import and plot dataset

The preprocessed dataset is imported and filtered (e.g. > 40 years of data). The list of stocks that will be used in the analysis is stored in `[output_folder]/csv/00-info_stocks.csv`.

The map of the stocks by time series length and the examples of time series are generated and stored in `[output_folder]/[plot_type]/fig1a-map_piechart_durations.[plot_type]` and `[output_folder]/[plot_type]/fig1b-timeseries_one_stock-[stockid].[plot_type]` respectively.

### Simplex projection

The simplex projection is performed for each stock and each variable of interest (e.g. `sst.z`, `prodbest.divTB`, `UdivUmsypref`). The results are stored in `[output_folder]/csv/01-res_E_opti.csv`. It contains the optimal embedding dimension `E_opti` and other information, for each stock and each variable of interest.

Plots are generated to show the results of the simplex projection (`figAdd1` for each stock and `figAdd2` for the distribution of $E_{\text{opti}}$).

### CCM

The CCM analysis is performed for each pair of variables in `list_of_causality_tested` and for each stock. The results are stored in `[output_folder]/csv/02-res_CCM.csv`. It contains the results of the CCM analysis agregated over the bootstraps (e.g. median and percentiles of $\rho$) for each pair of variables and each stock, for the observed data and for the surrogate data.

The results are plotted for the `ids_single_plots` stocks in `fig2`.

### Test causality

The tests are applied on the results of the CCM analysis to determine whether there is a significant causality between the variables (Kendall test on the original data and comparison with the surrogate data). The results are stored in `[output_folder]/csv/03-res_causality.csv`. It contains the results of the tests (e.g. p-values) for each pair of variables and each stock.

The results are plotted for the entire dataset in `fig4a` and `fig4b` (proportion of stocks with significant causality, and per variable pair).

### Strength of causality

The strength of causality is estimated using the S-map method for the pairs of variables with significant causality. The results are stored in `[output_folder]/csv/04-res_smap.csv`. It contains the results of the S-map analysis (e.g. optimal theta, coefficients) for each pair of variables and each stock.


### Test the s-map

The tests are applied on the results of the S-map analysis to determine whether there is a sign or trend in the coefficients of the S-map. The results are stored in `[output_folder]/csv/05-res_test_smap.csv`. It contains the results of the tests (e.g. p-values) for each pair of variables and each stock.

**The overall results of the causality analysis (CCM and S-map) are exported in `[output_folder]/csv/all_res.csv`.**

Plots are generated for the `ids_single_plots` stocks in `fig3` (smap coefficients over time). The overall results of the S-map analysis are plotted in `fig5` and `fig6`.

Supplementary figures are created in `figAdd4` and `figAdd7` to `figAdd11`.

### Traits

The traits of the stocks are analyzed to determine whether they are related to the presence or strength of causality. The results are plotted in `figAdd5` and `figAdd6`.

## Comparison with Pierre et al. 's analysis

The comparison with the analysis of Pierre et al. (2018) is performed in the `comparison_with_pierre_et_al_2018.R` script. The results are stored in `out/comparison_with_pierre_et_al_2018.csv`.
