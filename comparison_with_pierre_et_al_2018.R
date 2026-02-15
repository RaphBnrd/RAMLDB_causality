# COMPARISON WITH THE RESULTS FROM: 
# Pierre, M., Rouyer, T., Bonhommeau, S., & Fromentin, J. M. (2018). 
# Assessing causal links in fish stock–recruitment relationships. 
# ICES Journal of Marine Science, 75(3), Article 3. 
# https://doi.org/10.1093/icesjms/fsx202

library(tidyverse)

rm(list=ls())

# Current results ---------------------------------------------------------

res.ccm.prod.assessment = read.csv(paste0(
      "out/20260215_141155-on_SProd/",
      "csv/all_res.csv"
    )) %>% 
  filter(var_cause == "SST", var_consequence == "SProd")
res.ccm.R.assessment = read.csv(paste0(
    "out/20260215_143248-on_Recruitment/",
    "csv/all_res.csv"
  )) %>% 
  filter(var_cause == "SST", var_consequence == "Recruitment")


# Pierre et al.'s results -------------------------------------------------

# Assessing causality (see Pierre et al. 2018): 
# "a Mann-Kendall test for monotonic trend (Mann, 1945) was applied for each 
# stock to detect a potential lack of causation. The correlation coefficient 
# returned by the test gave an indication on the trend: a positive value 
# indicated an increasing trend whereas a negative value indicated a decreasing 
# trend. Stocks characterized by a significant correlation coefficient lower 
# than 0.95 displayed a causal link."
# So we use the Kendall tau and p-value from supplementary materials "Tab_MK_Appendix.pdf"

df_comparison = data.frame(
  species = c(
    "Clupea harengus", "Clupea harengus", 
    "Gadus morhua", "Gadus morhua", "Gadus morhua", "Gadus morhua", "Gadus morhua", "Gadus morhua", 
    "Melanogrammus aeglefinus", "Melanogrammus aeglefinus", "Melanogrammus aeglefinus",
    "Pleuronectes platessa", "Pleuronectes platessa", "Pollachius virens", "Pollachius virens",
    "Solea vulgaris", "Thunnus thynnus"
  ),
  stock_them = c(
    "Irish Sea (VIIa)", "Norwegian spring spawning (NSS)", 
    "Baltic (25-32)", "Barents", "Faroe", "Iceland", "Irish Sea (VIIa)", "North Sea", 
    "Arctic", "Faroe", "North Sea", 
    "Irish Sea (VIIa)", "North Sea", "Arctic", "Faroe", 
    "North Sea", "Eastern Atlantic"
  ),
  stock_us = c(
    "HERRNIRS", "HERRNORSS", 
    "CODBA2532", "CODNEAR", "CODFAPL", "CODICE", "CODIS", "CODIIIaW-IV-VIId", 
    "HADNEAR", "HADFAPL", "HADNS-IIIa", 
    "PLAICIS", "PLAICNS", "POLLNEAR", "POLLFAPL", 
    "", "ATBTUNAEATL"
  ),
  cause_R_tau_them = c(
    -0.9, -1, 
    -1, -1, -0.62, -0.93, -1, -1, 
    -0.89, -0.9, -0.73, 
    -1, -1, -1, -1, 
    -0.79, -1
  ),
  cause_R_pval_them = c(
    .01, 2.62e-5,
    .01, 2.63e-4, .07, 1.98e-3, .01, .01,
    1.23e-3, .01, .06,
    .01, 2.67e-3, .03, 2.67e-3,
    .01, 2.63e-4
  )
) %>% 
  mutate(cause_R_them = (cause_R_pval_them < 0.05) & (cause_R_tau_them < -.95))
                               

# Merge the results -------------------------------------------------------

df_comparison = df_comparison %>% 
  left_join(res.ccm.prod.assessment %>% 
              dplyr::select(stockid, causality, CCM_kendall_tau, CCM_kendall_pval) %>% 
              mutate(causality = ifelse(causality == "Yes", TRUE, ifelse(causality == "No", FALSE, causality))) %>%
              rename(cause_prod_us = causality, cause_prod_tau_us = CCM_kendall_tau, cause_prod_pval_us = CCM_kendall_pval),
            by = c("stock_us" = "stockid")) %>%
  left_join(res.ccm.R.assessment %>% 
              dplyr::select(stockid, causality, CCM_kendall_tau, CCM_kendall_pval) %>% 
              mutate(causality = ifelse(causality == "Yes", TRUE, ifelse(causality == "No", FALSE, causality))) %>%
              rename(cause_R_us = causality, cause_R_tau_us = CCM_kendall_tau, cause_R_pval_us = CCM_kendall_pval),
            by = c("stock_us" = "stockid"))

write.csv(df_comparison, "out/comparison_with_pierre_et_al_2018.csv", row.names = FALSE)


# Latex table -------------------------------------------------------------

generate_latex_table <- function(df) {
  cat("\\begin{table}[h]\n",
      "    \\centering\n",
      "    \\begin{tabular}{l l l c c c}\n",
      "        \\toprule\n",
      "        \\textbf{Species} & \\textbf{Stock (Pierre et al.)} & \\textbf{Stock (RAM)} & \\textbf{SST to R (Pierre et al.)} & \\textbf{SST to R (here)} & \\textbf{SST to SProd (here)} \\\\\n",
      "        \\midrule\n", sep = "")
  apply(df, 1, function(row) {
    cat("        ", paste(row, collapse = " & "), " \\\\\n", sep = "")
  })
  cat("        \\bottomrule\n",
      "    \\end{tabular}\n",
      "    \\caption{Causal relationships between stocks}\n",
      "    \\label{tab:causal_relationships}\n",
      "\\end{table}\n")
}

df_comparison %>%
  dplyr::select(species, stock_them, stock_us, cause_R_them, cause_R_us, cause_prod_us) %>% 
  generate_latex_table()

# df_comparison %>%dplyr::select(species, stock_them, stock_us, cause_R_them, cause_R_us, cause_prod_us) %>% View()
