
library(tidyverse)
library(ggalluvial)
library(ggplot2)

save_plots = TRUE

# Import data -------------------------------------------------------------


summary.prod = read.csv(paste0("../fishstocks_sensitivity/out/20240906-simplex_smap_ccm/", "computations/out_strength_conf_ccm_smap.csv"))
summary.R = read.csv(paste0("../fishstocks_sensitivity/out/20240925-recruitment/", "computations/out_strength_conf_ccm_smap.csv"))

load("../fishstocks_sensitivity/data/data_timeseries.RData")

stocks = read.csv("../fishstocks_sensitivity/data/stock.csv")
# View(stocks)

stocks_in_pierre_et_al = c(
  "SABLEFEBSAIGA", # Anoplopoma fimbria
  "HERRSIRS", "HERRVIaVIIbc", "HERRNIRS", "HERRNORSS", "HERRNS-IIIa-VIId", # Clupea harengus
  "HERRCC", "HERRPRD", "HERRHG", "HERRSOG", "HERRWCVANI", # Clupea harengus
  "PANCHPERUNC", # Engraulis ringens
  "PSOLEPCOAST", # Eopsetta jordani (both merged)
  "CODBA2532", "CODNEAR", "CODVIIek", "CODFABNK", "CODFAPL", "CODICE", "CODIS", "CODIIIaW-IV-VIId", "COD3NO", # Gadus morhua
  "STMARLINSWPO", # Kajikia audax
  "RSOLEHSTR", # Lepidopsetta bilineata
  "HADNEAR", "HADFAPL", "HADNS-IIIa-IV", # Melanogrammus aeglefinus
  "PHAKEPCOAST", # Merluccius productus
  "GAGSATLC", # Mycteroperca microlepis
  "MORWONGSE", # Nemadactylus macropterus
  "ESOLEPCOAST", # Parophrys vetulus
  "PLAICIS", "PLAICNS", # Pleuronectes platessa
  "POLLNEAR", "POLLFAPL", # Pollachius virens
  "CMACKPCOAST", # Scomber japonicus
  "MACK5YCHATT", # Scomber scombrus
  "POPERCHPCOAST", # Sebastes alutus
  "GOPHERSPCOAST", # Sebastes carnatus
  "WROCKPCOAST", # Sebastes entomelas
  "CHILISPCOAST", # Sebastes goodei
  "BOCACCSPCOAST", # Sebastes paucispinis
  "CROCKPCOAST", # Sebastes pinniger
  "NROCKGA", # Sebastes polyspinis
  # , # Solea vulgaris
  "SPRBLKGSA29", # Sprattus sprattus
  "WPOLLEBS", # Theragra chalcogramma
  "ALBANATL", "ALBASPAC", # Thunnus alalunga
  "YFINCWPAC", # Thunnus albacares
  "BIGEYECWPAC", # Thunnus obesus
  "ATBTUNAEATL" # Thunnus thynnus
)

causality_found_pierre_glmm = c( # From Tab_CCM_GLMM_Appendix.pdf
  "HERRNORSS", "HERRNS-IIIa-VIId", # Clupea harengus
  "CODVIIek", "CODFABNK", "CODFAPL", "CODICE", "CODIS", "CODIIIaW-IV-VIId", # Gadus morhua
  "HADNEAR", "HADFAPL", "HADNS-IIIa-IV", # Melanogrammus aeglefinus
  "PLAICIS", "PLAICNS", # Pleuronectes platessa
  "POLLNEAR", "POLLFAPL", # Pollachius virens
  # , # Solea vulgaris
  "ATBTUNAEATL" # Thunnus thynnus
)
causality_found_pierre_kendall_tested = c( # From Tab_MK_Appendix.pdf
  "HERRNIRS", "HERRNORSS", # Clupea harengus
  "CODBA2532", "CODNEAR", "CODFABNK", "CODFAPL", "CODICE", "CODIS", # Gadus morhua
  "HADNEAR", "HADFAPL", "HADNS-IIIa-IV", # Melanogrammus aeglefinus
  "PLAICIS", "PLAICNS", # Pleuronectes platessa
  "POLLNEAR", "POLLFAPL", # Pollachius virens
  # , # Solea vulgaris
  "ATBTUNAEATL" # Thunnus thynnus
)
causality_found_pierre_kendall = c( # From Tab_MK_Appendix.pdf
  "HERRNORSS", # Clupea harengus
  "CODBA2532", "CODNEAR", "CODICE", "CODIS", # Gadus morhua
  "PLAICIS", "PLAICNS", # Pleuronectes platessa
  "POLLNEAR", "POLLFAPL", # Pollachius virens
  # , # Solea vulgaris
  "ATBTUNAEATL" # Thunnus thynnus
)



df_compare_prod_R = full_join(
  summary.prod %>% 
    filter(var_lib == "prodbest.div", var_target == "sst.z") %>% 
    dplyr::select(stockid, E, causality) %>% 
    rename(E_prod = E, causality_prod = causality),
  summary.R %>%
    filter(var_lib == "R", var_target == "sst.z") %>%
    dplyr::select(stockid, E, causality) %>%
    rename(E_R = E, causality_R = causality),
  by = c("stockid")
)



# Plot comparison prod and R within our method  ---------------------------


df_compare_prod_R %>%
  group_by(causality_prod, causality_R) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  
  ggplot(aes(x = causality_prod, y = causality_R, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "white") +
  labs(x = "Causality SST to productivity", y = "Causality SST to recruitment",
       title = "Comparison of causality using our method",
       subtitle = "From SST to productivity or recruitment") +
  theme_light() +
  theme(legend.position = "none")

if (save_plots) {
  ggsave("out/compare_pierre_et_al/01-table-prod_R-our_method.png", width = 5, height = 4)
}



df_compare_prod_R %>%
  group_by(causality_prod, causality_R) %>%
  summarise(n = n(), .groups = "drop") %>% 
  
  ggplot(aes(axis1 = causality_prod, axis2 = causality_R, y = n)) +
  geom_alluvium(aes(fill = causality_prod), width = 0.2, knot.pos = 0.3) +
  geom_stratum(width = 0.3, fill = "gray70", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), color = "black") +
  geom_text(aes(label = n), stat = "alluvium", size = 3, color = "white", hjust = 0.5) +
  labs(x = "Causality SST", y = "Count", 
       title = "Comparison of causality using our method",
       subtitle = "From SST to productivity or recruitment") +
  scale_x_discrete(limits = c("Causality SST to productivity", "Causality SST to recruitment"), 
                   expand = c(0.15, 0.15)) +
  theme_light() +
  theme(legend.position = "none")

if (save_plots) {
  ggsave("out/compare_pierre_et_al/01-alluvial-prod_R-our_method.png", width = 6, height = 5)
}





# Plot comparison prod and R our method (only stocks Pierre et al.) -------

df_compare_prod_R %>%
  filter(stockid %in% causality_found_pierre_kendall_tested) %>%
  group_by(causality_prod, causality_R) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  
  ggplot(aes(x = causality_prod, y = causality_R, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "white") +
  labs(x = "Causality SST to productivity", y = "Causality SST to recruitment",
       title = "Comparison of causality using our method",
       subtitle = "From SST to productivity or recruitment (stocks in Pierre et al.)") +
  theme_light() +
  theme(legend.position = "none")

if (save_plots) {
  ggsave("out/compare_pierre_et_al/02-table-prod_R-our_method-filter_stocks_pierre.png", width = 5, height = 4)
}



df_compare_prod_R %>%
  # filter(stockid %in% causality_found_pierre_kendall_tested) %>%
  group_by(causality_prod, causality_R) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  
  ggplot(aes(axis1 = causality_prod, axis2 = causality_R, y = n)) +
  geom_alluvium(aes(fill = causality_prod), width = 0.2, knot.pos = 0.3) +
  geom_stratum(width = 0.3, fill = "gray70", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), color = "black") +
  geom_text(aes(label = n), stat = "alluvium", size = 3, color = "white", hjust = 0.5) +
  labs(x = "Causality SST", y = "Count", 
       title = "Comparison of causality using our method",
       subtitle = "From SST to productivity or recruitment (stocks in Pierre et al.)") +
  scale_x_discrete(limits = c("Causality SST to productivity", "Causality SST to recruitment"), 
                   expand = c(0.15, 0.15)) +
  theme_light() +
  theme(legend.position = "none")

if (save_plots) {
  ggsave("out/compare_pierre_et_al/02-alluvial-prod_R-our_method-filter_stocks_pierre.png", width = 6, height = 5)
}



# Plot comparison R between our method and Pierre et al -------------------

df_compare_prod_R %>%
  filter(stockid %in% causality_found_pierre_kendall_tested) %>% 
  mutate(causality_pierre = stockid %in% causality_found_pierre_kendall) %>% 
  group_by(causality_pierre, causality_R) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  
  ggplot(aes(x = causality_pierre, y = causality_R, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "white") +
  labs(x = "Causality SST to recruitment (Pierre et al)", y = "Causality SST to recruitment (us)",
       title = "Comparison of causality between\nour method and Pierre et al",
       subtitle = "From SST to recruitment") +
  theme_light() +
  theme(legend.position = "none")

if (save_plots) {
  ggsave("out/compare_pierre_et_al/03-table-R-compare_methods.png", width = 4, height = 4)
}



df_compare_prod_R %>%
  filter(stockid %in% causality_found_pierre_kendall_tested) %>% 
  mutate(causality_pierre = stockid %in% causality_found_pierre_kendall) %>% 
  group_by(causality_pierre, causality_R) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  
  ggplot(aes(axis1 = causality_pierre, axis2 = causality_R, y = n)) +
  geom_alluvium(aes(fill = causality_pierre), width = 0.2, knot.pos = 0.3) +
  geom_stratum(width = 0.3, fill = "gray70", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), color = "black") +
  geom_text(aes(label = n), stat = "alluvium", size = 3, color = "white", hjust = 0.5) +
  labs(x = "Causality SST to recruitment (Pierre et al)", y = "Count", 
       title = "Comparison of causality between our method and Pierre et al",
       subtitle = "From SST to recruitment") +
  scale_x_discrete(limits = c("Causality SST to recruitment (Pierre et al)", "Causality SST to recruitment (us)"), 
                   expand = c(0.15, 0.15)) +
  theme_light() +
  theme(legend.position = "none")

if (save_plots) {
  ggsave("out/compare_pierre_et_al/03-alluvial-R-compare_methods.png", width = 6, height = 5)
}
  
  


