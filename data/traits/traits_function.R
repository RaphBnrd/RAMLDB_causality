#' Extract stocks RAMLBD timeseries
#' (the RAMLBD should be loaded in the environment)
#'
#' @param id Short stock ID character.
#' @param ts_type Type of timeseries (TBbest, ERbest...) character.
#' @param drop_na Keep only years with all types of timeseries available.
#'
#' @return Data frame of the selected timeseries.
#'
#' @export

extract_RAM <- function(id, ts_type, drop_na=TRUE){
  
  ts <- timeseries_values_views %>%
    dplyr::filter(stockid %in% id) %>%
    dplyr::mutate(scen = paste(ts_type, stockid, sep="_")) %>%
    dplyr::select(scen, year, tidyselect::all_of(ts_type)) %>%
    dplyr::relocate(scen)
  
  if (drop_na){
    ts <- ts %>% na.omit()
  }
  
  return(ts)
  
}





traits_fun <- function(traj_SProd){

  # Add names to match databases ----
  name_table <- traj_SProd %>%
    dplyr::select(stockid, scientificname) %>%
    dplyr::mutate(

      # Names for FishBase population growth traits (tm, Lm):
      fishbase_name = scientificname,
      fishbase_name =
        plyr::revalue(fishbase_name,
                      c("Theragra chalcogramma" = "Gadus chalcogrammus",
                        "Zearaja chilensis" = "Dipturus chilensis",
                        "Tetrapturus albidus" = "Kajikia albida",
                        "Sardinops melanostictus" = "Sardinops sagax",
                        "Raja rhina" = "Beringraja rhina",
                        "Neoplatycephalus richardsoni" = "Platycephalus richardsoni",
                        "Limanda ferruginea" = "Myzopsetta ferruginea",
                        "Etrumeus teres" = "Etrumeus sadina",
                        "Epinephelus niveatus" = "Hyporthodus niveatus",
                        "Epinephelus flavolimbatus" = "Hyporthodus flavolimbatus",
                        "Dentex tumifrons" = "Evynnis tumifrons",
                        "Chrysophrys auratus" = "Pagrus auratus",
                        "Bathyraja parmifera" = "Arctoraja parmifera"
                      )),

      # Names for FishLife population growth traits (tm, Lm):
      fishlife_name = scientificname,
      fishlife_name =
        plyr::revalue(fishlife_name,
                      c(
                        "Ammodytes spp"= "Ammodytes",
                        "Merluccius spp" = "Merluccius",
                        "Sebastes spp" = "Sebastes",
                        "Tetrapturus albidus" = "Kajikia albida",
                        "Sardinops melanostictus" = "Sardinops sagax",
                        "Neoplatycephalus richardsoni" = "Platycephalus richardsoni",
                        "Etrumeus teres" = "Etrumeus sadina",
                        "Epinephelus niveatus" = "Hyporthodus niveatus",
                        "Epinephelus flavolimbatus" = "Hyporthodus flavolimbatus",
                        "Chrysophrys auratus" = "Pagrus auratus",
                        "Bathyraja parmifera" = "Bathyraja",
                        "Centroberyx gerrardi" = "Centroberyx",
                        "Clupea pallasii" = "Clupea pallasii pallasii",
                        "Hexagrammos decagrammus" = "Hexagrammos",
                        "Merluccius gayi" = "Merluccius gayi gayi",
                        "Platycephalus conatus" = "Platycephalus",
                        "Scomber colias" = "Scomber",
                        "Sebastes auriculatus" = "Sebastes",
                        "Sebastes aurora" = "Sebastes",
                        "Sebastes carnatus" = "Sebastes",
                        "Sebastes chlorostictus" = "Sebastes",
                        "Sebastes diploproa" = "Sebastes",
                        "Sebastes entomelas" = "Sebastes",
                        "Sebastes nebulosus" = "Sebastes",
                        "Sebastolobus altivelis" = "Sebastolobus",
                        "Strangomera bentincki" = "Clupea bentincki")))

  traj_SProd <- traj_SProd %>%
    dplyr::left_join(name_table, by=c("stockid", "scientificname"))


  ## FishBase ----
  # Maximum weight, growth rate, lifespan...
  LH_grw <- rfishbase::popgrowth(
    traj_SProd %>% dplyr::pull(fishbase_name)) %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(mean_Loo = mean(Loo, na.rm = TRUE),
                     mean_K = mean(K, na.rm = TRUE),
                     mean_Winfinity = mean(Winfinity, na.rm = TRUE),
                     mean_tmax = mean(tmax, na.rm = TRUE),
                     mean_tm = mean(tm, na.rm = TRUE),
                     mean_M = mean(M, na.rm = TRUE),
                     mean_Lm = mean(Lm, na.rm = TRUE))

  # Trophic level  ----
  LH_troph <- rfishbase::ecology(traj_SProd %>%
                                   dplyr::pull(fishbase_name)) %>%
    dplyr::select(Species, FoodTroph) %>%
    # https://github.com/ropensci/rfishbase/issues/199
    # "FoodTroph gives a MonteCarlo estimate of trophic level based on known food items.
    # DietTroph uses the mean or median of trophic levels derived from actual diet composition studies.
    # While in theory troph from diet should be more reliable,
    # many diet studies are incomplete or biased and I often find FoodTroph more reasonable." Rainer Froese
    dplyr::group_by(Species) %>%
    dplyr::slice(1) # make sure to keep only one row by species

  # Type of habitat (demersal, pelagic, reef)  ----
  LH_hab <- rfishbase::fb_tbl("species") %>%
    dplyr::mutate(Species = paste(Genus, Species)) %>%
    dplyr::filter(Species %in% (traj_SProd %>% dplyr::pull(fishbase_name))) %>%
    dplyr::select(Species, DemersPelag) %>%
    dplyr::bind_rows(
      data.frame(Species=c("Ammodytes spp", "Merluccius spp", "Sebastes spp"),
                 DemersPelag = c("demersal", "bathydemersal", "bathypelagic"))) %>%
    dplyr::mutate(DemersPelag = dplyr::case_when(
      DemersPelag %in% c("bathydemersal", "benthopelagic", "demersal", "reef-associated") ~ "demersal",
      DemersPelag %in% c("pelagic-neritic", "pelagic-oceanic", "bathypelagic") ~ "pelagic")
      # DemersPelag %in% c("bathydemersal", "bathypelagic", "demersal", "reef-associated") ~ "demersal",
      # DemersPelag %in% c("pelagic-neritic", "pelagic-oceanic", "benthopelagic") ~ "pelagic")
      # https://fishbase.se/manual/English/FishBaseThe_Species_Table.htm
    )

  # Habitat RAMLDB  ----
  hab_ram <- traj_SProd %>%
    dplyr::left_join(bioparams %>%
                       dplyr::filter(bioid=="Habitat-Habitat") %>%
                       dplyr::rename(habitat_raw=biovalue) %>%
                       dplyr::select(stockid, habitat_raw),
                     by="stockid") %>%
    dplyr::select(stockid, scientificname, habitat_raw) %>%
    dplyr::mutate(habitat_raw = stringr::str_to_lower(habitat_raw),
                  habitat = dplyr::case_when(
                    grepl("pelagic|pekagic|pleagic|diadromous", habitat_raw) ~ "pelagic",
                    grepl("demersal", habitat_raw) ~ "demersal",
                    is.na(habitat_raw) ~ NA
                  )) %>%
    dplyr::mutate(
      # Names for FishBase:
      scientificname =
        plyr::revalue(scientificname,
                      c(
                        "Theragra chalcogramma" = "Gadus chalcogrammus",
                        "Zearaja chilensis" = "Dipturus chilensis",
                        "Tetrapturus albidus" = "Kajikia albida",
                        "Sardinops melanostictus" = "Sardinops sagax",
                        "Raja rhina" = "Beringraja rhina",
                        "Neoplatycephalus richardsoni" = "Platycephalus richardsoni",
                        "Limanda ferruginea" = "Myzopsetta ferruginea",
                        "Etrumeus teres" = "Etrumeus sadina",
                        "Epinephelus niveatus" = "Hyporthodus niveatus",
                        "Epinephelus flavolimbatus" = "Hyporthodus flavolimbatus",
                        "Dentex tumifrons" = "Evynnis tumifrons",
                        "Chrysophrys auratus" = "Pagrus auratus",
                        "Bathyraja parmifera" = "Arctoraja parmifera"
                      ), warn_missing=FALSE)) %>%
    dplyr::group_by(scientificname, habitat) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::group_by(scientificname) %>%
    dplyr::mutate(dup = n()) %>%
    dplyr::left_join(LH_hab, by=c("scientificname"="Species")) %>%
    tidyr::drop_na(habitat) %>%
    dplyr::mutate(no_match = ifelse(habitat != DemersPelag, "mismatch", NA))


  LH <-
    traj_SProd %>%
    dplyr::select(stockid, fishbase_name, classname, ordername, family) %>%
    dplyr::group_by(fishbase_name) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-stockid) %>%
    dplyr::rename(Species=fishbase_name) %>%
    dplyr::left_join(LH_grw, by="Species") %>%
    dplyr::left_join(LH_troph, by="Species") %>%
    dplyr::left_join(LH_hab, by="Species") %>%
    dplyr::select(Species, classname, ordername, family, FoodTroph, DemersPelag)

  ## FishLife  ----
  traits_fishlife <- FishLife::FishBase$beta_gv %>%
    tibble::as_tibble(rownames="taxa") %>%
    dplyr::mutate(taxa_type = dplyr::case_when(
      stringr::str_count(taxa, "_")==4 ~"species",
      stringr::str_count(taxa, "_")==3 ~"genus",
      stringr::str_count(taxa, "_")==2 ~"family",
      stringr::str_count(taxa, "_")==1 ~"order",
      stringr::str_count(taxa, "_")==0 ~"class")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Species = dplyr::case_when(
      taxa_type=="species" ~paste(strsplit(taxa, "_")[[1]][4:5], collapse=" "),
      taxa_type=="genus" ~strsplit(taxa, "_")[[1]][4],
      taxa_type=="family" ~strsplit(taxa, "_")[[1]][3],
      taxa_type=="order" ~strsplit(taxa, "_")[[1]][2],
      taxa_type=="class" ~strsplit(taxa, "_")[[1]][1]))

  fishlife_traits <- traj_SProd %>%
    dplyr::select(stockid, fishlife_name, phylum, classname, ordername, family) %>%
    dplyr::group_by(fishlife_name) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-stockid) %>%
    dplyr::rename(Species=fishlife_name) %>%
    dplyr::mutate(Species = sub(" spp", "", Species)) %>%
    dplyr::left_join(traits_fishlife %>%
                       dplyr::select(-taxa), by="Species")

  LH_fishlife_complete <-
    traj_SProd %>%
    dplyr::select(stockid, scientificname, fishbase_name, fishlife_name) %>%
    dplyr::group_by(scientificname) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-stockid) %>%
    dplyr::rename(Species=scientificname) %>%
    dplyr::left_join(fishlife_traits %>%
                       dplyr::select(-c(phylum, classname, ordername, family)),
                     by=c("fishlife_name"="Species")) %>%
    dplyr::left_join(LH, by=c("fishbase_name"="Species"))


  ## Fishing-related traits  ----
  fish_Umsy <-
    extract_RAM(traj_SProd %>% dplyr::pull(stockid),
                ts_type="UdivUmsypref", drop_na=TRUE) %>%
    dplyr::mutate(stockid = sub("UdivUmsypref_","",scen)) %>%
    dplyr::left_join(
      traj_SProd %>%
        dplyr::select(stockid, scientificname, class, trend, loc_brk_chg) %>%
        dplyr::mutate(loc_brk_chg = ifelse(class == "abrupt", loc_brk_chg, NA)) %>%
        dplyr::select(stockid, loc_brk_chg),
      by="stockid") %>%
    dplyr::group_by(stockid) %>%
    dplyr::mutate(
      max_U = max(UdivUmsypref),
      mean_U = mean(UdivUmsypref),
      U_change =
        summary(lm(UdivUmsypref~year))$coefficients["year","Estimate"],
      sd_U = sd(UdivUmsypref),
      detrsd_U = sd(pracma::detrend(UdivUmsypref)),
      len_U = length(UdivUmsypref),
      D_U = (1/(len_U-1))*sum(abs(log((dplyr::lead(UdivUmsypref)+0.01*mean_U)/
                                        (UdivUmsypref+0.01*mean_U))), na.rm=TRUE)
    ) %>%
    dplyr::filter(is.na(loc_brk_chg) | year<=loc_brk_chg) %>%
    dplyr::mutate(
      max_U_prshf = max(UdivUmsypref),
      mean_U_prshf = mean(UdivUmsypref),
      U_change_prshf =
        summary(lm(UdivUmsypref~year))$coefficients["year","Estimate"],
      sd_U_prshf = sd(UdivUmsypref),
      detrsd_U_prshf = sd(pracma::detrend(UdivUmsypref)),
      len_U_prshf = length(UdivUmsypref),
      D_U_prshf = (1/(len_U-1))*sum(abs(log((dplyr::lead(UdivUmsypref)+0.01*mean_U)/
                                              (UdivUmsypref+0.01*mean_U))), na.rm=TRUE)
    ) %>%
    dplyr::slice(1) %>%
    tidyr::drop_na(mean_U) %>%
    dplyr::select(stockid,
                  max_U, mean_U, U_change,
                  sd_U, detrsd_U, D_U, len_U,
                  max_U_prshf, mean_U_prshf, U_change_prshf,
                  sd_U_prshf, detrsd_U_prshf, D_U_prshf, len_U_prshf
    )
  # There are two stocks ("COD2J3KL" and "RKCRABBB") for which the shift occurred
  # before values of UdivUmsypref were available.
  
  var_ER = "ERbest" # previously "ERbest2"
  fish_ER <- traj_SProd %>%
    dplyr::pull(stockid) %>%
    extract_RAM(ts_type=var_ER, drop_na=TRUE) %>%
    dplyr::mutate(stockid = sub(paste0(var_ER, "_"),"",scen)) %>%
    dplyr::left_join(
      traj_SProd %>%
        dplyr::select(stockid, scientificname, class, trend, loc_brk_chg) %>%
        dplyr::mutate(loc_brk_chg = ifelse(class == "abrupt", loc_brk_chg, NA)) %>%
        dplyr::select(stockid, loc_brk_chg),
      by="stockid") %>%
    dplyr::group_by(stockid) %>%
    dplyr::mutate(mean_ER = mean(!!sym(var_ER)),
                  len_ER = length(!!sym(var_ER))) %>% 
    dplyr::filter(is.na(loc_brk_chg) | year<=loc_brk_chg) %>%
    dplyr::filter(year>=first | year<=last) %>%
    dplyr::mutate(mean_ER_prshf = mean(!!sym(var_ER)),
                  ER_change_prshf =
                    summary(lm(!!sym(var_ER)~year))$coefficients["year","Estimate"],
                  len_ER_prshf = length(!!sym(var_ER))) %>%
    dplyr::slice(1) %>%
    tidyr::drop_na(mean_ER_prshf) %>%
    dplyr::select(stockid,
                  mean_ER, mean_ER_prshf, ER_change_prshf, len_ER, len_ER_prshf)

  # Minimum & Average B/Bmsy  ----
  fish_Bmsy <-
    extract_RAM(traj_SProd %>% dplyr::pull(stockid),
                ts_type="BdivBmsypref", drop_na=TRUE) %>%
    dplyr::mutate(stockid = sub("BdivBmsypref_","",scen)) %>%
    dplyr::left_join(
      traj_SProd %>%
        dplyr::select(stockid, scientificname, class, trend, loc_brk_chg) %>%
        dplyr::mutate(loc_brk_chg = ifelse(class == "abrupt", loc_brk_chg, NA)) %>%
        dplyr::select(stockid, loc_brk_chg),
      by="stockid") %>%
    dplyr::group_by(stockid) %>%
    dplyr::mutate(
      min_B = min(BdivBmsypref),
      mean_B = mean(BdivBmsypref),
      sd_B = sd(BdivBmsypref),
      detrsd_B = sd(pracma::detrend(BdivBmsypref)),
      len_B = length(BdivBmsypref),
      D_B = (1/(len_B-1))*sum(abs(log((dplyr::lead(BdivBmsypref)+0.01*mean_B)/
                                        (BdivBmsypref+0.01*mean_B))), na.rm=TRUE)
    ) %>%
    dplyr::filter(is.na(loc_brk_chg) | year<=loc_brk_chg) %>%
    dplyr::mutate(
      min_B_prshf = min(BdivBmsypref),
      mean_B_prshf = mean(BdivBmsypref),
      sd_B_prshf = sd(BdivBmsypref),
      detrsd_B_prshf = sd(pracma::detrend(BdivBmsypref)),
      len_B_prshf = length(BdivBmsypref),
      D_B_prshf = (1/(len_B-1))*sum(log((dplyr::lead(BdivBmsypref)+0.01*mean_B)/
                                          (BdivBmsypref+0.01*mean_B)), na.rm=TRUE)
    ) %>%
    dplyr::slice(1) %>%
    tidyr::drop_na(mean_B) %>%
    dplyr::select(stockid,
                  min_B, mean_B,
                  sd_B, detrsd_B, D_B, len_B,
                  min_B_prshf, mean_B_prshf,
                  sd_B_prshf, detrsd_B_prshf, D_B_prshf, len_B_prshf
    )

  # Total exploitation time  ----
  fish_expl_time <- timeseries %>%
    dplyr::group_by(stockid) %>%
    tidyr::drop_na(tsvalue) %>%
    dplyr::mutate(first_year_avail = min(tsyear)) %>%
    dplyr::filter(tsyear==first_year_avail) %>%
    dplyr::slice(1) %>%
    dplyr::select(stockid, first_year_avail) %>%
    dplyr::filter(stockid %in% (traj_SProd %>%
                                  dplyr::pull(stockid))) %>%
    dplyr::left_join(
      traj_SProd %>%
        dplyr::select(stockid, scientificname, class, trend, first, last, loc_brk_chg) %>%
        dplyr::mutate(loc_brk_chg = ifelse(class == "abrupt", loc_brk_chg, NA)) %>%
        dplyr::select(stockid, first, last, loc_brk_chg),
      by="stockid") %>%
    dplyr::mutate(total_time = last-first_year_avail+1,
                  time_prshf = ifelse(!is.na(loc_brk_chg),
                                      loc_brk_chg-first_year_avail+1,
                                      total_time))

  # Absolute catch (mean or cumulative)  ----
  fish_catch <- extract_RAM(traj_SProd %>% dplyr::pull(stockid),
                            ts_type="TCbest", drop_na=TRUE) %>%
    dplyr::mutate(stockid = sub("TCbest_","",scen)) %>%
    dplyr::left_join(
      traj_SProd %>%
        dplyr::select(stockid, scientificname, class, trend, loc_brk_chg) %>%
        dplyr::mutate(loc_brk_chg = ifelse(class == "abrupt", loc_brk_chg, NA)) %>%
        dplyr::select(stockid, loc_brk_chg),
      by="stockid") %>%
    dplyr::group_by(stockid) %>%
    dplyr::mutate(
      sum_C = sum(TCbest, na.rm=TRUE),
      mean_C = mean(TCbest)
    ) %>%
    dplyr::filter(is.na(loc_brk_chg) | year<=loc_brk_chg) %>%
    dplyr::mutate(
      sum_C_prshf = sum(TCbest, na.rm=TRUE),
      mean_C_prshf = mean(TCbest)
    ) %>%
    dplyr::slice(1) %>%
    tidyr::drop_na(mean_C) %>%
    dplyr::select(stockid,
                  sum_C, sum_C_prshf,
                  mean_C, mean_C_prshf)

  # Management approach
  fish_param <- traj_SProd %>%
    dplyr::select(stockid, scientificname) %>%
    dplyr::left_join(fish_expl_time, by="stockid") %>%
    dplyr::left_join(fish_Bmsy, by="stockid") %>%
    dplyr::left_join(fish_Umsy, by="stockid") %>%
    dplyr::left_join(fish_ER, by="stockid") %>%
    dplyr::left_join(fish_catch, by="stockid")


  # Environmental data  ----
  SST_had <-
    readr::read_csv(paste0("data/SST_share_Mathieu/Annual_SST_code/Output/",
                           "1e_yearly_had_sst_polMP.csv")) %>%
    dplyr::left_join(assessment %>%
                       dplyr::select(assessid, stockid), by="assessid") %>%
    dplyr::filter(stockid %in% (traj_SProd %>% dplyr::pull(stockid)))


  # Trim SST timeseries to match period available for productivity
  SST_had_SProd <- SST_had %>%
    dplyr::left_join(traj_SProd %>%
                       dplyr::select(stockid, first, last, loc_brk_chg, class), by="stockid") %>%
    dplyr::group_by(stockid) %>%
    dplyr::mutate(loc_brk_chg = ifelse(class=="abrupt", loc_brk_chg, NA)) %>%
    dplyr::filter(is.na(loc_brk_chg) | year<=loc_brk_chg) %>% # to match preshift period
    dplyr::filter(year>=first & year<=last) # to match period available

  sst_stock_list <- SST_had_SProd %>%
    tidyr::drop_na(sst_c) %>%
    dplyr::distinct(stockid) %>%
    dplyr::pull(stockid) %>%
    sort()

  SST_had_SProd_df <- SST_had_SProd %>%
    tidyr::drop_na(sst_c) %>%
    dplyr::mutate(sst_avg = mean(sst_c),
                  sst_cv = sd(sst_c)/mean(sst_c),
                  sst_sd = sd(sst_c),
                  sst_detrsd = sd(pracma::detrend(sst_c)),
                  sst_len = length(sst_c),
                  # sst_D = (1/(length(sst_c)-1))*sum(abs(log((dplyr::lead(sst_c)+0.01*sst_avg)/
                  #                                             (sst_c+0.01*sst_avg))), na.rm=TRUE),
                  sst_change =
                    summary(lm(sst_c~year))$coefficients["year","Estimate"]) %>%
    dplyr::slice(1) %>%
    dplyr::select(stockid, sst_avg, sst_change
                  # , sst_cv, sst_sd, sst_detrsd, sst_D
    )

  ## Polygons ----
  polygons <- traj_SProd %>%
    dplyr::left_join(
      readr::read_csv(
        paste0("data/_spatial/ramldb_boundaries/",
               "ramldb_v3.8_stock_boundary_table_v2_formatted.csv")) %>%
        dplyr::mutate(batch="assess1") %>%
        dplyr::bind_rows(
          readr::read_csv(
            paste0("data/_spatial/ramldb_boundaries/",
                   "ramldb_stock_boundary_formatted_bis.csv")) %>%
            dplyr::mutate(batch="assess2")) %>%
        dplyr::rename(assessid_polygon_avail = assessid) %>%
        dplyr::group_by(stockid) %>%
        dplyr::mutate(n=n()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n==1 | batch=="assess2") %>%
        dplyr::select(stockid, assessid_polygon_avail, batch),
      by="stockid")

  stocks_available_pol <- polygons %>%
    tidyr::drop_na(assessid_polygon_avail) %>%
    dplyr::pull(stockid)


  centroids_available <- lapply(stocks_available_pol, function(x){

    batch <- polygons %>%
      dplyr::filter(stockid==x) %>%
      dplyr::pull(batch)

    if(batch=="assess1"){
      path <- "shapes"
    } else {
      path <- "add_shapes"
    }

    sf::read_sf(paste0("data/_spatial/ramldb_boundaries/",path,"/",
                       polygons$assessid_polygon_avail[polygons$stockid==x],
                       ".shp")) %>%
      # transform to a projection with projected coordinates (World Mercator):
      sf::st_transform(3395) %>%
      sf::st_centroid() %>%
      # transform back to WGS 84 (with geographic coordinates):
      sf::st_transform(4326) %>%
      sf::st_coordinates() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(stockid=x) %>%
      suppressWarnings()

  }) %>%
    do.call(what=dplyr::bind_rows)

  polygons <- polygons %>%
    dplyr::left_join(centroids_available, by="stockid")

  # Merge all traits ----
  traits <- LH_fishlife_complete %>%
    dplyr::select(-c(taxa_type, fishbase_name, fishlife_name)) %>%
    dplyr::left_join(polygons %>%
                       dplyr::select(scientificname, stockid, X, Y) %>%
                       dplyr::rename(Species=scientificname), by="Species") %>%
    dplyr::left_join(fish_param %>%
                       dplyr::select(stockid, mean_B_prshf, mean_U_prshf, U_change_prshf,
                                     mean_ER_prshf, ER_change_prshf, sum_C_prshf),
                     by="stockid") %>%
    dplyr::left_join(SST_had_SProd_df %>%
                       dplyr::select(stockid, sst_avg, sst_change
                                     # , sst_sd, sst_D
                       ), by="stockid") %>%
    dplyr::relocate(stockid)

  return(traits)
}
