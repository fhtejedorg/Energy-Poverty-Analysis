# ---- title: 01_Spatial_DataProcessing.R ----
# Purpose: Consolidate geo dataframe for further analysis, plus exporting all the  districts without filtering  
# Inputs: def_wijken_23.csv, def_wijken_23.shp
# Outputs: g_def_wijken_23.Rdata, g_def_merged_all_wijken_23.gpkg
# Author: Fabio Tejedor 
# Date: 2025-01-01
# Usage: R/03_Spatial_DataProcessing.R

rm(list = ls())
gc()
setwd("C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/02_Paper_energy_poverty/Energy poverty Analysis/data/wijken/00_intermediate_data")
# ==== Libraries ================
library(sf)        
library(dplyr)     
library(ggplot2)   
library(gstat)     
library(spdep)     
library(viridis)   
library(scales) 
library(gpkg)


# ==== Globals ================
outpath_figure <- "../../../output/R/econometric/final_results"

# ==== Defining units of interpretation ================
per_n_units <- 10000
per_n_years <- 10

# ==== Preparing data ================
source("../../../R/national/01_data_preparation.R")

# === To inspect gpkg ================
# g_list_GDB <- geopackage("data/wijken/00_intermediate_data/cbsgebiedsindelingen2016-2025/cbsgebiedsindelingen2023.gpkg", connect = TRUE)
# gpkg_list_tables(g_list_GDB)
# gpkg_table(g_list_GDB, "gpkg_contents")
# gpkg_table(g_list_GDB, "provincie_gegeneraliseerd")

# ==== Filtering data with units of final analysis ================
g_def_wijken_23$p_LIHC <- g_def_wijken_23$LIHE

g_def_wijken_23 <- g_def_wijken_23 %>% 
  filter(AANT_INW_23 > 0 &
           (p_rental_home > 0) & 
           !is.na(p_LIHC) & WOZ >0 & !is.na(p_low_house_energy_eff)#& 
         )

g_def_wijken_23$urbanity_degree <- relevel(g_def_wijken_23$urbanity_degree, ref = "semi_urban")
g_def_wijken_23$STED <- as.factor(as.character(g_def_wijken_23$STED))
g_def_wijken_23$STED <- relevel(g_def_wijken_23$STED, ref = "5")
g_def_wijken_23$wijk_predominant_typology <- factor(g_def_wijken_23$wijk_predominant_typology)
# Renaming LIHC with LIHC, official name 

# ==== Creating new columns ================
g_def_wijken_23[, "ratio_rental_owner"] <- g_def_wijken_23$p_rental_home/g_def_wijken_23$p_owner_ocupied_home
g_def_wijken_23[, "ratio_housingCorp_owner"] <- g_def_wijken_23$p_rental_corporations_home/g_def_wijken_23$p_owner_ocupied_home
g_def_wijken_23[, "p_gas_consum_rental"] <- with(g_def_wijken_23, t_gas_consum_rental/(t_gas_consum_rental + t_gas_consum_owner) * 100)
g_def_wijken_23[, "p_gas_consum_rental"] <- ifelse(is.nan(g_def_wijken_23$p_gas_consum_rental), 0, g_def_wijken_23$p_gas_consum_rental)

g_def_wijken_23[, "p_elec_consum_rental"] <- with(g_def_wijken_23, t_elec_consum_rental/(t_elec_consum_rental + t_elec_consum_owner)* 100)
g_def_wijken_23[, "p_elec_consum_rental"] <- ifelse(is.nan(g_def_wijken_23$p_elec_consum_rental), 0, g_def_wijken_23$p_elec_consum_rental)

g_def_wijken_23[, "t_h_type_apartment"] <- with(g_def_wijken_23, p_h_type_apartment * total_residential_units / per_n_units)
g_def_wijken_23[, "t_h_type_corner_house"] <- with(g_def_wijken_23, p_h_type_corner_house * total_residential_units / per_n_units)
g_def_wijken_23[, "t_h_type_detached"] <- with(g_def_wijken_23, p_h_type_detached * total_residential_units / per_n_units)
g_def_wijken_23[, "t_h_type_semi_detached"] <- with(g_def_wijken_23, p_h_type_semi_detached * total_residential_units / per_n_units)
g_def_wijken_23[, "t_h_type_terraced_house_semi_detached"] <- with(g_def_wijken_23, p_h_type_terraced_house_semi_detached * total_residential_units / per_n_units)
g_def_wijken_23[, "t_h_type_terraced_semi_detached_corner"] <- with(g_def_wijken_23, t_h_type_corner_house + t_h_type_terraced_house_semi_detached / per_n_units)
g_def_wijken_23[, "p_homes_without_solar_panels"] <- with(g_def_wijken_23, 100 - p_homes_with_solar_panels)
g_def_wijken_23[, "building_median_ageX10Y"] <- with(g_def_wijken_23, building_median_age / per_n_years)
g_def_wijken_23[, "p_h_year_cons_Before.1947"] <- with(g_def_wijken_23, p_h_year_cons_Before.1924 + p_h_year_cons_1925.1947)
g_def_wijken_23[, "p_h_year_cons_1948.1981"] <- with(g_def_wijken_23, p_h_year_cons_1948.1963 +  p_h_year_cons_1964.1981)
g_def_wijken_23[, "p_h_year_cons_After.2002"] <- with(g_def_wijken_23, p_h_year_cons_2003.2015 + p_h_year_cons_After.2015)
g_def_wijken_23[, "p_elec_heat"] <- with(g_def_wijken_23, 
                                         p_elec_heat_high_gas_consum + 
                                           p_elec_heat_low_gas_consum + 
                                           p_elec_heat_no_gas_consum
                                           )
g_def_wijken_23[, "ratio_ethnicity_NL_Outside"] <- with(g_def_wijken_23, 
                                                        p_pop_Origin_Outside_Europe_Born_Outside_NL/p_pop_Origin_NL_Born_NL)


# ==== impute missing by GM median (selected vars)  ================
main_var_imputation <- c(
  ################ Socio-demographic vulnerable #######################
  "p_fragile_health_65", 
  "p_pension_coverage_rate", 
  "p_unemployment_rate", 
  "p_precarious_part_time", 
  "p_disability", 
  # "p_without_young_children",
  "p_single_person_household",
  "p_with_young_children",
  
  ################ Education ##############################################
  #"p_education_High",  # Reference
  "p_education_Secondary",
  "p_education_Low",
  
  ################ Housing rental type ######################################
  #"p_owner_occupied_home", # Reference Unknown are very few (assumed here a negligible effect) 
  "p_rental_corporations_home",
  "p_rental_private_landlord_home",
  
  ################ Energy consumption ######################################
  "p_gas_consum_rental",
  "p_elec_consum_rental",
  
  ################ Heating system ######################################
  "p_individual_CV",
  "p_elec_heat",
  
  ################ Energy independence ######################################
  "p_homes_without_solar_panels", 
  
  ################ Built-infrastructure ######################################
  "p_green", 
  "p_gray", 
  
  ################ Temperature ######################################
  "CDD", 
  "HDD", 
  
  ################ Housing typology ######################################
  #"p_h_type_apartment", # Reference 
  "p_h_type_corner_house", 
  "p_h_type_detached", 
  "p_h_type_semi_detached", 
  "p_h_type_terraced_house_semi_detached", 
  
  ################ Construction year ######################################
  #"p_h_year_cons_Before.1947", # Reference
  "p_h_year_cons_1948.1981",
  "p_h_year_cons_1982.2002",
  "p_h_year_cons_After.2002", 
  
  ################ Energetic efficiency ######################################
  # p_high_house_energy_eff # reference 
  # p_very_high_house_energy_eff # reference 
  "p_low_house_energy_eff", 
  "p_medium_house_energy_eff", 
  
  ################ Property value ############################################
  "WOZ",
  
  ################ Ethnicity and language barriers ##########################
  # "p_pop_Origin_NL_Born_NL",
  # "p_pop_Origin_Europe_Born_NL",
  "p_pop_Origin_Outside_Europe_Born_NL",
  "p_pop_Origin_Europe_Born_Outside_NL",
  "p_pop_Origin_Outside_Europe_Born_Outside_NL",
  
  ################ Urbanity degree ##########################
  # "urbanity_degree",
  
  ############### Other relevant ###########################
  "building_median_age",
  "p_without_young_children",
  "ratio_rental_owner",
  "ratio_housingCorp_owner",
  "t_av_household_size",
  "p_households_under_around_social_minimum",
  "building_median_age"
)

for(var in main_var_imputation){
  temp_df <- data.frame(g_def_wijken_23[, c("GM_CODE_23_x", var)])
  target_var <- temp_df[, var]
  temp_df$geometry <- NULL
  temp_df <- temp_df %>% 
    group_by(GM_CODE_23_x) %>% 
    summarise(aver_GM = mean(.data[[var]], na.rm = T),
              median_GM = median(.data[[var]], na.rm = T))
  g_def_wijken_23 <- merge(g_def_wijken_23, 
                           temp_df,  by = "GM_CODE_23_x", all.x = TRUE)
  cat(var, ": ", sum(is.na(target_var))/length(target_var), "\n")
  g_def_wijken_23[, var] <- ifelse(is.na(target_var), 
                                   g_def_wijken_23$median_GM, 
                                   target_var)
  g_def_wijken_23$median_GM <- NULL
  g_def_wijken_23$aver_GM <- NULL
}


# ==== CRS info ===============================================================
crs_info <- st_crs(g_def_wijken_23)

# ==== Calculating centroids per each district ========================================
g_def_wijken_23$centroids <- st_point_on_surface(g_def_wijken_23$geometry)
g_def_wijken_23$id <- 1:nrow(g_def_wijken_23)

# ==== Generating/exporting geo dataframes for visualisations and descriptives ========================================
save(g_def_wijken_23, file = file.path("../../../output/R/data/03_Spatial Data Processing", "g_def_wijken_23.Rdata"))
g_wijken_all <- st_read("./data/wijken/00_intermediate_data/def_wijken_23.shp")
g_wijken_all <- g_wijken_all %>% dplyr::select(WK_CODE_23)
g_def_wijken_23_NoGeom <- st_drop_geometry(g_def_wijken_23)
g_def_wijken_23_NoGeom$centroids <- NULL
g_wijken_all <- merge(g_wijken_all, g_def_wijken_23_NoGeom, by = "WK_CODE_23", all.x = T)
st_write(g_wijken_all, "./output/R/data/03_Spatial Data Processing/g_def_merged_all_wijken_23.gpkg", layer = "g_def_merged_all_wijken_23", delete_layer = TRUE)



