# ---- title: 06_Spatial_Econometric_Analysis.R ----
# Purpose: Spatial Econometric Analysis: Model exploration and inference 
# Inputs: def_wijken_23.shp COROP:Gebieden_in_Nederland_2023_85385NED.csv
# Outputs: Analysis
# Author: Fabio Tejedor 
# Date: 2025-01-01
# Usage: 06_Spatial_Econometric_Analysis.R

rm(list = ls())
gc()
setwd("C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/02_Paper_energy_poverty/Energy poverty Analysis")
library(tidyverse)
library(lmtest)
library(sandwich)
library(dplyr)
library(ggforce)
library(car)
library(MASS)
library(sf)
library(spdep)
library(spatialreg)
library(RColorBrewer)
library(ggplot2)
library(tmap)
library(tmaptools)
library(lmtest)
library(splm)
library(data.table)
library(sphet)
library(spgwr)
library(ade4)
library(rgeoda)
library(stargazer)
library(texreg)
### Additional packages 
library(gstat)
library(ggiraph)
library(spatialreg)
library(SDPDmod)
library(ggpubr)
library(spmoran)
library(kableExtra)
library(purrr)
library(tibble)
library(kableExtra)
source("R/national/utils.R")
# ==== Globals ================

outpath_figure <- "output/R/econometric/final_results/Spatial econometric/"
input_data <- "output/R/data/03_Spatial Data Processing/"

# ==== Loading data ================
# Consolidated data from step 3: Spatial Data Processing

load(file = file.path("output/R/data/03_Spatial Data Processing/", "g_def_wijken_23.Rdata"))

# Loading COROP
dat_COROP <- read.delim("data/wijken/Gebieden_in_Nederland_2023_85385NED.csv", 
                        sep = ";", header = T)
dat_COROP$ID <- NULL
dat_COROP[,] <- lapply(dat_COROP, trimws)
dat_COROP <- dat_COROP %>% 
  rename(
    GM_CODE = RegioS,
    GM_NAAM = Naam_2,
    COROP_CODE = Code_10,
    COROP_NAME = Naam_11,
    REGIO_CODE = Code_28,
    REGIO_NAAM = Naam_29
  ) %>% dplyr:::select(GM_CODE, GM_NAAM, COROP_CODE, COROP_NAME, REGIO_CODE, REGIO_NAAM)


# ==== Merging  data from regions ===============================================
g_def_wijken_23 <- merge(g_def_wijken_23, dat_COROP, by.x = "GM_CODE_23_x", by.y = "GM_CODE", all.x = T)

# variable list with reference categories 
cols_model_nal_01 <- c(
  # "p_LIHC", 
  #####################################################################
  ################ Block 1: Social and economic context  ##############
  #####################################################################
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
  
  ################ Ethnicity and language barriers ##########################
  # "p_pop_Origin_NL_Born_NL",
  # "p_pop_Origin_Europe_Born_NL",
  "p_pop_Origin_Outside_Europe_Born_NL",
  "p_pop_Origin_Europe_Born_Outside_NL",
  "p_pop_Origin_Outside_Europe_Born_Outside_NL",
  
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
  
  ################ Property value ############################################
  "WOZ",
  
  ################ Urbanity degree ##########################
  "urbanity_degree",
  
  #####################################################################
  ################ Block 2: Physical and Built environment ############
  #####################################################################
  
  ################ Housing typology ######################################
  # "p_h_type_apartment", # Reference 
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
  
  ################ Built-infrastructure ######################################
  "p_green", 
  "p_gray", 
  
  ################ Temperature ######################################
  "CDD", 
  "HDD"
)

cols_local_sel_nal_01 <- c(
  "p_unemployment_rate",
  "p_homes_without_solar_panels",
  # "p_h_type_apartment",
  "p_h_type_corner_house",
  "p_h_type_detached",
  "p_h_type_semi_detached",
  "p_h_type_terraced_house_semi_detached",
  "p_h_year_cons_1948.1981",
  "p_h_year_cons_1982.2002",
  "p_h_year_cons_After.2002",
  "WOZ"
)

cols_local_extended_sel_nal_01 <- c(
  cols_local_sel_nal_01, 
  "p_green", "p_gray", "CDD", "HDD", "urbanity_degree")

# ======================================================================
# ==== Section1: Model selection              
# ======================================================================
# Merging coordinates 
coords <- st_coordinates(g_def_wijken_23$centroids)
g_def_wijken_23 <- cbind(g_def_wijken_23, coords)

# ======================================================================
# ==== Case 1: W: QUEEN, Log transformation LIHC 
# ======================================================================
# Configuration W 
coords <- st_coordinates(g_def_wijken_23$centroids)
nb <- poly2nb(g_def_wijken_23, queen=TRUE, snap = 100) ### it works well in the analysis of contiguity 
lw_queen <- nb2listw(nb, style="W", zero.policy=TRUE) ### W: row type of standardization 

# Formula model 
var_target <- "log(p_LIHC)"
formula_block <- paste(var_target, " ~", paste(cols_model_nal_01, collapse = " + ", sep = " "))
formula_block <- formula(formula_block)
formula_local <- paste(" ~", paste(cols_local_sel_nal_01, collapse = " + ", sep = " "))
formula_local <- formula(formula_local)

# Testing Lagrange for error and autocorrelation parameter: Specific to General approaach 
mod_pool_OLS_nl_log <- lm(formula_block, data = g_def_wijken_23)
spdep::lm.RStests(mod_pool_OLS_nl_log, lw_queen, test=c("adjRSerr", "adjRSlag"))
spdep::lm.LMtests(mod_pool_OLS_nl_log, lw_queen, test= "all")

# Testing models: LAG, SEM, SDEM, SDM 
mod_SEM_Queen_local_nl_log <- errorsarlm(formula_block,data = g_def_wijken_23, listw = lw_queen, tol.solve=1.0e-30, 
                                         zero.policy=TRUE)
mod_LAG_Queen_local_nl_log <- lagsarlm(formula_block, data = g_def_wijken_23, listw = lw_queen, zero.policy=TRUE) 
mod_SLX_Queen_local_nl_log  <- spatialreg:::lmSLX(formula_block,data = g_def_wijken_23, lw_queen, zero.policy = TRUE, Durbin = formula_local)

mod_SDM_Queen_local_nl_log  <- spatialreg:::lagsarlm(formula_block,data = g_def_wijken_23, lw_queen, 
                                                     Durbin = formula_local , zero.policy = TRUE)
mod_SDEM_Queen_local_nl_log  <- spatialreg:::errorsarlm(formula_block, data = g_def_wijken_23, lw_queen, 
                                                        Durbin = formula_local , zero.policy = TRUE)

mod_SDEM_Queen_local_extended_nl_log  <- spatialreg:::errorsarlm(formula_block, data = g_def_wijken_23, lw_queen, 
                                                        Durbin = formula_local_extended , zero.policy = TRUE)

mod_SDEM_Queen_local_full_nl_log  <- spatialreg:::errorsarlm(formula_block, data = g_def_wijken_23, lw_queen, 
                                                                 Durbin = TRUE , zero.policy = TRUE)

# Testing deviations from normal distribution 
png(file.path(outpath_figure, "Robustness_check", "QQplots_SDEM_SDM.png"),
    res = 1200, width = 8, height = 6, units = "in")
par(mfrow = c(1, 2))
qqPlot(mod_SDM_Queen_local_nl_log$residuals)
qqPlot(mod_SDEM_Queen_local_nl_log$residuals)
dev.off()

# GEnerating sumaries for comparison 
W <- as(lw_queen, "CsparseMatrix")
set.seed(500L)
trMC <- trW(W, type="MC", p = 20) #default p = 16, 
impacts_SDM_Queen_local_nl_log <- spatialreg:::impacts(mod_SDM_Queen_local_nl_log, tr = trMC, R=200, Q=5)
summary_SDM_Queen_local_nl_log <- summary(impacts_SDM_Queen_local_nl_log, zstats=TRUE, short=TRUE)
summary_SDM_Queen_local_nl_log

mod_lists_Queen <- 
  list(
    "LAG_LOG"  = mod_LAG_Queen_local_nl_log,
    "SEM_LOG"  = mod_SEM_Queen_local_nl_log,
    "SLX_LOG"  = mod_SLX_Queen_local_nl_log,
    "SDM_LOG"  = mod_SDM_Queen_local_nl_log, 
    "SDEM_LOG" = mod_SDEM_Queen_local_nl_log
  )

sort(sapply(mod_lists_Queen, AIC))
sort(sapply(mod_lists_Queen, BIC))
sort(sapply(mod_lists_Queen, logLik))

# Likelihood testing beteen SEM and SDEM 
LR.Sarlm(mod_SEM_Queen_local_nl_log, mod_SDEM_Queen_local_nl_log)

# ======================================================================
# ==== Case 2: W: QUEEN, No-transformation LIHC 
# ======================================================================

# Formula model 
var_target <- "LIHE"
formula_blok_MI <- paste(var_target, " ~", paste(cols_model_nal_01, collapse = " + ", sep = " "))
formula_blok_MI <- formula(formula_blok_MI)
formula_local_MI <- paste(" ~", paste(cols_local_sel_nal_01, collapse = " + ", sep = " "))
formula_local_MI <- formula(formula_local_MI)

# Testing Lagrange for error and autocorrelation parameter: Specific to General approaach 
mod_pool_OLS_nl_MI <- lm(formula_blok_MI, data = g_def_wijken_23)
spdep::lm.RStests(mod_pool_OLS_nl_MI, lw_queen, test=c("adjRSerr", "adjRSlag"))

# Testing models: LAG, SEM, SDEM, SDM 
mod_SEM_Queen_local_nl_MI  <- errorsarlm(formula_blok_MI,data = g_def_wijken_23, listw = lw_queen, tol.solve=1.0e-30, zero.policy=TRUE)
mod_LAG_Queen_local_nl_MI <- lagsarlm(formula_blok_MI, data = g_def_wijken_23, listw = lw_queen, zero.policy=TRUE) 
mod_SLX_Queen_local_nl_MI  <- spatialreg:::lmSLX(formula_blok_MI,data = g_def_wijken_23, lw_queen, zero.policy = TRUE, Durbin = formula_local_MI)

mod_SDM_Queen_local_nl_MI  <- spatialreg:::lagsarlm(formula_blok_MI,data = g_def_wijken_23, lw_queen, 
                                                    Durbin = formula_local_MI , zero.policy = TRUE)
mod_SDEM_Queen_local_nl_MI  <- spatialreg:::errorsarlm(formula_blok_MI, data = g_def_wijken_23, lw_queen, 
                                                       Durbin = formula_local_MI , zero.policy = TRUE)

lrtest(mod_SDM_Queen_local_nl_MI, mod_SLX_Queen_local_nl_MI)
lrtest(mod_SDM_Queen_local_nl_MI, mod_SEM_Queen_local_nl_MI)

lrtest(mod_SDEM_Queen_local_nl_MI, mod_SLX_Queen_local_nl_MI)
lrtest(mod_SDEM_Queen_local_nl_MI, mod_SEM_Queen_local_nl_MI)


deviance(mod_SDM_Queen_local_nl_MI);deviance(mod_SDEM_Queen_local_nl_MI); 
deviance(mod_SLX_Queen_local_nl_MI);deviance(mod_SEM_Queen_local_nl_MI)


###### Comparison between LOG and MI for the QUEEN configuration matrix 
mod_lists_Queen <- 
  list(
    "SDM_LOG"  = mod_SDM_Queen_local_nl_log, 
    "SDEM_LOG" = mod_SDEM_Queen_local_nl_log,
    "SLX_LOG"  = mod_SLX_Queen_local_nl_log,
    "LAG_LOG"  = mod_LAG_Queen_local_nl_log,
    "SEM_LOG"  = mod_SEM_Queen_local_nl_log,
    "SDM_MI"  = mod_SDM_Queen_local_nl_MI, 
    "SDEM_MI" = mod_SDEM_Queen_local_nl_MI,
    "SLX_MI"  = mod_SLX_Queen_local_nl_MI,
    "LAG_MI"  = mod_LAG_Queen_local_nl_MI,
    "SEM_MI"  = mod_SEM_Queen_local_nl_MI
  )
lapply(mod_lists_Queen, AIC)
lapply(mod_lists_Queen, BIC)
qqPlot(mod_SDEM_Queen_local_nl_log$residuals)
qqPlot(mod_SDEM_Queen_local_nl_MI$residuals)

# ======================================================================
# ==== Case 3: W: distance 3KM , Log transformation LIHC 
# ======================================================================
# Using binary configuration with d= 3KM
nb_dist <- dnearneigh(coords, 0, 3000, longlat = FALSE) 
lw_distance <- nb2listw(nb_dist, style="W", zero.policy=TRUE) 
col_nb1_sf = spdep::nb2lines(lw_distance$neighbours, coords=coords, 
                             proj4string=crs_info$proj4string, as_sf=T) # weights are binary
n_neigh_3000 <- attr(lw_distance$weights, "comp")$d
g_def_wijken_23$n_neigh_3000 <- n_neigh_3000

# configuration formula 
var_target <- "log(LIHE)"
formula_block <- paste(var_target, " ~", paste(cols_model_nal_01, collapse = " + ", sep = " "))
formula_block <- formula(formula_block)
formula_local <- paste(" ~", paste(cols_local_sel_nal_01, collapse = " + ", sep = " "))
formula_local <- formula(formula_local)
spdep::lm.RStests(mod_pool_OLS_nl_log, lw_distance, test=c("adjRSerr", "adjRSlag"))

mod_SEM_dist3000_local_nl_log <- errorsarlm(formula_block,data = g_def_wijken_23, listw = lw_distance, tol.solve=1.0e-30, zero.policy=TRUE)
mod_LAG_dist3000_local_nl_log <- lagsarlm(formula_block, data = g_def_wijken_23, listw = lw_distance, zero.policy=TRUE) 
mod_SLX_dist3000_local_nl_log  <- spatialreg:::lmSLX(formula_block,data = g_def_wijken_23, lw_distance, zero.policy = TRUE, Durbin = formula_local)

mod_SDM_dist3000_local_nl_log  <- spatialreg:::lagsarlm(formula_block,data = g_def_wijken_23, lw_distance, 
                                                     Durbin = formula_local , zero.policy = TRUE)
mod_SDEM_dist3000_local_nl_log  <- spatialreg:::errorsarlm(formula_block, data = g_def_wijken_23, lw_distance, 
                                                        Durbin = formula_local , zero.policy = TRUE)

qqPlot(mod_SDM_dist3000_local_nl_log$residuals)

lrtest(mod_SDM_dist3000_local_nl_log, mod_SLX_dist3000_local_nl_log)
lrtest(mod_SDM_dist3000_local_nl_log, mod_SEM_dist3000_local_nl_log)

lrtest(mod_SDEM_dist3000_local_nl_log, mod_SLX_dist3000_local_nl_log)
lrtest(mod_SDEM_dist3000_local_nl_log, mod_SEM_dist3000_local_nl_log)

mod_lists_Distance <- 
  list(
    "SDM_LOG"  = mod_SDM_dist3000_local_nl_log, 
    "SDEM_LOG" = mod_SDEM_dist3000_local_nl_log,
    "SLX_LOG"  = mod_SLX_dist3000_local_nl_log,
    "LAG_LOG"  = mod_LAG_dist3000_local_nl_log,
    "SEM_LOG"  = mod_SEM_dist3000_local_nl_log
  )

sort(sapply(mod_lists_Distance, AIC))
sort(sapply(mod_lists_Distance, BIC))
sort(sapply(mod_lists_Distance, logLik))

# Likelihood testing beteen SEM and SDEM 
LR.Sarlm(mod_SEM_dist3000_local_nl_log, mod_SDEM_dist3000_local_nl_log)

# Generating sumaries for comparison 
W <- as(lw_distance, "CsparseMatrix")
set.seed(500L)
trMC <- trW(W, type="MC", p = 20) #default p = 16, 
impacts_SDM_dist3000_local_nl_log <- spatialreg:::impacts(mod_SDM_dist3000_local_nl_log, tr = trMC, R=200, Q=5)
summary_SDM_dist3000_local_nl_log <- summary(impacts_SDM_dist3000_local_nl_log, zstats=TRUE, short=TRUE)
summary_SDM_dist3000_local_nl_log

# ======================================================================
# ==== Case 4: W: distance 5.5KM , Log transformation LIHC 
# ======================================================================
# Using binary configuration with d= 5.5KM
nb_dist <- dnearneigh(coords, 0, 5500, longlat = FALSE) 
lw_distance_5KM <- nb2listw(nb_dist, style="W", zero.policy=TRUE) 
col_nb1_sf = spdep::nb2lines(lw_distance_5KM$neighbours, coords=coords, 
                             proj4string=crs_info$proj4string, as_sf=T) # weights are binary
n_neigh_5000 <- attr(lw_distance_5KM$weights, "comp")$d
g_def_wijken_23$n_neigh_5000 <- n_neigh_5000

# Formula configuration 
var_target <- "log(LIHE)"
formula_block <- paste(var_target, " ~", paste(cols_model_nal_01, collapse = " + ", sep = " "))
formula_block <- formula(formula_block)
formula_local <- paste(" ~", paste(cols_local_sel_nal_01, collapse = " + ", sep = " "))
formula_local <- formula(formula_local)
spdep::lm.RStests(mod_pool_OLS_nl_log, lw_distance_5KM, test=c("adjRSerr", "adjRSlag"))

mod_SEM_dist5000_local_nl_log <- errorsarlm(formula_block,data = g_def_wijken_23, listw = lw_distance_5KM, tol.solve=1.0e-30, zero.policy=TRUE)
mod_LAG_dist5000_local_nl_log <- lagsarlm(formula_block, data = g_def_wijken_23, listw = lw_distance_5KM, zero.policy=TRUE) 
mod_SLX_dist5000_local_nl_log  <- spatialreg:::lmSLX(formula_block,data = g_def_wijken_23, lw_distance_5KM, zero.policy = TRUE, Durbin = formula_local)

mod_SDM_dist5000_local_nl_log  <- spatialreg:::lagsarlm(formula_block,data = g_def_wijken_23, lw_distance_5KM, 
                                                        Durbin = formula_local , zero.policy = TRUE)
mod_SDEM_dist5000_local_nl_log  <- spatialreg:::errorsarlm(formula_block, data = g_def_wijken_23, lw_distance_5KM, 
                                                           Durbin = formula_local , zero.policy = TRUE)



# ======================================================================
# ==== Case 4: W: distance 3KM , No transformation LIHC 
# ======================================================================
# Using binary configuration with d= 5.5KM
var_target <- "LIHE"
formula_blok_MI <- paste(var_target, " ~", paste(cols_model_nal_01, collapse = " + ", sep = " "))
formula_blok_MI <- formula(formula_blok_MI)
formula_local_MI <- paste(" ~", paste(cols_local_sel_nal_01, collapse = " + ", sep = " "))
formula_local_MI <- formula(formula_local_MI)

mod_pool_OLS_nl_MI <- lm(formula_blok_MI, data = g_def_wijken_23)

spdep::lm.RStests(mod_pool_OLS_nl_MI, lw_distance, test=c("adjRSerr", "adjRSlag"))

mod_SEM_dist3000_local_nl_MI  <- errorsarlm(formula_blok_MI,data = g_def_wijken_23, listw = lw_distance, tol.solve=1.0e-30, zero.policy=TRUE)
mod_LAG_dist3000_local_nl_MI <- lagsarlm(formula_blok_MI, data = g_def_wijken_23, listw = lw_distance, zero.policy=TRUE) 
mod_SLX_dist3000_local_nl_MI  <- spatialreg:::lmSLX(formula_blok_MI,data = g_def_wijken_23, lw_distance, zero.policy = TRUE, Durbin = formula_local_MI)

mod_SDM_dist3000_local_nl_MI  <- spatialreg:::lagsarlm(formula_blok_MI,data = g_def_wijken_23, lw_distance, 
                                                    Durbin = formula_local_MI , zero.policy = TRUE)
mod_SDEM_dist3000_local_nl_MI  <- spatialreg:::errorsarlm(formula_blok_MI, data = g_def_wijken_23, lw_distance, 
                                                       Durbin = formula_local_MI , zero.policy = TRUE)

lrtest(mod_SDM_dist3000_local_nl_MI, mod_SLX_dist3000_local_nl_MI)
lrtest(mod_SDM_dist3000_local_nl_MI, mod_SEM_dist3000_local_nl_MI)

lrtest(mod_SDEM_dist3000_local_nl_MI, mod_SLX_dist3000_local_nl_MI)
lrtest(mod_SDEM_dist3000_local_nl_MI, mod_SEM_dist3000_local_nl_MI)

deviance(mod_SDEM_Queen_local_nl_MI); deviance(mod_SLX_Queen_local_nl_MI)


###### Comparison between LOG and MI for the QUEEN configuration matrix 
mod_lists_distance <- 
  list(
    "SDM_LOG"  = mod_SDM_dist3000_local_nl_log, 
    "SDEM_LOG" = mod_SDEM_dist3000_local_nl_log,
    "SLX_LOG"  = mod_SLX_dist3000_local_nl_log,
    "LAG_LOG"  = mod_LAG_dist3000_local_nl_log,
    "SEM_LOG"  = mod_SEM_dist3000_local_nl_log,
    "SDM_MI"  = mod_SDM_dist3000_local_nl_MI, 
    "SDEM_MI" = mod_SDEM_dist3000_local_nl_MI,
    "SLX_MI"  = mod_SLX_dist3000_local_nl_MI,
    "LAG_MI"  = mod_LAG_dist3000_local_nl_MI,
    "SEM_MI"  = mod_SEM_dist3000_local_nl_MI
  )
lapply(mod_lists_distance, AIC)
lapply(mod_lists_distance, BIC)
qqPlot(mod_SDEM_Queen_local_nl_log$residuals)
qqPlot(mod_SDEM_Queen_local_nl_MI$residuals)

# ======================================================================
# ==== Section 2: Model comparison               
# ======================================================================
# Comparison Queen and Distance based 
mod_lists_SDEM <- 
  list(
    "SDEM_LOG_Queen"  = mod_SDEM_Queen_local_nl_log, 
    "SDEM_LOG_Distance" = mod_SDEM_dist3000_local_nl_log,
    "SDEM_LOG_Distance5KM" = mod_SDEM_dist5000_local_nl_log
  )
lapply(mod_lists_SDEM, AIC)
lapply(mod_lists_SDEM, BIC)
lapply(mod_lists_SDEM, logLik)
anova(mod_SDEM_Queen_local_nl_log, mod_SDEM_dist3000_local_nl_log)
lrtest(mod_SDEM_Queen_local_nl_log, mod_SDEM_dist3000_local_nl_log)

summary(mod_SDEM_Queen_local_nl_log)
summary(mod_SDEM_dist3000_local_nl_log)
summary(mod_SDEM_dist5000_local_nl_log)
summary(mod_SDEM_Queen_local_extended_nl_log)
summary(mod_SDEM_Queen_local_full_nl_log)

# ======================================================================
# ==== Section 3: Output results
# ======================================================================
# for formatting the results 

rename_summary_coefficients <- c(
  "Intercept",
  "Fragile older old", 
  "Pension", 
  "Unemployment rate", 
  "Precarious living", 
  "Disability", 
  "Single person household",
  "Household young children",
  "Education: Secondary",
  "Education: Low",
  "Non-European,NL-born", #--> "Migrant background outside Europe, born NL",
  "European,foreign-born", #--> "Migrant background Europe, born outside NL",
  "Non-European,foreign born", #--> "Migrant background outside europe, born outside NL",
  "Rental housing corporations",
  "Rental private-own",
  "Gas consumption rental",
  "Electricity consumption rental",
  "Heating: Gas fired",
  "Heating: Electrical",
  "Home without solar PV",
  "Property value: WOZ",
  "Urbanity: Urban",
  "Urbanity: No-Urban",
  "House type: Corner",
  "House type: Detached",
  "House type: Semi-detached",
  "House type: Terraced",
  "Construction year 1948-1981",
  "Construction year 1982-2002",
  "Construction year After.2002",
  "Energetic efficiency: Low",
  "Energetic efficiency: Medium",
  "Green infras.",
  "Gray infras.",
  "CDD",
  "HDD",
  "lag.Unemployment rate", 
  "lag.Home without solar PV", 
  "lag.House type: Corner", 
  "lag.House type: Detached", 
  "lag.House type: Semi-detached", 
  "lag.House type: Terraced", 
  "lag.Construction year 1948-1981", 
  "lag.Construction year 1982-2002", 
  "lag.Construction year After.2002", 
  "lag.Property value: WOZ",
  "Lambda"
)

# Estimations and p-values
plotreg(mod_lists_SDEM)
texreg(mod_lists_SDEM, custom.coef.names = rename_summary_coefficients)

fun_transform_coef <- function(x){
    coeff_model <- coef(x)
    coeff_names <- names(coeff_model)
    coeff_scaled <- NULL
    for(ii in 1:length(coeff_names)){
      # print(coeff_names[ii])
      if(coeff_names[ii] == "lambda"){
        coeff_scaled[ii] = coeff_model[ii]
      }
      if(coeff_names[ii] == "(Intercept)" ){
        coeff_scaled[ii] <- exp(coeff_model[ii]) * 100
      }
      if(!coeff_names[ii] %in% c("(Intercept)", "lambda")){
        coeff_scaled[ii] <- (exp(coeff_model[ii])-1)*100
      }
    }
    names(coeff_scaled) <- coeff_names
    coeff_scaled <- c(coeff_scaled[2:length(coeff_scaled)], coeff_scaled[1])
    return(coeff_scaled)
}

mod_extract_pvalues <- function(object, adj.se = FALSE){
      if (object$type == "error" || ((object$type == "lag" || object$type == 
                                      "mixed" || object$type == "sac" || object$type == "sacmixed") && 
                                     object$ase)) {
        object$coeftitle <- "(asymptotic standard errors)"
        SE <- object$rest.se
        if (adj.se) {
          N <- length(residuals(object))
          adj <- N/(N - (length(object$coefficients)))
          SE <- sqrt((SE^2) * adj)
        }
        object$Coef <- cbind(object$coefficients, SE, object$coefficients/SE, 
                             2 * (1 - pnorm(abs(object$coefficients/SE))))
        colnames(object$Coef) <- c("Estimate", "Std. Error", 
                                   ifelse(adj.se, "t value", "z value"), "Pr(>|z|)")
        rownames(object$Coef) <- names(object$coefficients)
      }
  }

coef_lists_SDEM <- lapply(mod_lists_SDEM, fun_transform_coef)
# createTexreg(coef.names = rename_summary_coefficients,
#              coef = coef_lists_SDEM$SDEM_LOG_Queen,
#              pvalues = coef_lists_SDEM$SDEM_LOG_Queen,
#              )

texreg(mod_lists_SDEM)
texreg(mod_lists_SDEM$SDEM_LOG_Queen, 
       custom.coef.names = rename_summary_coefficients, 
       override.coef = coef_lists_SDEM$SDEM_LOG_Queen)

W <- as(lw_queen, "CsparseMatrix")
set.seed(500L)
trMC <- trW(W, type="MC", p = 20) #default p = 16, 
impacts_SDEM_Queen_local_nl_log <- spatialreg:::impacts(mod_SDEM_Queen_local_nl_log, tr = trMC, R=200, Q=5)
summary_SDEM_Queen_local_nl_log<- summary(impacts_SDEM_Queen_local_nl_log, zstats=TRUE, short=TRUE)
summary_SDEM_Queen_local_nl_log

# Spillover effects: direct and indirect estimations 
spillover_estimation <- function(model, W){
  set.seed(500L)
  trMC <- trW(W, type="MC", p = 20) #default p = 16, 
  impacts <- spatialreg:::impacts(model, tr = trMC, R=200, Q=5)
  summary <- summary(impacts, zstats=TRUE, short=TRUE)
  list(impacts = impacts, signif = summary)
}

W_Queen <- as(lw_queen, "CsparseMatrix")
W_Distance <- as(lw_distance, "CsparseMatrix")

##### Impact estimation:  log 
impacts_SDEM_Queen_log <- spillover_estimation(mod_SDEM_Queen_local_nl_log, W_Queen)$impacts
impacts_SDEM_Distance_log <- spillover_estimation(mod_SDEM_dist3000_local_nl_log, W_Distance)$impacts
impacts_SDM_Queen_log <- spillover_estimation(mod_SDM_Queen_local_nl_log, W_Queen)$impacts
impacts_SDM_Distance_log <- spillover_estimation(mod_SDM_dist3000_local_nl_log, W_Distance)$impacts


#### log SDEM - Queen 
data.frame(impacts_SDEM_Queen_log$impacts$direct,
           impacts_SDEM_Distance_log$impacts$direct, 
           v = round(sort(impacts_SDEM_Queen_log$impacts$direct - impacts_SDEM_Distance_log$impacts$direct), 3))
#### log SDEM - Distance 
data.frame(impacts_SDEM_Queen_log$impacts$direct,
           impacts_SDEM_Distance_log$impacts$direct, 
           v = round(sort(impacts_SDEM_Queen_log$impacts$direct - impacts_SDEM_Distance_log$impacts$direct), 3))

#### log SDM - Queen
data.frame(impacts_SDM_Queen_log$res$direct,impacts_SDM_Distance_log$res$direct, v = round(sort(impacts_SDM_Queen_log$res$direct - impacts_SDM_Distance_log$res$direct), 3))
#### log SDM - Queen

## log SDM Queen vs Distance 
data.frame(impacts_SDM_Queen_log$res$direct,impacts_SDM_Distance_log$res$direct, v = round(sort(impacts_SDM_Queen_log$res$direct - impacts_SDM_Distance_log$res$direct), 3))


#  Impact estimation:  Without transformation 
impacts_SDEM_Queen_MI <- spillover_estimation(mod_SDEM_Queen_local_nl_MI, W_Queen)$impacts
impacts_SDEM_Distance_MI <- spillover_estimation(mod_SDEM_dist3000_local_nl_MI, W_Distance)$impacts
data.frame(impacts_SDEM_Queen_MI$impacts$direct,impacts_SDEM_Distance_MI$impacts$direct, v = round(sort(impacts_SDEM_Queen_MI$impacts$direct - impacts_SDEM_Distance_MI$impacts$direct), 3))


impacts_SDM_Queen_MI <- spillover_estimation(mod_SDM_Queen_local_nl_MI, W_Queen)$impacts
impacts_SDM_Distance_MI <- spillover_estimation(mod_SDM_dist3000_local_nl_MI, W_Distance)$impacts
data.frame(impacts_SDM_Queen_MI$res$direct,impacts_SDM_Distance_MI$res$direct, v = round(sort(impacts_SDM_Queen_MI$res$direct - impacts_SDM_Distance_MI$res$direct), 3))

data.frame(impacts_SDEM_Queen_MI$impacts$direct,impacts_SDM_Queen_MI$res$direct, v = round(sort(impacts_SDEM_Queen_MI$impacts$direct - impacts_SDM_Queen_MI$res$direct), 3))
data.frame(impacts_SDEM_Distance_MI$impacts$direct,impacts_SDM_Distance_MI$res$direct, v = round(sort(impacts_SDEM_Distance_MI$impacts$direct - impacts_SDM_Distance_MI$res$direct), 3))


AIC(mod_SDEM_Queen_local_nl_MI);AIC(mod_SDM_Queen_local_nl_MI)
AIC(mod_SDEM_dist3000_local_nl_MI);AIC(mod_SDM_dist3000_local_nl_MI)

set_models <- list((mod_SDM_Queen_local_nl_log), (mod_SDEM_Queen_local_nl_log),
                (mod_SDM_dist3000_local_nl_log), (mod_SDEM_dist3000_local_nl_log),
                (mod_SDM_Queen_local_nl_MI), (mod_SDEM_Queen_local_nl_MI),
                (mod_SDM_dist3000_local_nl_MI), (mod_SDEM_dist3000_local_nl_MI))

data.frame("AIC" = sapply(set_models, AIC),
           "BIC" = sapply(set_models, BIC))


# ======================================================================
# ==== Section 4: Comparison SDEM and SDM
# ======================================================================
# Two models SDEM and SDM 
spillover_estimation(mod_SDEM_Queen_local_nl_log, W_Queen)
spillover_estimation(mod_SDEM_dist3000_local_nl_log, W_Distance)
spillover_estimation(mod_SDM_Queen_local_nl_log, W_Queen)

summary(mod_SDEM_Queen_local_nl_log)
summary(mod_SDEM_dist3000_local_nl_log)
summary(mod_SDEM_dist5000_local_nl_log)


# ======================================================================
# ==== Section 5: GEnerating main outputs for SDEM. W: Queen configuration
# ======================================================================

spillover_estimation(mod_SDEM_Queen_local_nl_log, W_Queen)

# Save workspace 
save.image("C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/02_Paper_energy_poverty/Energy poverty Analysis/data/wijken/00_intermediate_data/work_space_LIHE.RData")
