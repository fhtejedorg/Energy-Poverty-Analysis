# ---- title: 05_Selection Contiguity Matrix.R ----
# Purpose: Selection configuration W 
# Inputs: def_wijken_23.shp COROP:Gebieden_in_Nederland_2023_85385NED.csv
# Outputs: Analysis
# Author: Fabio Tejedor 
# Date: 2025-01-01
# Usage: 05_Selection Contiguity Matrix.R

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

# ======================================================================
# ==== Section1: Configuring the best distance W          ==============
# ======================================================================

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


# ==========================================================================================
# Exploring via variogram given distances 
# ==========================================================================================

# Formulas configuration 
var_target <- "log(p_LIHC)"
formula_block <- paste(var_target, " ~", paste(cols_model_nal_01, collapse = " + ", sep = " "))
formula_block <- formula(formula_block)

formula_local <- paste(" ~", paste(cols_local_sel_nal_01, collapse = " + ", sep = " "))
formula_local <- formula(formula_local)

formula_local_extended <- paste(" ~", paste(cols_local_extended_sel_nal_01, collapse = " + ", sep = " "))
formula_local_extended <- formula(formula_local_extended)

var_gram <- variogram(formula_block, 
                      na.omit(g_def_wijken_23[, c("p_LIHC", cols_model_nal_01)]),
                      cutoff = 70000, width = 100)

plot(var_gram$dist, var_gram$gamma, main = "semivariogram", xlab = "Distance (meters)", ylab = "Gamma")
abline(v = 40000, lty = 2, col = "gray")
p <- ggplot(var_gram, aes(x = dist/1000, y = gamma)) +
  geom_point() + 
  geom_smooth(method = "loess", span = 0.3) + scale_x_continuous(breaks= get_breaks(n = 20), limits = c(0, 20000/1000)) + 
  theme(axis.text = element_text(size = 18)) + 
  geom_vline(xintercept = 3, colour = 'red', linetype = 2) + 
  ggtitle("Semivariogram - distance decay approximation")
ggsave(plot = p, filename = file.path(outpath_figure, "semivariogram.png"))

# ==========================================================================================
# Exploring via various distances: Binary configuration according distance, 
# Note: The spatialreg package does not allow general matrices configurations e.g. 
# Generalisation for weights configuration is not coded  
# ==========================================================================================

coords <- st_coordinates(g_def_wijken_23$centroids)
g_def_wijken_23 <- cbind(g_def_wijken_23, coords)

vect_distances <- c(seq(500, 900, by  = 100), seq(1000, 5500, by = 500))

# Testing directly via SDEM model. 
# For SDM model use: spatialreg:::lagsarlm(formula_blok,data = g_def_wijken_23, 
                        #                  listw = lw_distance,
                        #                  Durbin = formula_local , zero.policy = TRUE)
list_SDEM_model <- list()
t1 <- Sys.time()
for(ii in 1:length(vect_distances)){
  print(vect_distances[ii])
  nb_dist <- dnearneigh(coords, 0, vect_distances[ii], longlat = FALSE) 
  lw_distance <- nb2listw(nb_dist, style="W", zero.policy=TRUE) 
  list_SDEM_model[[ii]]  <- spatialreg:::errorsarlm(formula_block, data = g_def_wijken_23, lw_distance, 
                                                   Durbin = formula_local , zero.policy = TRUE)
}
difftime(t1, Sys.time())
# Using Information Criteria for selection 
list_SDEM_model <- list_SDEM_model
sens_AIC <- sapply(list_SDEM_model, AIC)
sens_BIC <- sapply(list_SDEM_model, BIC)
sens_LogLik <- sapply(list_SDEM_model, logLik)

ppp <- data.frame(
  "Type" = c(rep("AIC", length(sens_AIC)), rep("BIC", length(sens_AIC)), rep("LogLik", length(sens_AIC))),
  "Distance" = rep(vect_distances/1000, 3),
  'Quality_Fit' = c(sens_AIC, sens_BIC, sens_LogLik))
ppp$Type <- factor(ppp$Type)
breaks_plot <- c(0, 500, 1000, vect_distances[-(0:6)])/1000

theme_ggplot <- theme_minimal() +
  theme(
    rect = element_rect(fill = "transparent"), 
    axis.ticks.length = unit(0, "mm"),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text( size = 16),
    axis.title.x = element_text( size = 16),
    axis.title.y = element_text( size = 14),
  ) 

gg <- ggplot(ppp %>% filter(Type == "AIC"), aes(x = Distance, y = Quality_Fit)) + 
  theme(axis.text = element_text(size = 20)) + 
  geom_smooth(method = "loess", span = 0.3, colour = "#3b0f70") + 
  geom_point(size = 3, colour = "#de4968") + 
  scale_y_continuous(name = "AIC") +
  scale_x_continuous(name = "Distance (Km)", breaks = breaks_plot) +
  theme_ggplot
ggsave(plot = gg, filename = file.path(outpath_figure, "AIC_X_Distance.png"), width = 12, height = 8)


gg <- ggplot(ppp %>% filter(Type == "BIC"), aes(x = Distance, y = Quality_Fit)) + 
  theme(axis.text = element_text(size = 20)) + 
  geom_smooth(method = "loess", span = 0.3, colour = "#3b0f70") + 
  geom_point(size = 3, colour = "#de4968") + 
  scale_y_continuous(name = "BIC") +
  scale_x_continuous(name = "Distance (Km)", breaks = breaks_plot) +
  theme_ggplot
ggsave(plot = gg, filename = file.path(outpath_figure, "BIC_X_Distance.png"), width = 12, height = 8)


gg <- ggplot(ppp %>% filter(Type == "LogLik"), aes(x = Distance, y = Quality_Fit)) + 
  theme(axis.text = element_text(size = 20)) + 
  geom_smooth(method = "loess", span = 0.3, colour = "#3b0f70") + 
  geom_point(size = 3, colour = "#de4968") + 
  scale_y_continuous(name = "LogLik") +
  scale_x_continuous(name = "Distance (Km)", breaks = breaks_plot) +
  theme_ggplot
ggsave(plot = gg, filename = file.path(outpath_figure, "LogLik_X_Distance.png"), width = 12, height = 8)

save(list_SDEM_model, file = file.path(outpath_figure, "list_SDEM_model_simulation_distance_W.Rdata"))

gg <- ggplot(var_gram, aes(x = dist/1000, y = gamma)) +
  geom_point() + 
  geom_smooth(method = "loess", span = 0.3) + scale_x_continuous(breaks= get_breaks(n = 20), limits = c(0, 20000/1000)) + 
  theme(axis.text = element_text(size = 18)) + 
  geom_vline(xintercept = 3, colour = 'red', linetype = 2) + 
  ggtitle("Semivariogram - distance decay approximation")
ggsave(plot = gg, filename = file.path(outpath_figure, "semivariogram.png"))

### Comparative aoutput in html 
htmlreg(list_SDM_model,
        digits = 5,
        file = file.path(outpath_figure, "SENS_W_SDM.html"))

# Creating maps with the output network induced by the W matrix (distance based) configuration
crs_info <- st_crs(g_def_wijken_23)
plot_neighbours_W(param_dist = 1500, outpath_figure)
plot_neighbours_W(param_dist = 3000, outpath_figure)
plot_neighbours_W(param_dist = 3500, outpath_figure)
plot_neighbours_W(param_dist = 4500, outpath_figure)

# ======================================================================
# ==== Section2: Summary distances in a Queen configuration W ==============
# ======================================================================
# Configuration W 
coords <- st_coordinates(g_def_wijken_23$centroids)
nb <- poly2nb(g_def_wijken_23, queen=TRUE, snap = 100) ### it works well in the analysis of contiguity 
lw_queen <- nb2listw(nb, style="W", zero.policy=TRUE) ### W: row type of standardization 

####### Average distance between neighbors in the Queen configuration matrix:

neighbor_dists <- unlist(
  lapply(1:length(nb), function(i) {
    sapply(nb[[i]], function(j) {
      sqrt(sum((coords[i, ] - coords[j, ])^2))
    })
  })
)

mean(neighbor_dists)
median(neighbor_dists)
summary(neighbor_dists)
length(neighbor_dists)
neighbor_dists <- neighbor_dists[neighbor_dists!=0]
length(neighbor_dists)
neighbor_dists <- neighbor_dists/1000
neighbor_dists <- data.frame(neighbor_dists)
breaks_plot <- c(seq(0, 10, by = 1), seq(12, 22, by = 2))
pp <- ggplot(neighbor_dists, aes(x = neighbor_dists)) +
  geom_density(colour = "#3b0f70", adjust = 1.5, linewidth = 1) + 
  geom_vline(xintercept = 3, colour="#de4968", linetype = "longdash", linewidth = 1) + 
  geom_vline(xintercept = 5, colour="#fe9f6d", linetype = "longdash", linewidth = 1) + 
  theme_minimal() +
  theme(
    rect = element_rect(fill = "transparent"), 
    axis.ticks.length = unit(0, "mm"),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text( size = 16),
    axis.title.x = element_text( size = 16),
    axis.title.y = element_text( size = 14),
    
  ) +
  scale_y_continuous(name = "Density") +
  scale_x_continuous(name = "Distance (Km)", breaks = breaks_plot) 
ggsave(plot = pp, filename = file.path(outpath_figure, "Distance_Queen_And_3_5KM.png"), dpi=300, width = 8.5, height = 6, units = "in")  # Good default for LaTeX)
