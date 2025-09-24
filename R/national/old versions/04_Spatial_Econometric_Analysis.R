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
# library(maptools)
library(RColorBrewer)
library(leaflet)
library(ggplot2)
library(tmap)
library(tmaptools)
library(lmtest)
library(splm)
library(data.table)
library(sphet)
library(spgwr)
# library(rgeos)
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
#### Global variables

outpath_figure <- "output/R/econometric/final_results/Spatial econometric/"
input_data <- "output/R/data/03_Spatial Data Processing/"
load(file = file.path("output/R/data/03_Spatial Data Processing/", "g_def_wijken_23.Rdata"))
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

pallete_viridis_hex <- c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154")
pallete_magma_hex <- c("#fcfdbf", "#fe9f6d", "#de4968", "#8c2981", "#3b0f70", "#000004")
##################################################################################################
### Variable selection ###########################################################################
##################################################################################################

summary_cols <- c(
  "p_LIHC", 
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
  # "urbanity_degree",
  
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

# dd <- c("p_pop_Origin_NL","p_pop_Origin_Europe", "p_pop_Origin_Outside_Europe", "p_pop_Origin_NL_Born_NL", 
# "p_pop_Origin_NL_Born_Europe", "p_pop_Origin_NL_Born_Outside_Europe", "p_pop_Origin_Outside_NL_Born_Europe", "p_pop_Origin_Outside_NL_Born_Outside_Europe")

plot(g_def_wijken_23$p_precarious_part_time, g_def_wijken_23$p_disability)
g_def_wijken_23$p_LIHC <- g_def_wijken_23$LIHE
g_def_wijken_23 <- merge(g_def_wijken_23, dat_COROP, by.x = "GM_CODE_23_x", by.y = "GM_CODE", all.x = T)
################## Descriptives ########################################################################
df_def_wijken_23 <- data.frame(g_def_wijken_23)
# summary_cols <- variable_summary[!variable_summary %in% "urbanity_degree"]
# summary_cols <- c("p_LIHC", summary_cols)
rename_summary_cols <- c(
  "LIHC", 
  "Fragile older old", 
  "Pension", 
  "Unemployment rate", 
  "Precarious living", 
  "Disability", 
  "Single person \nhousehold",
  "Household young \nchildren",
  "Education: Secondary",
  "Education: Low",
  "Non-European,\nNL-born", #--> "Migrant background outside Europe, born NL",
  "European,\nforeign-born", #--> "Migrant background Europe, born outside NL",
  "Non-European,\nforeign born", #--> "Migrant background outside europe, born outside NL",
  "Rental housing \ncorporations",
  "Rental private-own",
  "Gas consumption \nrental",
  "Electricity consumption \nrental",
  "Heating: Gas fired",
  "Heating: Electrical",
  "Home without \nsolar PV",
  "Property value: WOZ",
  # "Urbanity: Urban",
  # "Urbanity: No-Urban",
  
  # "House type: Apartment",
  "House type: Corner",
  "House type: Detached",
  "House type: Semi-detached",
  "House type: Terraced",
  "Construction year \n1948-1981",
  "Construction year \n1982-2002",
  "Construction year \nAfter.2002",
  "Energetic efficiency:\nLow",
  "Energetic efficiency:\nMedium",
  "Green infras.",
  "Gray infras.",
  "CDD",
  "HDD"
)
  
df_def_wijken_23 <- data.frame(g_def_wijken_23)
g_def_wijken_Urban_23 <- g_def_wijken_23 %>%
  filter(urbanity_degree %in% c("urban"))
g_def_wijken_NoUrban_23 <- g_def_wijken_23 %>%
  filter(!urbanity_degree %in% c("urban", "semi_urban"))

summary_GM <- df_def_wijken_23 %>% 
  group_by(REGIO_NAAM, 
           # urbanity_degree, 
           GM_NAAM_x) %>% 
  summarise(mean = mean(p_LIHC), 
            max = max(p_LIHC),
            mean_unem = mean(p_unemployment_rate)) %>%
  arrange(mean)

summary_Regio <- df_def_wijken_23 %>% 
  group_by(REGIO_NAAM) %>% 
  summarise(mean = mean(p_LIHC), 
            max = max(p_LIHC)) %>%
  arrange(mean)

summary_Regio <- df_def_wijken_23 %>% 
  group_by(REGIO_NAAM) %>% 
  summarise(mean1 = mean(p_h_year_cons_Before.1924), 
            mean2 = mean(p_h_year_cons_1925.1947), ) %>%
  arrange(mean1)

summary_COROP <- df_def_wijken_23 %>% 
  group_by(COROP_NAME) %>% 
  summarise(mean = mean(p_LIHC), 
            max = max(p_LIHC)) %>%
  arrange(mean)

homologate_names <- data.frame(var_name = summary_cols, homologate = rename_summary_cols)
summary_df <- map_dfr(
  .x = summary_cols,
  .f = \(var) {
    df_def_wijken_23 |>
      summarise(
        variable = var,
        mean = round(mean(.data[[var]], na.rm = TRUE), digits = 1),
        sd = round(sd(.data[[var]], na.rm = TRUE), digits = 1),
        median = round(median(.data[[var]], na.rm = TRUE), digits = 1),
        min = round(min(.data[[var]], na.rm = TRUE), digits = 1),
        max = round(max(.data[[var]], na.rm = TRUE), digits = 1)
      )
  }
)
summary_df$variable <- rename_summary_cols


df_def_wijken_23 %>% 
  group_by(urbanity_degree) %>% 
  summarise(apartment = median(p_h_type_apartment, na.rm = TRUE),
            corner = median(p_h_type_corner_house, na.rm = TRUE),
            detached = median(p_h_type_detached, na.rm = TRUE),
            semi_detached = median(p_h_type_semi_detached, na.rm = TRUE),
            terraced = median(p_h_type_terraced_house_semi_detached, na.rm = TRUE)
            )

df_def_wijken_23 %>% 
  # group_by(urbanity_degree) %>% 
  summarise(apartment = median(p_h_type_apartment, na.rm = TRUE),
            corner = median(p_h_type_corner_house, na.rm = TRUE),
            detached = median(p_h_type_detached, na.rm = TRUE),
            semi_detached = median(p_h_type_semi_detached, na.rm = TRUE),
            terraced = median(p_h_type_terraced_house_semi_detached, na.rm = TRUE)
  )

################## Simulation to pick the best distance to configure W ##########################################
## variable list with reference categories 
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

###########################################################################################3
###########################################################################################3
###  Testing with W = distance, but binary contiguity structure ###########################3
###########################################################################################3
###########################################################################################3
## The spatialreg package does not allow to run general matrices types. It is necessary to 
## use another package 

#### Sensitivity analysis under the basis of differnt W structures 
#### W = 2,3,4,5 KM 
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

# png("../../../output/R/econometric/Semivariogram.png", width = 600, height = 400)
plot(var_gram$dist, var_gram$gamma, main = "semivariogram", xlab = "Distance (meters)", ylab = "Gamma")
abline(v = 40000, lty = 2, col = "gray")
p <- ggplot(var_gram, aes(x = dist/1000, y = gamma)) +
  geom_point() + 
  geom_smooth(method = "loess", span = 0.3) + scale_x_continuous(breaks= get_breaks(n = 20), limits = c(0, 20000/1000)) + 
  theme(axis.text = element_text(size = 18)) + 
  geom_vline(xintercept = 3, colour = 'red', linetype = 2) + 
  ggtitle("Semivariogram - distance decay approximation")
p
ggsave(plot = p, filename = file.path(outpath_figure, "semivariogram.png"))

# dev.off()

coords <- st_coordinates(g_def_wijken_23$centroids)
g_def_wijken_23 <- cbind(g_def_wijken_23, coords)

matrix_dist <- dist(coords)
matrix_dist <- as.matrix(matrix_dist)

vect_distances <- c(seq(500, 900, by  = 100), seq(1000, 5500, by = 500))
# list_SDM_model <- list()
list_SDEM_model <- list()
t1 <- Sys.time()
for(ii in 1:length(vect_distances)){
  print(vect_distances[ii])
  nb_dist <- dnearneigh(coords, 0, vect_distances[ii], longlat = FALSE) 
  lw_distance <- nb2listw(nb_dist, style="W", zero.policy=TRUE) 
  # list_SDM_model[[ii]]  <- spatialreg:::lagsarlm(formula_blok,data = g_def_wijken_23, 
  #                                                listw = lw_distance,
  #                                                Durbin = formula_local , zero.policy = TRUE)
  
  list_SDEM_model[[ii]]  <- spatialreg:::errorsarlm(formula_block, data = g_def_wijken_23, lw_distance, 
                                                   Durbin = formula_local , zero.policy = TRUE)
}

difftime(t1, Sys.time())
list_SDEM_model <- list_SDEM_model
sens_AIC <- sapply(list_SDEM_model, AIC)
sens_BIC <- sapply(list_SDEM_model, BIC)
sens_LogLik <- sapply(list_SDEM_model, logLik)
utrecht <- g_def_wijken_23 %>% filter(GM_NAAM_23_x == "Utrecht")
thehague <- g_def_wijken_23 %>% filter(GM_NAAM_23_x == "'s-Gravenhage")
summary(utrecht$p_LIHC)
summary(thehague$p_LIHC)

ppp <- data.frame(
  "Type" = c(rep("AIC", length(sens_AIC)), rep("BIC", length(sens_AIC)), rep("LogLik", length(sens_AIC))),
  "Distance" = rep(vect_distances/1000, 3),
  'Quality_Fit' = c(sens_AIC, sens_BIC, sens_LogLik))
ppp$Type <- factor(ppp$Type)
breaks_plot <- c(0, 500, 1000, vect_distances[-(0:6)])/1000

theme_ggplot <- theme_minimal() +
  theme(
    rect = element_rect(fill = "transparent"), 
    # Set background color to white
    #panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    #panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    # axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    # axis.line.y.left = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_text(size = 14),
    # Remove labels from the vertical axis
    # But customize labels for the horizontal axis
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


htmlreg(list_SDM_model,
        digits = 5,
        file = file.path(outpath_figure, "SENS_W_SDM.html"))

################################################################################################
################################################################################################
##############3 Running the model based on distance = 3000m ####################################
################################################################################################
################################################################################################
crs_info <- st_crs(g_def_wijken_23)
plot_neighbours_W(param_dist = 1500, outpath_figure)
plot_neighbours_W(param_dist = 3000, outpath_figure)
plot_neighbours_W(param_dist = 3500, outpath_figure)
plot_neighbours_W(param_dist = 4500, outpath_figure)

###########################################################################################
###### Choosing the model #################################################################
###########################################################################################
###########################################################################################################
####### Matrix composition is QUEEN
###########################################################################################################

##### LOG 
coords <- st_coordinates(g_def_wijken_23$centroids)
nb <- poly2nb(g_def_wijken_23, queen=TRUE, snap = 100) ### it works well in the analysis of contiguity 
lw_queen <- nb2listw(nb, style="W", zero.policy=TRUE) ### W: row type of standardization 
summary(nb)

coords_urban <- st_coordinates(g_def_wijken_Urban_23$centroids)
nb_urban <- poly2nb(g_def_wijken_Urban_23, queen=TRUE, snap = 100) ### it works well in the analysis of contiguity 
lw_queen_urban <- nb2listw(nb_urban, style="W", zero.policy=TRUE) ### W: row type of standardization 


var_target <- "log(p_LIHC)"
formula_block <- paste(var_target, " ~", paste(cols_model_nal_01, collapse = " + ", sep = " "))
formula_block <- formula(formula_block)
formula_local <- paste(" ~", paste(cols_local_sel_nal_01, collapse = " + ", sep = " "))
formula_local <- formula(formula_local)

mod_pool_OLS_nl_log <- lm(formula_block, data = g_def_wijken_23)
spdep::lm.RStests(mod_pool_OLS_nl_log, lw_queen, test=c("adjRSerr", "adjRSlag"))
spdep::lm.LMtests(mod_pool_OLS_nl_log, lw_queen, test= "all")

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

png(file.path(outpath_figure, "Robustness_check", "QQplots_SDEM_SDM.png"),
    res = 1200, width = 8, height = 6, units = "in")
par(mfrow = c(1, 2))
qqPlot(mod_SDM_Queen_local_nl_log$residuals)
qqPlot(mod_SDEM_Queen_local_nl_log$residuals)
dev.off()

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

LR.Sarlm(mod_SEM_Queen_local_nl_log, mod_SDEM_Queen_local_nl_log)

####### Average distance between neighboors in the Queen configuration matrix:

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
    # Set background color to white
    #panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    #panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    # axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    # axis.line.y.left = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_text(size = 14),
    # Remove labels from the vertical axis
    # But customize labels for the horizontal axis
    axis.text.x = element_text( size = 16),
    axis.title.x = element_text( size = 16),
    axis.title.y = element_text( size = 14),
    
  ) +
  scale_y_continuous(name = "Density") +
  scale_x_continuous(name = "Distance (Km)", breaks = breaks_plot) 
ggsave(plot = pp, filename = file.path(outpath_figure, "Distance_Queen_And_3_5KM.png"), dpi=300, width = 8.5, height = 6, units = "in")  # Good default for LaTeX)
####### Without transformation 
###########################################################################################3
###########################################################################################3
###  Econometric models without any ransformation ###########################3
###########################################################################################3
###########################################################################################3
var_target <- "LIHE"
formula_blok_MI <- paste(var_target, " ~", paste(cols_model_nal_01, collapse = " + ", sep = " "))
formula_blok_MI <- formula(formula_blok_MI)
formula_local_MI <- paste(" ~", paste(cols_local_sel_nal_01, collapse = " + ", sep = " "))
formula_local_MI <- formula(formula_local_MI)

mod_pool_OLS_nl_MI <- lm(formula_blok_MI, data = g_def_wijken_23)

spdep::lm.RStests(mod_pool_OLS_nl_MI, lw_queen, test=c("adjRSerr", "adjRSlag"))

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

###########################################################################################################
####### Matrix composition is distance dependent Distance = 3KM
###########################################################################################################
nb_dist <- dnearneigh(coords, 0, 3000, longlat = FALSE) 
lw_distance <- nb2listw(nb_dist, style="W", zero.policy=TRUE) 
col_nb1_sf = spdep::nb2lines(lw_distance$neighbours, coords=coords, 
                             proj4string=crs_info$proj4string, as_sf=T) # weights are binary
n_neigh_3000 <- attr(lw_distance$weights, "comp")$d
g_def_wijken_23$n_neigh_3000 <- n_neigh_3000

##### LOG 
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

LR.Sarlm(mod_SEM_dist3000_local_nl_log, mod_SDEM_dist3000_local_nl_log)

W <- as(lw_distance, "CsparseMatrix")
set.seed(500L)
trMC <- trW(W, type="MC", p = 20) #default p = 16, 
impacts_SDM_dist3000_local_nl_log <- spatialreg:::impacts(mod_SDM_dist3000_local_nl_log, tr = trMC, R=200, Q=5)
summary_SDM_dist3000_local_nl_log <- summary(impacts_SDM_dist3000_local_nl_log, zstats=TRUE, short=TRUE)
summary_SDM_dist3000_local_nl_log

###########################################################################################################
####### Matrix composition is distance dependent Distance = 5KM
###########################################################################################################
nb_dist <- dnearneigh(coords, 0, 5500, longlat = FALSE) 
lw_distance_5KM <- nb2listw(nb_dist, style="W", zero.policy=TRUE) 
col_nb1_sf = spdep::nb2lines(lw_distance_5KM$neighbours, coords=coords, 
                             proj4string=crs_info$proj4string, as_sf=T) # weights are binary
n_neigh_5000 <- attr(lw_distance_5KM$weights, "comp")$d
g_def_wijken_23$n_neigh_5000 <- n_neigh_5000

##### LOG 
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


####### Without transformation 
###########################################################################################3
###########################################################################################3
###  Econometric models without any ransformation ###########################3
###########################################################################################3
###########################################################################################3
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

########################################################################################################################
####### Choosing the right model 
########################################################################################################################
#### Comparison Queen and Distance based 
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


##### Print results ###################################

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

########################################################################################################################
####### Comparison between direct and indirect estimations 
########################################################################################################################
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


##### Impact estimation:  Without transformation 
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


###############################################################################################################
###############################################################################################################
###############################################################################################################

# Two models SDEM and SDM 


spillover_estimation(mod_SDEM_Queen_local_nl_log, W_Queen)
spillover_estimation(mod_SDEM_dist3000_local_nl_log, W_Distance)
spillover_estimation(mod_SDM_Queen_local_nl_log, W_Queen)

summary(mod_SDEM_Queen_local_nl_log)
summary(mod_SDEM_dist3000_local_nl_log)
summary(mod_SDEM_dist5000_local_nl_log)


##############################################################################################################
##############################################################################################################
##############################################################################################################

spillover_estimation(mod_SDEM_Queen_local_nl_log, W_Queen)

##### Ssave workspace 
save.image("C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/02_Paper_energy_poverty/Energy poverty Analysis/data/wijken/00_intermediate_data/work_space_LIHE.RData")
load("C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/02_Paper_energy_poverty/Energy poverty Analysis/data/wijken/00_intermediate_data/work_space_LIHE.RData")

############# Analysis of results ###################################################################

##### Employment 
library(wesanderson)
ll <- df_def_wijken_23 %>% 
  group_by(REGIO_NAAM, 
           urbanity_degree) %>% 
  summarise(mean_un = mean(p_unemployment_rate),
            mean_ep = mean(p_LIHC)) %>% 
  data.frame %>%
  arrange(mean_un) 
ggplot(ll, aes(x = mean_un, y = mean_ep, colour = REGIO_NAAM )) + 
  geom_point(size = 5) + scale_color_discrete(RColorBrewer::brewer.pal(8, "Accent")) + 
  ylim(c(1,7))
  
summary(lm(log(p_LIHC) ~ p_unemployment_rate * REGIO_NAAM, data = df_def_wijken_23))


df_def_wijken_23 %>% 
  group_by(REGIO_NAAM, 
           urbanity_degree) %>% 
  summarise(mean_un = mean(p_disability),
            mean_ep = mean(p_LIHC)) %>% 
  data.frame %>%
  arrange(mean_un)
