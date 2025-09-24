# ---- title: 03_01_Data Exploration.R ----
# Purpose: Exploring consolidated data from the output 03_Spatial_DataProcessing.R 
# Inputs: g_def_wijken_23.Rdata, cbsgebiedsindelingen2023.gpkg", "gdf_gemeente_2023.shp"
# Outputs: Mainly plots, and statistics
# Author: Fabio Tejedor 
# Date: 2025-01-01
# Usage: R/03_01_Data Exploration.R

rm(list = ls())
gc()
setwd("C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/02_Paper_energy_poverty/Energy poverty Analysis")
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
input_data <- "output/R/data/03_Spatial Data Processing/"
outpath_figure <- "output/R/econometric/final_results/"

# ==== Loading data ================
load(file = file.path(input_data, "g_def_wijken_23.Rdata"))

# Provinces 
g_def_provincie <- st_read("data/wijken/00_intermediate_data/cbsgebiedsindelingen2016-2025/cbsgebiedsindelingen2023.gpkg", layer = "provincie_gegeneraliseerd")

# Municipalities/Gemeente dataset 
g_def_gemeente_23 <- st_read("gdf_gemeente_2023.shp")
# ==== tidy COROP to get provinces/regions ================

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
  ) %>% dplyr::select(GM_CODE, GM_NAAM, COROP_CODE, COROP_NAME, REGIO_CODE, REGIO_NAAM)

g_def_gemeente_23 <- g_def_gemeente_23 %>% dplyr::select(GM_CODE, GM_NAAM, STED)


# ==== Data exploration ================

##1)  ### Summary Urbanity degree 
tot_urbanity <- table(g_def_wijken_23$urbanity_degree)
summ_urban <- round(prop.table(tot_urbanity)*100, 2)
summ_urban <- data.frame(summ_urban)
summ_urban$total <- tot_urbanity

custom_labels <- c("urban" = "Urban", "semi_urban" = "Semi-urban", "no_urban" = "Non urban")
plt <- ggplot(summ_urban, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste("n:", total, sep = "")), vjust = -0.5, size = 4.5) +
  scale_fill_brewer(palette = "Set2") +  # Nice solid colors
  scale_x_discrete(labels = custom_labels, name = "") + 
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
    axis.line.y.left = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_text(size = 18),
    # Remove labels from the vertical axis
    # But customize labels for the horizontal axis
    axis.text.x = element_text( size = 18)
  ) +
  scale_y_continuous(name = "(%)", limits = c(0, 50)) 
plt
ggsave(plot = plt, filename = file.path(outpath_figure, "fig_summary_urban.png"), dpi=300, width = 6.5, height = 4, units = "in")  

##2)  ### Summary Urbanity degree 
def_wijken_23 <- g_def_wijken_23
dt_amsterdam <- def_wijken_23 %>% filter(GM_NAAM_23_x == "Amsterdam")
table(dt_amsterdam$wijk_predominant_typology)

dt_urban <- def_wijken_23 %>% filter(urbanity_degree %in% c("urban", "semi_urban"))
table(dt_urban$wijk_predominant_typology)

tab_g_urban_typo <- def_wijken_23 %>% 
  group_by(urbanity_degree, wijk_predominant_typology) %>% 
  summarise(median = median(p_LIHC, na.rm = TRUE),
            n = n())
tab_g_urban_typo <- tab_g_urban_typo %>% 
  group_by(urbanity_degree) %>%
  mutate(median_perc = median/sum(median)*100)

ttt <- def_wijken_23 %>% 
  group_by(wijk_predominant_typology) %>% 
  summarise(med_p_LIHC = median(p_LIHC, na.rm = TRUE),
            med_HEQ = median(HEQ, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(med_LIHC))

q_LIHC <- quantile(na.omit(def_wijken_23[, "p_LIHC"]))
def_wijken_23["LIHC_Q"] <- 
  cut(def_wijken_23[,"LIHC"], q_LIHC, c("Q1", "Q2", "Q3", "Q4"))

def_wijken_23 %>% 
  group_by(wijk_predominant_typology) %>% 
  summarise(med_LIHC = median(p_LIHC, na.rm = TRUE),
            med_HEQ = median(HEQ, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(med_LIHC))


def_wijken_23 %>% 
  group_by(LIHC_Q, urbanity_degree) %>% 
  summarise(med_CV = median(p_individual_CV, na.rm = TRUE),
            med_bh = median(p_block_heating, na.rm = TRUE),
            med_dh = max(p_district_heat, na.rm = TRUE),
            med_el = median(p_elec_heat, na.rm = TRUE)
  )
ggplot(tab_g_urban_typo, aes(urbanity_degree, median_perc , fill = wijk_predominant_typology )) +
  geom_bar(position="stack", stat="identity")


def_wijken_23 %>% 
  group_by(urbanity_degree) %>% 
  summarise(CV = mean(p_individual_CV, na.rm = TRUE),
            BLOCK = mean(p_block_heating, na.rm = TRUE),
            DISTRICT = mean(p_district_heat, na.rm = TRUE),
            ELECTRICITY = median(p_elec_heat, na.rm = TRUE),
            n = n())


summary(g_def_wijken_23$p_LIHC)
summary(log(g_def_wijken_23$p_LIHC))

png(filename = file.path(outpath_figure, "LIHC_Distribution.png"), width = 600, height = 500)
plot(density(g_def_wijken_23$p_LIHC), xlab = "LIHC", main = "Distribution LIHC")
dev.off()

png(filename = file.path(outpath_figure, "Log_LIHC_Distribution.png"), width = 600, height = 500)
plot(density(log(g_def_wijken_23$p_LIHC/100)), xlab = "Log LIHC", main = "Distribution Log LIHC")
dev.off()

### variogram analysis
var_gram <- variogram(log(p_LIHC) ~ 1, g_def_wijken_23)
plot(var_gram)

var_gram <- variogram(log(p_LIHC)  ~ p_fragile_health_65 + p_with_young_children + 
                        p_pension_coverage_rate + p_unemployment_rate + 
                        p_precarious_part_time + p_disability + 
                        p_rental_home, g_def_wijken_23)
plot(var_gram)

##### General summary 
def_wijken_23 <- data.frame(g_def_wijken_23)
def_wijken_23 %>% 
  group_by(urbanity_degree) %>% 
  summarise(median = median(p_LIHC, na.rm = TRUE),
            aver = mean(p_LIHC, na.rm = TRUE),
            min = min(p_LIHC, na.rm = TRUE), 
            max = max(p_LIHC, na.rm = TRUE),
            n = n())


summary_gm_urban <- def_wijken_23 %>% 
  group_by(GM_NAAM_23_x, urbanity_degree) %>% 
  summarise(median = median(p_LIHC, na.rm = TRUE),
            aver = mean(p_LIHC, na.rm = TRUE),
            min = min(p_LIHC, na.rm = TRUE), 
            max = max(p_LIHC, na.rm = TRUE),
            n = n()) %>%
  arrange(median)
summary_gm_urban <- data.frame(summary_gm_urban)
summary_gm_urban$geometry <- NULL


summary_gm_urban <- def_wijken_23 %>% 
  group_by(GM_NAAM_23_x, urbanity_degree) %>% 
  summarise(median = median(p_LIHC, na.rm = TRUE),
            aver = mean(p_LIHC, na.rm = TRUE),
            min = min(p_LIHC, na.rm = TRUE), 
            max = max(p_LIHC, na.rm = TRUE),
            n = n()) %>%
  arrange(median)
summary_gm_urban <- data.frame(summary_gm_urban)
summary_gm_urban$geometry <- NULL

######## Testing the global moran 

nb <- poly2nb(g_def_wijken_23, queen=TRUE, snap = 100) ### it works well in the analysis of contiguity 
lw_queen <- nb2listw(nb, style="W", zero.policy=TRUE) ### W: row type of standardization 

globalMoran <- moran.test(log(g_def_wijken_23$p_LIHC), lw_queen, na.action =  na.exclude)
globalMoran

###############################################################################################
###########################################################################################3
###########################################################################################3
###  Testing with W = QUEEN ###############################################################3
###########################################################################################3
###########################################################################################3

###########################################################################################3
##### Adjusting snap to find right way to find W 
#### Testing by using Amsterdam  
amsterdam <- g_def_wijken_23 %>% filter(GM_NAAM_23_x == "Amsterdam")
coords1 = sf::st_coordinates(amsterdam$centroids)
nb <- poly2nb(amsterdam, queen=TRUE, snap = 100) ## measure in meters 
col_nb1_sf = spdep::nb2lines(nb, coords=coords1, proj4string=crs_info$proj4string, as_sf=T) # weights are binary
pts <- st_point_on_surface(st_geometry(amsterdam))

ggplot() +
  geom_sf(data = amsterdam, fill = "white", colour = "red") +
  geom_sf(data = amsterdam$centroids, color = "blue") + 
  geom_sf(data = col_nb1_sf, color = "green")

plot(amsterdam[, "t_av_income_recepientX1000"])

plot(g_def_wijken_23[g_def_wijken_23$GM_NAAM_23_x == "'s-Gravenhage", ][, "t_av_income_recepientX1000"])

#### Extension to Netherlands 
coords <- st_coordinates(g_def_wijken_23$centroids)
nb <- poly2nb(g_def_wijken_23, queen=TRUE, snap = 100) ### it works well in the analysis of contiguity 
lw_queen <- nb2listw(nb, style="W", zero.policy=TRUE) ### W: row type of standardization 
summary(nb)
col_nb1_sf = spdep::nb2lines(nb, coords=coords, proj4string=crs_info$proj4string, as_sf=T) # weights are binary
pts <- st_point_on_surface(st_geometry(g_def_wijken_23))

pp <- ggplot() +
  geom_sf(data = g_def_wijken_23, fill = alpha("blue",0.2), colour = alpha("red",0.2)) +
  geom_sf(data = g_def_wijken_23$centroid, color = "blue", size = 0.001) + 
  geom_sf(data = col_nb1_sf, color = "green", linewidth = 0.1)
ggsave(plot = pp, 
       filename = file.path(outpath_figure, "network_QUEEN.tiff"), device='tiff', dpi=700)

pp <- ggplot() +
  geom_sf(data = g_def_wijken_23, fill = alpha("blue",0.2), colour = alpha("red",0.2)) +
  geom_sf(data = sf::st_centroid(g_def_wijken_23), color = "blue", size = 0.01) + 
  geom_sf(data = col_nb1_sf, color = "green", linewidth = 0.1) + 
  ggtitle("distance decay = 1.5KM")
###########################################################################################3
##### Autocorrelation analysis 
g_def_wijken_23$lag <- lag.listw(lw_queen, g_def_wijken_23$p_LIHC)
plot(g_def_wijken_23[, c("lag", "p_LIHC")])

globalMoran <- moran.test(g_def_wijken_23$p_LIHC, listw, na.action =  na.exclude)
globalMoran

# Create a regression model
M <- lm(lag ~ p_LIHC, g_def_wijken_23)

# Plot the data
png(filename = file.path(output_figures_path, "moran_I_LIHC.png"))
plot( lag ~ p_LIHC, temp_file, pch=21, asp=1, las=1, col = "grey40", bg="grey80")
abline(M, col="blue") # Add the regression line from model M
abline(v = mean(temp_file$p_LIHC), lty=3, col = "grey80")
abline(h = mean(temp_file$p_LIHC), lty=3, col = "grey80")
title("Moran-I Scatterplot")
dev.off()
moran(temp_file$p_LIHC, listw = lw, n = length(nb), S0 = Szero(lw))
test_moran <- moran.test(temp_file$p_LIHC, listw = lw)


####### 
moran.plot(x= temp_file$p_LIHC, y = temp_file$p_fragile_health_65, listw = lw)

moran.plot(x= temp_file$p_LIHC, y = temp_file$p_fragile_health_65, listw = lw)

moran.plot(x= temp_file$p_LIHC, y = temp_file$p_fragile_health_65, listw = lw)

moran.plot(x= temp_file$p_LIHC, y = temp_file$p_fragile_health_65, listw = lw)


nb2 <- st_contiguity(temp_file$geometry, queen = TRUE, snap = 100)
wt2 <- st_weights(nb2, allow_zero = T)

temp_file_lagged <- temp_file %>% 
  mutate(nb = nb2,
         wts = wt2,
         lag_p_LIHC = st_lag(p_LIHC, nb, wts),
         lag_fragile = st_lag(p_fragile_health_65 , nb, wts), 
         lag_with_children = st_lag(p_with_young_children  , nb, wts), 
         lag_pension = st_lag(p_pension_coverage_rate  , nb, wts),
         lag_unemployment = st_lag(p_unemployment_rate  , nb, wts),
         lag_precarious = st_lag(p_precarious_part_time  , nb, wts),
         lag_rental = st_lag(p_rental_home  , nb, wts),
         lisa = categorize_lisa(p_rental_home, lag_rental)
  )
ggplot(data = temp_file_lagged) + geom_sf(aes(fill = lisa))

######### Model specification: variables of interest ######################################
summary(g_def_wijken_23[c("p_pop_Origin_NL_Born_NL",
                          "p_pop_Origin_Europe_Born_NL",
                          "p_pop_Origin_Outside_Europe_Born_NL",
                          "p_pop_Origin_Europe_Born_Outside_NL",
                          "p_pop_Origin_Outside_Europe_Born_Outside_NL")])

#### to study the quadratic form os some variables
plot(g_def_wijken_23$p_fragile_health_65, log(g_def_wijken_23$p_LIHC))
plot(g_def_wijken_23$p_with_young_children, log(g_def_wijken_23$p_LIHC))
plot(g_def_wijken_23$p_single_person_household, log(g_def_wijken_23$p_LIHC))
plot(g_def_wijken_23$p_pension_coverage_rate, log(g_def_wijken_23$p_LIHC))
plot(g_def_wijken_23$p_unemployment_rate, log(g_def_wijken_23$p_LIHC))
plot(g_def_wijken_23$p_precarious_part_time, log(g_def_wijken_23$p_LIHC))
summary(lm(log(p_LIHC) ~ p_precarious_part_time, g_def_wijken_23))
abline(a = 7.867e-01, b = 6.298e-04)
mod_t_1 <- lm(log(p_LIHC) ~ p_precarious_part_time + p_unemployment_rate + p_disability, g_def_wijken_23)
summary(mod_t_1)
plot(g_def_wijken_23$p_disability, log(g_def_wijken_23$p_LIHC))
plot(g_def_wijken_23$ratio_rental_owner, g_def_wijken_23$p_LIHC)
plot(log(g_def_wijken_23$p_rental_home), g_def_wijken_23$p_LIHC)
plot(g_def_wijken_23$p_rental_corporations_home, log(g_def_wijken_23$p_LIHC))
plot(g_def_wijken_23$p_rental_private_landlord_home, log(g_def_wijken_23$p_LIHC))
summary(lm(log(p_LIHC) ~ p_rental_private_landlord_home , g_def_wijken_23))
summary(lm(log(p_LIHC) ~ p_rental_private_landlord_home + I(p_rental_private_landlord_home**2), g_def_wijken_23))

plot(g_def_wijken_23$p_gas_consum_rental, log(g_def_wijken_23$p_LIHC))
summary(lm(log(p_LIHC) ~ p_gas_consum_rental, g_def_wijken_23 %>% filter(p_gas_consum_rental > 0)))
abline(a =  -0.145266, b = 0.033049)

plot(g_def_wijken_23$p_elec_consum_rental, log(g_def_wijken_23$p_LIHC))
summary(lm(log(p_LIHC) ~ p_elec_consum_rental, g_def_wijken_23))

plot(g_def_wijken_23$p_elec_consum_rental, g_def_wijken_23$p_gas_consum_rental)

plot(g_def_wijken_23$p_elec_heat_high_gas_consum, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_homes_with_solar_panels, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_green, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_gray, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$CDD, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$HDD, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_h_type_apartment, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_h_type_detached, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_h_type_semi_detached, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_h_type_terraced_house_semi_detached, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_h_year_cons_1948.1981, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_h_year_cons_1982.2002, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_h_year_cons_After.2002, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_low_house_energy_eff, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_medium_house_energy_eff, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_high_house_energy_eff, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$WOZ, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_pop_Origin_Outside_Europe_Born_Outside_NL, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_education_Low, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$p_education_High, log(g_def_wijken_23$p_LIHC))

plot(g_def_wijken_23$urbanity_degree, log(g_def_wijken_23$p_LIHC))
##################################################################################################
### Variable selection ###########################################################################
##################################################################################################

cols_model_nal_01 <- c(
  "p_fragile_health_65", 
  # "p_without_young_children",
  "p_with_young_children",
  "p_single_person_household",
  "p_pension_coverage_rate", 
  "p_unemployment_rate", 
  "p_precarious_part_time", 
  "p_disability", 
  # "ratio_rental_owner",
  # "p_owner_ocupied_home", # Reference Unknown are very few (assumed here a negligible effect) 
  "p_rental_corporations_home",
  "p_rental_private_landlord_home",
  # "p_rental_unknown_home", # Reference 
  # "t_av_household_size",
  # "p_households_under_around_social_minimum",
  # Energy consumption 
  "p_gas_consum_rental",
  "p_elec_consum_rental",
  # "p_district_heat_high_gas_consum", 
  # "p_district_heat_low_gas_consum", 
  # "p_district_heat_no_gas_consum", 
  "p_individual_CV", 
  "p_elec_heat", 
  "p_homes_without_solar_panels", 
  "p_green", 
  "p_gray", 
  "CDD", 
  "HDD", 
  #### housing market variables 
  # "p_h_type_apartment", # Reference 
  "p_h_type_corner_house", 
  "p_h_type_detached", 
  "p_h_type_semi_detached", 
  "p_h_type_terraced_house_semi_detached", 
  
  # "p_h_year_cons_Before.1947", # Reference
  "p_h_year_cons_1948.1981",
  "p_h_year_cons_1982.2002",
  "p_h_year_cons_After.2002", 
  "p_low_house_energy_eff", 
  "p_medium_house_energy_eff", 
  # p_high_house_energy_eff # reference 
  # p_very_high_house_energy_eff # reference 
  
  # "building_median_age",
  "WOZ",
  # ethnicity 
  # "p_pop_Origin_NL_Born_NL",
  # "p_pop_Origin_Europe_Born_NL",
  "p_pop_Origin_Outside_Europe_Born_NL",
  "p_pop_Origin_Europe_Born_Outside_NL",
  "p_pop_Origin_Outside_Europe_Born_Outside_NL",
  # "ratio_ethnicity_NL_Outside",
  # 
  # "p_pop_western_residents",
  # "p_pop_Non_western_residents",
  # "p_pop_Other_Non_Western", Reference 
  
  # Education 
  
  "p_education_Low",
  "p_education_Secondary",
  # "p_education_High",  # Reference
  # Additional variables 
  # "p_rental_own_hous_association",
  # "GINI", 
  "urbanity_degree"
)

cols_local_sel_nal_01 <- c(
  # "p_fragile_health_65", 
  # "p_without_young_children",
  # "p_with_young_children",
  # "p_single_person_household",
  # "p_pension_coverage_rate", 
  "p_unemployment_rate",
  # "p_precarious_part_time", 
  # "p_disability", 
  # "ratio_rental_owner",
  # "p_owner_ocupied_home", # Reference Unknown are very few (assumed here a negligible effect) 
  # "p_rental_corporations_home",
  # "p_rental_private_landlord_home",
  # "p_rental_unknown_home", # Reference 
  # "t_av_household_size",
  # "p_households_under_around_social_minimum",
  # Energy consumption 
  # "p_gas_consum_rental",
  # "p_elec_consum_rental",
  # "p_district_heat_high_gas_consum", 
  # "p_district_heat_low_gas_consum", 
  # "p_district_heat_no_gas_consum", 
  # "p_individual_CV", 
  # "p_elec_heat", 
  "p_homes_without_solar_panels",
  # "p_green", 
  # "p_gray", 
  # "CDD", 
  # "HDD", 
  #### housing market variables 
  # "p_h_type_apartment", # Reference 
  "p_h_type_corner_house",
  "p_h_type_detached",
  "p_h_type_semi_detached",
  "p_h_type_terraced_house_semi_detached",
  
  # "p_h_year_cons_Before.1947", # Reference
  "p_h_year_cons_1948.1981",
  "p_h_year_cons_1982.2002",
  "p_h_year_cons_After.2002",
  # "p_low_house_energy_eff", 
  # "p_medium_house_energy_eff", 
  # p_high_house_energy_eff # reference 
  # p_very_high_house_energy_eff # reference 
  
  # "building_median_age",
  "WOZ"
  # ethnicity 
  # "p_pop_Origin_NL_Born_NL",
  # "p_pop_Origin_Europe_Born_NL",
  # "p_pop_Origin_Outside_Europe_Born_NL",
  # "p_pop_Origin_Europe_Born_Outside_NL",
  # "p_pop_Origin_Outside_Europe_Born_Outside_NL",
  # "ratio_ethnicity_NL_Outside",
  # 
  # "p_pop_western_residents",
  # "p_pop_Non_western_residents",
  # "p_pop_Other_Non_Western", Reference 
  
  # Education 
  
  # "p_education_Low",
  # "p_education_Secondary",
  # "p_education_High",  # Reference
  # Additional variables 
  # "p_rental_own_hous_association",
  # "GINI", 
  # "urbanity_degree"
)
colSums(is.na(data.frame(g_def_wijken_23[, cols_model_nal_01] )))


pp <- ggplot() + 
  geom_sf(data = g_def_wijken_23, aes(fill = p_LIHC), 
          linewidth  = 0.05, color = "transparent") + 
  scale_fill_viridis(
    option = "magma",
    name = "LIHC",
    na.value = "grey90",
    #na.translate = FALSE,
    # na.name = "Non sig.",
    alpha = 0.8, # make fill a bit brighter
    begin = 0.3, 
    end = 0.9,
    discrete = FALSE, # discrete classes, thus guide_legend instead of _colorbar
    direction = -1, # dark is lowest, yellow is highest
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = FALSE 
    )) + 
  # geom_sf(data = g_def_gemeente_23, fill = NA, color = "grey35", linewidth  = 0.1) + 
  geom_sf(data = g_def_provincie, fill = NA, color = "black", linewidth  = 0.1) + 
  theme_minimal() + 
  theme(
    panel.border = element_blank(), 
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.80)
    # legend.justification = c(0, 1)
    # legend.background = element_rect(fill = "white", color = "grey80")
  )

provinces_coords <- st_centroid(g_def_provincie)
provinces_points <- st_coordinates(provinces_coords)
provinces_points <- data.frame(provinces_points)
provinces_points$names <- c("Groningen", "Friesland", )
ggplot(g_def_provincie) +
  geom_sf(aes(fill = statnaam), color = "white", lwd = 0.3) +
  scale_fill_brewer(palette = "Set3", name = "Province") +  
  theme_minimal() +
  ggtitle("Provinces of the Netherlands") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )



#### for the descriptives 

g_def_wijken_23_descriptive <- merge(g_def_wijken_23, dat_COROP, 
                                     by.x = "GM_CODE_23_x", by.y = "GM_CODE", all.x = TRUE)

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
# ====  Descriptives ============================================================
df_def_wijken_23 <- data.frame(g_def_wijken_23_descriptive)

## Labeling output
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

# --- Using labels for summary table 
# Outout for paper 
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
  summarise(apartment = median(p_h_type_apartment, na.rm = TRUE),
            corner = median(p_h_type_corner_house, na.rm = TRUE),
            detached = median(p_h_type_detached, na.rm = TRUE),
            semi_detached = median(p_h_type_semi_detached, na.rm = TRUE),
            terraced = median(p_h_type_terraced_house_semi_detached, na.rm = TRUE)
  )


province_groningen <- g_def_wijken_23_descriptive %>% filter(REGIO_NAAM == "Groningen")
summary(province_groningen$p_LIHC)

ttt1 <- 
  province_groningen %>% 
  group_by(GM_NAAM_23_x) %>% 
  summarise(av_LIHC = mean(p_LIHC), 
            mean(p_h_year_cons_Before.1924), 
            mean(p_h_year_cons_1925.1947),
            mean(p_h_year_cons_1948.1963),
            mean(p_h_year_cons_1964.1981),
            mean(p_h_year_cons_1982.2002),
  ) %>% arrange(av_LIHC)
View(ttt)

ttt2 <- 
  g_def_wijken_23_descriptive %>% 
  group_by(REGIO_NAAM) %>% 
  summarise(av_LIHC = mean(p_LIHC), 
            mean(p_h_year_cons_Before.1924, na.rm = T), 
            mean(p_h_year_cons_1925.1947, na.rm = T),
            mean(p_h_year_cons_1948.1963, na.rm = T),
            mean(p_h_year_cons_1964.1981, na.rm = T),
            mean(p_h_year_cons_1982.2002, na.rm = T),
            mean(p_homes_without_solar_panels, na.rm = T),
            mean(p_low_house_energy_eff, na.rm = T),
            mean(p_medium_house_energy_eff, na.rm = T),
            mean(p_high_house_energy_eff, na.rm = T),
            mean(p_very_high_house_energy_eff, na.rm = T)
  )
View(ttt2)

av_gm_province_north_holland_solar <- 
  g_def_wijken_23_descriptive %>% filter(REGIO_NAAM == "Noord-Holland") %>% 
  group_by(GM_NAAM_23_x) %>% 
  summarise(ep = mean(p_LIHC), withoutsolar = mean(p_homes_without_solar_panels)) %>% 
  filter(ep > 4.1 & withoutsolar > 85)

district_province_north_holland_solar <- 
  g_def_wijken_23_descriptive %>% filter(REGIO_NAAM == "Noord-Holland") %>% 
  dplyr::select(WK_CODE_23, GM_NAAM_23_x, p_LIHC, p_homes_without_solar_panels) %>% 
  filter(p_LIHC > 4.1 & p_homes_without_solar_panels > 85)

summarise(ep = mean(p_LIHC), withoutsolar = mean(p_homes_without_solar_panels)) %>%
  filter(ep > 4.1 & withoutsolar > 85)