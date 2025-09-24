setwd("C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/02_Paper_energy_poverty/Energy poverty Analysis/data/wijken/00_intermediate_data")
source("../../../R/national/02_data_preparation.R")
### Additional packages 
library(gstat)
library(ggiraph)
library(spatialreg)
library(SDPDmod)
library(ggpubr)
library(spmoran)
# inpath_gdb <- "//WURNET.NL/Homes/tejed002/My Documents/ArcGIS/Projects/Wooningtypen_def/Default.gdb"
# st_layers(inpath_gdb)
# 
# dat_woning_type <- st_read(dsn = inpath_gdb, layer = "main_woning_verblijf_for_R2")
# 
# # Set the path to the Geodatabase
# gdb_path <- "C:/GISProject.gdb"
# 
# # List all feature classes in the GDB
# st_layers(gdb_path)
################################################################################################
##### Functions 



#### Global variables

outpath_figure <- "../../../output/R/econometric"


##### Additional variable creation 

# g_def_wijken_23[, "per_dwelling_gas_consum_rental"] <- 
#   g_def_wijken_23$t_gas_consum_rental / (g_def_wijken_23$p_rental_home/100 * g_def_wijken_23$total_residential_units )
# 
# g_def_wijken_23[, "per_dwelling_gas_consum_rental"] <- 
#   ifelse(is.infinite(g_def_wijken_23$per_dwelling_gas_consum_rental), NA, g_def_wijken_23$per_dwelling_gas_consum_rental)
# 
# g_def_wijken_23[, "per_dwelling_elec_consum_rental"] <- 
#   g_def_wijken_23$t_elec_consum_rental / (g_def_wijken_23$p_rental_home/100 * g_def_wijken_23$total_residential_units )
# 
# g_def_wijken_23[, "per_dwelling_elec_consum_rental"] <- 
#   ifelse(is.infinite(g_def_wijken_23$per_dwelling_elec_consum_rental), NA, g_def_wijken_23$per_dwelling_elec_consum_rental)
dim(g_def_wijken_23)
g_def_wijken_23 <- g_def_wijken_23 %>% 
  filter(AANT_INW_23 > 0 &
           (p_owner_ocupied_home > 0 & p_rental_home > 0) & 
           !is.na(LIHE) & WOZ >0 & !is.na(p_low_house_energy_eff)#& 
         #!WK_CODE_23 %in% c("WK055606", "WK085548", "WK027408", "WK098335") ## outliers in the residual analysis
         )
dim(g_def_wijken_23)

per_n_units <- 10000
per_n_years <- 10
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
                                                        p_pop_Origin_Outside_NL_Born_Outside_Europe/p_pop_Origin_NL_Born_NL)


g_def_wijken_23 %>% filter(ratio_rental_owner > 8)
g_def_wijken_23$p_pension_coverage_rate <- g_def_wijken_23$p_pension_coverage_rate/100
g_def_wijken_23$p_unemployment_rate = g_def_wijken_23$p_unemployment_rate/100
g_def_wijken_23$p_precarious_part_time = g_def_wijken_23$p_precarious_part_time/100
###### cleaning and imputation variables to some districts 

main_var_imputation <- c(
                    "p_fragile_health_65", 
                    "p_without_young_children", 
                    "p_with_young_children", 
                    "p_single_person_household",
                    "p_pension_coverage_rate", 
                    "p_unemployment_rate", 
                    "p_precarious_part_time", 
                    "p_disability", 
                    "ratio_rental_owner",
                    "ratio_housingCorp_owner",
                    "p_rental_corporations_home", 
                    "t_av_household_size",
                    "p_households_under_around_social_minimum",
                    # Energy consumption 
                    "p_gas_consum_rental",
                    "p_elec_consum_rental",
                    "t_elec_consum_rental", 
                    "t_gas_consum_rental",
                    # "p_district_heat_high_gas_consum", 
                    # "p_district_heat_low_gas_consum", 
                    # "p_district_heat_no_gas_consum", 
                    "p_elec_heat", 
                    "p_homes_with_solar_panels", 
                    "p_homes_without_solar_panels",
                    "p_green", 
                    "p_gray", 
                    "CDD", 
                    "HDD", 
                    #### housing market variables 
                    "p_h_type_apartment", 
                    "p_h_type_corner_house", 
                    "p_h_type_detached", 
                    "p_h_type_semi_detached", 
                    "p_h_type_terraced_house_semi_detached", 
                    "p_h_year_cons_Before.1947",
                    "p_h_year_cons_1948.1981",
                    "p_h_year_cons_1982.2002",
                    "p_h_year_cons_After.2002",
                    "p_low_house_energy_eff", 
                    "p_medium_house_energy_eff", 
                    "building_median_age",
                    "WOZ",
                    # ethnicity 
                    "p_pop_western_residents",
                    "p_pop_Non_western_residents",
                    "p_pop_Other_Non_Western"
                    # Additional variables 
                    # "GINI", 
                    )

for(var in main_var_imputation){
  temp_df <- data.frame(g_def_wijken_23[, c("GM_CODE_23_x", var)])
  target_var <- temp_df[, var]
  temp_df$geometry <- NULL
  temp_df <- temp_df %>% 
    group_by(GM_CODE_23_x) %>% 
    summarise(aver_GM = mean(.data[[var]], na.rm = T))
  g_def_wijken_23 <- merge(g_def_wijken_23, 
                           temp_df,  by = "GM_CODE_23_x", all.x = TRUE)
  cat(var, ": ", sum(is.na(target_var))/length(target_var), "\n")
  g_def_wijken_23[, var] <- ifelse(is.na(target_var), 
                                   g_def_wijken_23$aver_GM, 
                                   target_var)
  g_def_wijken_23$aver_GM <- NULL
}
######## Centroids and variogram 
g_def_wijken_23$centroids <- st_point_on_surface(g_def_wijken_23$geometry)
g_def_wijken_23$id <- 1:nrow(g_def_wijken_23)
# plot(g_def_wijken_23$centroids)

crs_info <- st_crs(g_def_wijken_23)
print(crs_info)
crs_info$units
## Exploring Summary 

dt_amsterdam <- def_wijken_23 %>% filter(GM_NAAM_23_x == "Amsterdam")
table(dt_amsterdam$wijk_predominant_typology)

dt_urban <- def_wijken_23 %>% filter(urbanity_degree %in% c("urban", "semi_urban"))
table(dt_urban$wijk_predominant_typology)

tab_g_urban_typo <- def_wijken_23 %>% 
  group_by(urbanity_degree, wijk_predominant_typology) %>% 
  summarise(median = median(LIHE, na.rm = TRUE),
            n = n())
tab_g_urban_typo <- tab_g_urban_typo %>% 
  group_by(urbanity_degree) %>%
    mutate(median_perc = median/sum(median)*100)

ttt <- def_wijken_23 %>% 
  group_by(wijk_predominant_typology) %>% 
  summarise(med_LIHE = median(LIHE, na.rm = TRUE),
            med_HEQ = median(HEQ, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(med_LIHE))

q_LIHE <- quantile(na.omit(def_wijken_23[, "LIHE"]))
def_wijken_23["LIHE_Q"] <- 
  cut(def_wijken_23[,"LIHE"], q_LIHE, c("Q1", "Q2", "Q3", "Q4"))

def_wijken_23 %>% 
  group_by(wijk_predominant_typology) %>% 
  summarise(med_LIHE = median(LIHE, na.rm = TRUE),
            med_HEQ = median(HEQ, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(med_LIHE))


def_wijken_23 %>% 
  group_by(LIHE_Q, urbanity_degree) %>% 
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


summary(g_def_wijken_23$LIHE)
summary(log(g_def_wijken_23$LIHE))

png(filename = file.path(outpath_figure, "LIHE_Distribution.png"), width = 600, height = 500)
plot(density(g_def_wijken_23$LIHE), xlab = "LIHE", main = "Distribution LIHE")
dev.off()

png(filename = file.path(outpath_figure, "Log_LIHE_Distribution.png"), width = 600, height = 500)
plot(density(log(g_def_wijken_23$LIHE/100)), xlab = "Log LIHE", main = "Distribution Log LIHE")
dev.off()

### variogram analysis
var_gram <- variogram(log(LIHE) ~ 1, g_def_wijken_23)
plot(var_gram)

var_gram <- variogram(log(LIHE)  ~ p_fragile_health_65 + p_with_young_children + 
                        p_pension_coverage_rate + p_unemployment_rate + 
                        p_precarious_part_time + p_disability + 
                        p_rental_home, g_def_wijken_23)
plot(var_gram)
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
g_def_wijken_23$lag <- lag.listw(lw_queen, g_def_wijken_23$LIHE)
plot(g_def_wijken_23[, c("lag", "LIHE")])
# Create a regression model
M <- lm(lag ~ LIHE, g_def_wijken_23)

# Plot the data
png(filename = file.path(output_figures_path, "moran_I_LIHE.png"))
plot( lag ~ LIHE, temp_file, pch=21, asp=1, las=1, col = "grey40", bg="grey80")
abline(M, col="blue") # Add the regression line from model M
abline(v = mean(temp_file$LIHE), lty=3, col = "grey80")
abline(h = mean(temp_file$LIHE), lty=3, col = "grey80")
title("Moran-I Scatterplot")
dev.off()
moran(temp_file$LIHE, listw = lw, n = length(nb), S0 = Szero(lw))
test_moran <- moran.test(temp_file$LIHE, listw = lw)


####### 
moran.plot(x= temp_file$LIHE, y = temp_file$p_fragile_health_65, listw = lw)

moran.plot(x= temp_file$LIHE, y = temp_file$p_fragile_health_65, listw = lw)

moran.plot(x= temp_file$LIHE, y = temp_file$p_fragile_health_65, listw = lw)

moran.plot(x= temp_file$LIHE, y = temp_file$p_fragile_health_65, listw = lw)


nb2 <- st_contiguity(temp_file$geometry, queen = TRUE, snap = 100)
wt2 <- st_weights(nb2, allow_zero = T)

temp_file_lagged <- temp_file %>% 
  mutate(nb = nb2,
         wts = wt2,
         lag_LIHE = st_lag(LIHE, nb, wts),
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
"p_pop_Origin_NL_Born_Europe",
"p_pop_Origin_Outside_NL_Born_Europe",
"p_pop_Origin_NL_Born_Outside_Europe",
"p_pop_Origin_Outside_NL_Born_Outside_Europe")])

#### to study the quadratic form os some variables
plot(g_def_wijken_23$p_fragile_health_65, log(g_def_wijken_23$LIHE))
plot(g_def_wijken_23$p_with_young_children, log(g_def_wijken_23$LIHE))
plot(g_def_wijken_23$p_single_person_household, log(g_def_wijken_23$LIHE))
plot(g_def_wijken_23$p_pension_coverage_rate, log(g_def_wijken_23$LIHE))
plot(g_def_wijken_23$p_unemployment_rate, log(g_def_wijken_23$LIHE))
plot(g_def_wijken_23$p_precarious_part_time, log(g_def_wijken_23$LIHE))
summary(lm(log(LIHE) ~ p_precarious_part_time, g_def_wijken_23))
abline(a = 7.867e-01, b = 6.298e-04)
mod_t_1 <- lm(log(LIHE) ~ p_precarious_part_time + p_unemployment_rate + p_disability, g_def_wijken_23)
summary(mod_t_1)
plot(g_def_wijken_23$p_disability, log(g_def_wijken_23$LIHE))
plot(g_def_wijken_23$ratio_rental_owner, g_def_wijken_23$LIHE)
plot(log(g_def_wijken_23$p_rental_home), g_def_wijken_23$LIHE)
plot(g_def_wijken_23$p_rental_corporations_home, log(g_def_wijken_23$LIHE))
plot(g_def_wijken_23$p_rental_private_landlord_home, log(g_def_wijken_23$LIHE))
summary(lm(log(LIHE) ~ p_rental_private_landlord_home , g_def_wijken_23))
summary(lm(log(LIHE) ~ p_rental_private_landlord_home + I(p_rental_private_landlord_home**2), g_def_wijken_23))

plot(g_def_wijken_23$p_gas_consum_rental, log(g_def_wijken_23$LIHE))
summary(lm(log(LIHE) ~ p_gas_consum_rental, g_def_wijken_23 %>% filter(p_gas_consum_rental > 0)))
abline(a =  -0.145266, b = 0.033049)

plot(g_def_wijken_23$p_elec_consum_rental, log(g_def_wijken_23$LIHE))
summary(lm(log(LIHE) ~ p_elec_consum_rental, g_def_wijken_23))

plot(g_def_wijken_23$p_elec_consum_rental, g_def_wijken_23$p_gas_consum_rental)

plot(g_def_wijken_23$p_elec_heat_high_gas_consum, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_homes_with_solar_panels, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_green, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_gray, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$CDD, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$HDD, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_h_type_apartment, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_h_type_detached, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_h_type_semi_detached, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_h_type_terraced_house_semi_detached, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_h_year_cons_1948.1981, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_h_year_cons_1982.2002, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_h_year_cons_After.2002, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_low_house_energy_eff, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_medium_house_energy_eff, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_high_house_energy_eff, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$WOZ, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_pop_Origin_Outside_NL_Born_Outside_Europe, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_education_Low, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$p_education_High, log(g_def_wijken_23$LIHE))

plot(g_def_wijken_23$urbanity_degree, log(g_def_wijken_23$LIHE))
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
  # # "p_pop_Origin_NL_Born_Europe",
  "p_pop_Origin_Outside_NL_Born_Europe",
  # # "p_pop_Origin_NL_Born_Outside_Europe",
  "p_pop_Origin_Outside_NL_Born_Outside_Europe",
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
  # # "p_pop_Origin_NL_Born_Europe",
  # "p_pop_Origin_Outside_NL_Born_Europe",
  # # # "p_pop_Origin_NL_Born_Outside_Europe",
  # "p_pop_Origin_Outside_NL_Born_Outside_Europe",
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
nrow(g_def_wijken_23)
g_def_wijken_23 <- g_def_wijken_23 %>% filter(!is.na(p_low_house_energy_eff))
nrow(g_def_wijken_23)

g_def_wijken_23$urbanity_degree <- relevel(g_def_wijken_23$urbanity_degree, ref = "semi_urban")
g_def_wijken_23$STED <- as.factor(as.character(g_def_wijken_23$STED))
g_def_wijken_23$STED <- relevel(g_def_wijken_23$STED, ref = "5")
g_def_wijken_23$wijk_predominant_typology <- factor(g_def_wijken_23$wijk_predominant_typology)

save(g_def_wijken_23, file = file.path(outpath_figure, "Rdata", "g_def_wijken_23.Rdata"))


###########################################################################################################
######## Back up programming ##############################################################################
###########################################################################################################


mod_SDM_dist3000_full_nl_MI  <- spatialreg:::lagsarlm(formula_blok_MI,data = g_def_wijken_23, 
                                                      listw = lw_distance,
                                                      Durbin = TRUE , zero.policy = TRUE)

mod_SDEM_dist3000_full_nl_MI  <- spatialreg:::errorsarlm(formula_blok_MI, data = g_def_wijken_23, 
                                                         lw_distance, 
                                                         Durbin = TRUE , zero.policy = TRUE)

data.frame("AIC" = 
             c(AIC(mod_SDM_Queen_full_nl_MI), AIC(mod_SDEM_Queen_full_nl_MI),
               AIC(mod_SDM_dist3000_full_nl_MI), AIC(mod_SDEM_dist3000_full_nl_MI)),
           "BIC" = 
             c(BIC(mod_SDM_Queen_full_nl_MI), BIC(mod_SDEM_Queen_full_nl_MI),
               BIC(mod_SDM_dist3000_full_nl_MI), BIC(mod_SDEM_dist3000_full_nl_MI)))
qqPlot(mod_SDM_Queen_full_nl_MI$residuals)
qqPlot(mod_SDM_dist3000_full_nl_MI$residuals)
qqPlot(mod_SDM_Queen_full_nl$residuals)
qqPlot(mod_SDM_dist3000_full_nl$residuals)



png(filename = file.path(outpath_figure, "Residuals_SDM_Log_LIHE.png"), width = 600, height = 500)
plot(mod_SDM_Queen_full_nl$residuals, main = "Log LIHE")
dev.off()

png(filename = file.path(outpath_figure, "Residuals_SDM_LIHE.png"), width = 600, height = 500)
plot(mod_SDM_Queen_full_nl_MI$residuals, main = "LIHE")
dev.off()


png(filename = file.path(outpath_figure, "QQPLOT_Residuals_SDM_Log_LIHE.png"), width = 600, height = 500)
qqPlot(mod_SDM_Queen_full_nl$residuals, main = "Log LIHE")
dev.off()

png(filename = file.path(outpath_figure, "QQPLOT_Residuals_SDM_LIHE.png"), width = 600, height = 500)
qqPlot(mod_SDM_Queen_full_nl_MI$residuals, main = "LIHE")
dev.off()


###########################################################################################
###########################################################################################3
######### Approach types: Anselin: Specific to General 

mod_pool_full_nl <- lm(formula_blok, 
                       data = g_def_wijken_23)
summary(mod_pool_full_nl)
vif(mod_pool_full_nl)

temp_data <- data.frame(g_def_wijken_23[c("LIHE", cols_model_nal_01)])
temp_data$geometry <- NULL
forward_model <- lm(log(LIHE) ~ ., data = na.omit(temp_data))
forward_model <- step(forward_model)

anova(mod_pool_full_nl)
car::Anova(mod_pool_full_nl, type = 3)
boxcox(as.formula(formula_blok), data = data.frame(g_def_wijken_23))

qqPlot(mod_pool_full_nl$residuals)
ks.test(mod_pool_full_nl$residuals, "pnorm")
plot(density((mod_pool_full_nl$residuals)))

col.moran <- lm.morantest(mod_pool_full_nl, lw)
col.moran

spdep::lm.RStests(mod_pool_full_nl, lw_queen, test=c("adjRSerr", "adjRSlag"))


formula_urban_blok <- paste(var_target, " ~", paste(cols_model_nal_01[!cols_model_nal_01%in%"urbanity_degree"], collapse = " + ", sep = " "))
mod_pool_full_urban <- lm(formula_urban_blok, 
                       data = g_def_wijken_23 %>% filter(urbanity_degree == "urban"))
summary(mod_pool_full_urban)
data.frame(vif(mod_pool_full_urban))
qqPlot(mod_pool_full_urban$residuals)

screenreg(list("OLS_NL" = mod_pool_full_nl, 
               "OLS_Urban" = mod_pool_full_urban),
          digits = 5
) 
#### the results suggest that autocorrelation parameter rho = 0, 
#### but exist Lambda diff 0. So a Spatial Error Model is more suitable 

#### Running a Spatial Error Model (SEM)
# boxplot(g_def_wijken_23[, c(  "p_h_type_apartment", 
#                       "p_h_type_corner_house", 
#                       "p_h_type_detached", 
#                       "p_h_type_semi_detached", 
#                       "urbanity_degree")]
# 
# boxplot(apartment ~ urbanity_degree, g_def_wijken_23) #### there are a lot of correlation between apartment share and urbanization 
# think about that because what needs to be interpreted should come from there 


mod_SEM_full_nl  <- errorsarlm(formula_blok,data = g_def_wijken_23, listw = lw_queen, tol.solve=1.0e-30, zero.policy=TRUE)
summary(mod_SEM_full_nl, Hausman=TRUE)
qqPlot(mod_SEM_full_nl$residuals)

mod_LAG_full_nl <- lagsarlm(formula_blok, data = g_def_wijken_23, listw = lw_queen, zero.policy=TRUE) 
summary(mod_LAG_full_nl, signif.stars = T)
qqPlot(mod_LAG_full_nl$residuals)

screenreg(list("OLS" = mod_pool_full_nl, 
               "SAR" = mod_LAG_full_nl,
               "SEM" = mod_SEM_full_nl),
          digits = 5
          ) 
htmlreg(list("OLS" = mod_pool_full_nl, 
            "SAR" = mod_LAG_full_nl,
            "SEM" = mod_SEM_full_nl),
       digits = 5,
       override.se = 0, file = file.path(outpath_figure, "OLS_SAR_SEM.html"))


lrtest(mod_pool_full_nl, mod_LAG_full_nl)
lrtest(mod_pool_full_nl, mod_SEM_full_nl)
W <- as(lw, "CsparseMatrix")
set.seed(500L)
trMC <- trW(W, type="MC", p = 20) #default p = 16, 
impacts_LAG_full_nl <- spatialreg:::impacts(mod_LAG_full_nl, tr = trMC, R=200, Q=5)
summary_LAG_nl <- summary(impacts_LAG_full_nl, zstats=TRUE, short=TRUE)


###########################################################################################
###########################################################################################3
######### Approach types: General to specific : Spatial Durbin to SAR, SLX, SEM 

mod_SDM_Queen_full_nl  <- spatialreg:::lagsarlm(formula_blok,data = g_def_wijken_23, lw_queen, 
                                          Durbin = TRUE , zero.policy = TRUE)
mod_SDEM_Queen_full_nl  <- spatialreg:::errorsarlm(formula_blok, data = g_def_wijken_23, lw_queen, 
                                                Durbin = TRUE , zero.policy = TRUE)

summary(mod_SDM_Queen_full_nl)
qqPlot(mod_SDM_Queen_full_nl$residuals)

W <- as(lw_queen, "CsparseMatrix")
set.seed(500L)
trMC <- trW(W, type="MC", p = 20) #default p = 16, 
impacts_SDM_full_nl <- spatialreg:::impacts(mod_SDM_Queen_full_nl, tr = trMC, R=200, Q=5)
summary_SDM_nl <- summary(impacts_SDM_full_nl, zstats=TRUE, short=TRUE)
summary_SDM_nl


mod_SLX_full_nl  <- spatialreg:::lmSLX(formula_blok,data = g_def_wijken_23, lw_queen, zero.policy = TRUE)
summary(mod_SLX_full_nl, signif.stars = T, correlation = T)
qqPlot(mod_SLX_full_nl$residuals)
coef(summary(mod_SLX_full_nl))

htmlreg(list("SEM" = mod_SEM_full_nl,
             "SLX" = mod_SLX_full_nl),
        digits = 5,
        override.se = 0, file = file.path(outpath_figure, "SEM_SLX.html"))

screenreg(list("SEM" = mod_SEM_full_nl,
               "SLX" = mod_SLX_full_nl,
               "SDM" = mod_SDM_full_nl),
        digits = 5)

htmlreg(list("SEM" = mod_SEM_full_nl,
               "SLX" = mod_SLX_full_nl,
               "SDM" = mod_SDM_full_nl),
          digits = 5,
        file = file.path(outpath_figure, "SEM_SLX_SDM.html"))


BIC(mod_SDM_Queen_full_nl)
BIC(mod_SDEM_Queen_full_nl)
BIC(mod_SLX_full_nl)
BIC(mod_LAG_full_nl)
BIC(mod_SEM_full_nl)
BIC(mod_pool_full_nl)


AIC(mod_SDM_Queen_full_nl)
AIC(mod_SDEM_Queen_full_nl)
AIC(mod_SLX_full_nl)
AIC(mod_LAG_full_nl)
AIC(mod_SEM_full_nl)
AIC(mod_pool_full_nl)

lrtest(mod_SDM_Queen_full_nl, mod_SEM_full_nl)

lrtest(mod_SDEM_Queen_full_nl, mod_SLX_full_nl)
lrtest(mod_SDEM_Queen_full_nl, mod_SEM_full_nl)



################################################################################################
################################################################################################
##############3 Running a definite model based on distance = 3000m ################
################################################################################################
################################################################################################


#################################################################################
#################################################################################
#################################################################################
# # 
#################################################################################
#################################################################################
#################################################################################
formula_local <- formula(formula_local)
formula_blok
formula_blok_MI


mod_SDM_dist3000_local_nl_Log  <- spatialreg:::lagsarlm(formula_blok, data = g_def_wijken_23, 
                                                        listw = lw_distance,
                                                       Durbin = ~ p_unemployment_rate + p_rental_corporations_home + p_rental_private_landlord_home + 
                                                         p_gas_consum_rental + p_elec_consum_rental + p_individual_CV + 
                                                         p_elec_heat + p_homes_with_solar_panels + p_h_type_corner_house + 
                                                         p_h_type_detached + p_h_type_semi_detached + p_h_type_terraced_house_semi_detached + 
                                                         p_h_year_cons_1948.1981 + p_h_year_cons_1982.2002 + p_h_year_cons_After.2002 + 
                                                         p_low_house_energy_eff + p_medium_house_energy_eff + WOZ + p_pop_Origin_Outside_NL_Born_Outside_Europe + 
                                                         p_education_Low + p_education_Secondary + urbanity_degree  , zero.policy = TRUE)

colSums(is.na(data.frame(g_def_wijken_23[, cols_model_nal_01] )))

mod_SDM_dist3000_local_nl_MI  <- spatialreg:::lagsarlm(formula_blok_MI, data = g_def_wijken_23, 
                                                       listw = lw_distance,
                                                      Durbin = ~p_unemployment_rate + p_rental_corporations_home + p_rental_private_landlord_home + 
                                                        p_gas_consum_rental + p_elec_consum_rental + p_individual_CV + 
                                                        p_elec_heat + p_homes_with_solar_panels + p_h_type_corner_house + 
                                                        p_h_type_detached + p_h_type_semi_detached + p_h_type_terraced_house_semi_detached + 
                                                        p_h_year_cons_1948.1981 + p_h_year_cons_1982.2002 + p_h_year_cons_After.2002 + 
                                                        p_low_house_energy_eff + p_medium_house_energy_eff + WOZ + p_pop_Origin_Outside_NL_Born_Outside_Europe + 
                                                        p_education_Low + p_education_Secondary + urbanity_degree  , zero.policy = TRUE)


summary(mod_SDM_dist3000_local_nl_Log)

summary(mod_SDM_dist3000_local_nl_MI)


W_distance <- as(lw_distance, "CsparseMatrix")
set.seed(500L)
trMC_distance <- trW(W_distance, type="MC", p = 20) #default p = 16, 

impacts_SDM_distance_local_nl_Log <- spatialreg:::impacts(mod_SDM_dist3000_local_nl_Log, tr = trMC_distance, R=200, Q=5)
impacts_SDM_distance_local_nl_Log <- summary(impacts_SDM_distance_local_nl_Log, zstats=TRUE, short=TRUE)
impacts_SDM_distance_local_nl_Log

impacts_SDM_distance_local_nl_MI <- spatialreg:::impacts(mod_SDM_dist3000_local_nl_MI, tr = trMC_distance, R=200, Q=5)
impacts_SDM_distance_local_nl_MI <- summary(impacts_SDM_distance_local_nl_MI, zstats=TRUE, short=TRUE)
impacts_SDM_distance_local_nl_MI

############################################

mod_SDM_Queen_local_nl_Log  <- spatialreg:::lagsarlm(formula_blok, data = g_def_wijken_23, 
                                                        listw = lw_queen,
                                                        Durbin = ~ p_unemployment_rate + p_rental_corporations_home + p_rental_private_landlord_home + 
                                                          p_gas_consum_rental + p_elec_consum_rental + p_individual_CV + 
                                                          p_elec_heat + p_homes_with_solar_panels + p_h_type_corner_house + 
                                                          p_h_type_detached + p_h_type_semi_detached + p_h_type_terraced_house_semi_detached + 
                                                          p_h_year_cons_1948.1981 + p_h_year_cons_1982.2002 + p_h_year_cons_After.2002 + 
                                                          p_low_house_energy_eff + p_medium_house_energy_eff + WOZ + p_pop_Origin_Outside_NL_Born_Outside_Europe + 
                                                          p_education_Low + p_education_Secondary + urbanity_degree  , zero.policy = TRUE)

colSums(is.na(data.frame(g_def_wijken_23[, cols_model_nal_01] )))

mod_SDM_Queen_local_nl_MI  <- spatialreg:::lagsarlm(formula_blok_MI, data = g_def_wijken_23, 
                                                       listw = lw_queen,
                                                       Durbin = ~p_unemployment_rate + p_rental_corporations_home + p_rental_private_landlord_home + 
                                                         p_gas_consum_rental + p_elec_consum_rental + p_individual_CV + 
                                                         p_elec_heat + p_homes_with_solar_panels + p_h_type_corner_house + 
                                                         p_h_type_detached + p_h_type_semi_detached + p_h_type_terraced_house_semi_detached + 
                                                         p_h_year_cons_1948.1981 + p_h_year_cons_1982.2002 + p_h_year_cons_After.2002 + 
                                                         p_low_house_energy_eff + p_medium_house_energy_eff + WOZ + p_pop_Origin_Outside_NL_Born_Outside_Europe + 
                                                         p_education_Low + p_education_Secondary + urbanity_degree  , zero.policy = TRUE)


summary(mod_SDM_Queen_local_nl_Log)

summary(mod_SDM_Queen_local_nl_MI)


W <- as(lw_queen, "CsparseMatrix")
set.seed(500L)
trMC <- trW(W, type="MC", p = 20) #default p = 16, 

impacts_SDM_Queen_local_nl_Log <- spatialreg:::impacts(mod_SDM_Queen_local_nl_Log, tr = trMC_distance, R=200, Q=5)
impacts_SDM_Queen_local_nl_Log <- summary(impacts_SDM_Queen_local_nl_Log, zstats=TRUE, short=TRUE)
impacts_SDM_Queen_local_nl_Log

impacts_SDM_Queen_local_nl_MI <- spatialreg:::impacts(mod_SDM_Queen_local_nl_MI, tr = trMC_distance, R=200, Q=5)
impacts_SDM_Queen_local_nl_MI <- summary(impacts_SDM_Queen_local_nl_MI, zstats=TRUE, short=TRUE)
impacts_SDM_Queen_local_nl_MI