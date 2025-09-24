setwd("C:/Users/tejed002/OneDrive - Wageningen University & Research/PhD-WUR/01_WUR-Research/05_Papers/02_Paper_energy_poverty/analysis/data/wijken/00_intermediate_data")

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


fun_plots <- function(block, dat_wijk, plot_type, mfrow = c(1,1), var_interest){
  if(plot_type == "boxplot"){
    par(mfrow)
    for(ii in block){
      boxplot(dat_wijk[ii], main = ii)
    }
  }
  if(plot_type == "correlation"){
    cor_matrix <- cor(na.omit(dat_wijk[block]))
    round(cor_matrix, 2)
    corrplot::corrplot(cor_matrix, method = "circle")
  }
  if(plot_type == "scatter"){
    dat_wijk = dat_wijk %>%
      filter(variable %in% c(block))
    p1 <- ggplot(dat_wijk, aes(x = .data[["value"]], y = .data[[var_interest]])) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
      facet_wrap(~variable, scales = "free_x") +  # Facet by variable
      labs(
        #title = "Scatterplots of Y vs. X, W, and Z",
        #x = "Independent Variables (X, W, Z)",
        y = var_interest
      ) +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=16,face="bold"),
            strip.text = element_text(size = 20)) 
    return(p1)
  }
}

fun_summary_model <- function(model_inter){
  cat("----- SUMMARY -----------\n")
  print(summary(model_inter))
  cat("----- ANOVA -----------\n")
  print(anova(model_inter))
  cat("----- VIF -----------\n")
  print(vif(model_inter))
}

############### Reading the data ##################################
def_wijken_23 <- read.delim("def_wijken_23.csv", header = T, sep = "|")

dim(def_wijken_23)
head(def_wijken_23)
def_wijken_23["urbanity_degree"] <- ifelse(def_wijken_23[,"STED"] %in% c(1,2), "urban", 
                                           ifelse(def_wijken_23[,"STED"] %in% c(3,4), "semi_urban", 
                                                  ifelse(def_wijken_23[,"STED"] %in% c(5), "no_urban",NA)))
def_wijken_23["urbanity_degree"] <- factor(def_wijken_23[,"urbanity_degree"], 
                                           levels = c("urban", "semi_urban", "no_urban"))
def_wijken_23["geometry"] <- NULL
name_cols <- names(def_wijken_23)
name_cols <- name_cols[!name_cols%in%"geometry"]


g_def_wijken_23 <- st_read("def_wijken_23.shp")

g_def_wijken_23["urbanity_degree"] <- ifelse(g_def_wijken_23$STED %in% c(1,2), "urban", 
                                           ifelse(g_def_wijken_23$STED %in% c(3,4), "semi_urban", 
                                                  ifelse(g_def_wijken_23$STED %in% c(5), "no_urban",NA)))
g_def_wijken_23["urbanity_degree"] <- factor(g_def_wijken_23$urbanity_degree, 
                                           levels = c("urban", "semi_urban", "no_urban"))
name_g_cols <- names(g_def_wijken_23)
name_g_cols <- name_g_cols[!name_g_cols%in%"geometry"]
name_g_cols

rename_columns <- data.frame(cbind(name_g_cols, name_cols))

for(ii in 1:nrow(rename_columns)){
  old <- rename_columns$name_g_cols[ii]
  new <- rename_columns$name_cols[ii]
  colnames(g_def_wijken_23)[colnames(g_def_wijken_23) == old]  <- new
}
  


###### Exploration #######################################################

def_wijken_23 %>% 
  group_by(urbanity_degree) %>%
  summarise(n = n(), 
            na = sum(is.na(GINI)), 
            mean = mean(GINI, na.rm = T), 
            sd = sd(GINI, na.rm = T)
            )

def_wijken_23 %>% 
  group_by(urbanity_degree) %>%
  summarise(across(c(p_individual_CV,p_block_heating,
                   p_district_heat,p_elec_heat ), 
            mean, na.rm = T))

def_wijken_23 %>% 
  group_by(urbanity_degree) %>%
  summarise(across(c(p_individual_CV,p_block_heating,
                     p_district_heat,p_elec_heat ), 
                   mean, na.rm = T))

cols_model =  c("HEQ", "LIHE", "LI", "HE", "standardize_residual_income", "average_energy_ratio", 
                "p_fragile_health_65", 
                "p_without_young_children", "p_with_young_children", 
                "p_pension_coverage_rate", 
                "p_unemployment_rate", 
                "p_precarious_part_time", 
                "p_owner_ocupied_home", 
                "p_rental_home", 
                "t_gas_consum_rental", "t_elec_consum_rental", 
                "t_gas_consum_owner", "t_elec_consum_owner", 
                "t_gas_consum_total", "t_elec_consum_total", 
                "p_individual_CV", "p_block_heating", "p_district_heat", "p_elec_heat", 
                "p_homes_with_solar_panels", 
                "housing_density", 
                "p_rental_own_hous_association", 
                "p_green", "p_gray", 
                #"GINI", 
                "CDD", "HDD", 
                "p_low_house_energy_eff", 
                "p_medium_house_energy_eff", 
                "p_high_house_energy_eff", 
                "p_very_high_house_energy_eff")

cor_matrix <- cor(na.omit(def_wijken_23[cols_model]))
round(cor_matrix, 2)
corrplot::corrplot(cor_matrix, method = "circle")

#### combination variables at the national level
cols_model_nal_01 <- c(
  "p_fragile_health_65", 
  "p_with_young_children", 
  "p_pension_coverage_rate",
  "p_unemployment_rate",
  "p_precarious_part_time", 
  "p_owner_ocupied_home", 
  "p_rental_home", 
  "t_gas_consum_total", "t_elec_consum_total",
  "p_individual_CV", 
  "p_block_heating", 
  "p_district_heat", 
  "p_elec_heat",
  "p_homes_with_solar_panels",
  "p_green", 
  "p_gray",
  "CDD", 
  "HDD",
  "p_low_house_energy_eff",
  "p_medium_house_energy_eff"
  )

select_vars <- c("WK_CODE_23", "LIHE", "HEQ", cols_model_nal_01)
def_wijken_23_long <- def_wijken_23[select_vars] %>%
  pivot_longer(cols = all_of(cols_model_nal_01), 
               names_to = "variable", values_to = "value")
head(def_wijken_23_long)



############### Analysis LIHE ########################################
var_target <- "LIHE"
eq_LIHE_mod_nal_01 <- c(var_target, cols_model_nal_01)

#### 
block_1 = c(var_target, "p_fragile_health_65", "p_with_young_children", "p_pension_coverage_rate",
            "p_unemployment_rate","p_precarious_part_time")
par(mfrow = c(2,3))
for(ii in block_1){
  boxplot(def_wijken_23[ii], main = ii)
}

#### 
block_2 = c(var_target, "p_owner_ocupied_home", "p_rental_home")
par(mfrow = c(1,3))
for(ii in block_2){
  boxplot(def_wijken_23[ii], main = ii)
}

cor_matrix <- cor(na.omit(def_wijken_23[cols_model]))
round(cor_matrix, 2)
corrplot::corrplot(cor_matrix, method = "circle")

#### 
block_3 = c(var_target, "t_gas_consum_total", "t_elec_consum_total")
par(mfrow = c(1,3))
for(ii in block_3){
  boxplot(def_wijken_23[ii], main = ii)
}

#### 
block_4 = c(var_target, "p_individual_CV", "p_block_heating","p_district_heat","p_elec_heat")
par(mfrow = c(3,2))
for(ii in block_4){
  boxplot(def_wijken_23[ii], main = ii)
}

#### 
block_5 = c(var_target,   "p_homes_with_solar_panels",  "p_low_house_energy_eff", "p_medium_house_energy_eff")
par(mfrow = c(2,2))
for(ii in block_5){
  boxplot(def_wijken_23[ii], main = ii)
}

#### 
block_6 = c(var_target, "p_green", "p_gray")
par(mfrow = c(1,3))
for(ii in block_6){
  boxplot(def_wijken_23[ii], main = ii)
}

### 

block_7 = c(var_target, "urbanity_degree")
boxplot(LIHE ~ urbanity_degree, data = def_wijken_23, main = "urbanity_degree")

"../../../output/r/plots"

scatter_b1 <- fun_plots(block_1, def_wijken_23_long, plot_type = "scatter", var_interest = "LIHE")
scatter_b2 <- fun_plots(block_2, def_wijken_23_long, plot_type = "scatter", var_interest = "LIHE")
scatter_b3 <- fun_plots(block_3, def_wijken_23_long, plot_type = "scatter", var_interest = "LIHE")
scatter_b4 <- fun_plots(block_4, def_wijken_23_long, plot_type = "scatter", var_interest = "LIHE")
scatter_b5 <- fun_plots(block_5, def_wijken_23_long, plot_type = "scatter", var_interest = "LIHE")
scatter_b6 <- fun_plots(block_6, def_wijken_23_long, plot_type = "scatter", var_interest = "LIHE")

output_figures_path <- "../../../output/r/plots"
ggsave(plot = scatter_b1, 
       filename = file.path(output_figures_path, "block_1.png"))
ggsave(plot = scatter_b2, 
       filename = file.path(output_figures_path, "block_2.png"))
ggsave(plot = scatter_b3, 
       filename = file.path(output_figures_path, "block_3.png"))
ggsave(plot = scatter_b4, 
       filename = file.path(output_figures_path, "block_4.png"))
ggsave(plot = scatter_b5, 
       filename = file.path(output_figures_path, "block_5.png"))
ggsave(plot = scatter_b6, 
       filename = file.path(output_figures_path, "block_6.png"))
### Exploration models 
def_wijken_23[, "urbanity_degree"] <- relevel(def_wijken_23[, "urbanity_degree"], ref = 'semi_urban')

block <- paste("block_", 1:6, sep = "")
vect_models <- paste("pool_model_", 1:6, sep = "")
list_models <- list()
for(ii in 1:length(vect_models)){
  regressor <- paste(get(block[ii])[-1], collapse = " + ", sep = " ")
  regressor <- paste(regressor, "urbanity_degree", sep = " + ")
  formula_blok <- paste(var_target, "~ ", regressor)
  execute_model <- paste("lm(", formula_blok ,", data = def_wijken_23)")
  model_temp <- eval(parse(text = execute_model))
  list_models[[ii]] <- model_temp
  assign(vect_models[ii], model_temp)
  # model.terms <- names(update(get(vect_models[ii]), method="model.frame"))[-1] # remove response
  # use.terms <- names(update(get(vect_models[ii]), terms, method="model.frame"))[-1] # remove response
  # residualPlots(model_temp) 
  # print(model.terms)
  # print(use.terms)
  # print(as.formula(formula_blok))
  # rm(model_temp, formula_blok)
}

# for(ii in 1:length(vect_models)){
#   formula_blok <- paste("LIHE ~", paste(get(block[ii])[-1], collapse = " + ", sep = " "))
#   model_temp <- lm(as.formula(formula_blok), data = def_wijken_23)
#   assign(vect_models[ii], model_temp)
# }
fun_summary_model(pool_model_1)
residualPlots( model = pool_model_1) 

fun_summary_model(pool_model_2)
residualPlots( model = pool_model_2 ) 

fun_summary_model(pool_model_3)
residualPlots( model = pool_model_3 ) 

fun_summary_model(pool_model_4)
residualPlots( model = pool_model_4 ) 

fun_summary_model(pool_model_5)
residualPlots( model = pool_model_5 ) 

fun_summary_model(pool_model_6)
residualPlots( model = pool_model_6 ) 

fun_summary_model(pool_model_7)
residualPlots( model = pool_model_7) 


cols_full_model <- c(cols_model_nal_01, "urbanity_degree")
formula_blok <- paste(var_target, " ~", paste(cols_full_model, collapse = " + ", sep = " "))
execute_model <- paste("lm(", formula_blok ,", data = def_wijken_23)")
pool_mod_full <- eval(parse(text = execute_model))
summary(pool_mod_full)
anova(pool_mod_full)
vif(pool_mod_full)
residualPlots( model = pool_mod_full ) 

#### adjusting to run with heteroscedasticity 
dim(def_wijken_23)
def_wijken_23_naOmit <- na.omit(def_wijken_23[c(var_target, cols_full_model)])
dim(def_wijken_23_naOmit)

formula_blok <- formula(log(LIHE) ~ 
                          p_fragile_health_65 + p_with_young_children + 
                          p_pension_coverage_rate + p_unemployment_rate + 
                          p_precarious_part_time + p_owner_ocupied_home + 
                          p_rental_home + t_gas_consum_total + t_elec_consum_total + 
                          p_individual_CV + p_block_heating + p_district_heat + 
                          p_elec_heat + p_homes_with_solar_panels + 
                          p_green + p_gray + 
                          CDD + HDD + 
                          p_low_house_energy_eff + p_medium_house_energy_eff + 
                          urbanity_degree)

pool_mod_full <- lm(formula_blok, data = def_wijken_23_naOmit)
summary(pool_mod_full)
anova(pool_mod_full)
vif(pool_mod_full)
residualPlots( model = pool_mod_full, plot=FALSE) 

weights_mod_full <- 1 / pool_mod_full$residuals^2
pool_mod_full_heter <- lm(formula_blok, 
                          data = def_wijken_23_naOmit, 
                          weights = weights_mod_full)
bptest(pool_mod_full_heter)
##### log LIHE 

formula_blok <- paste("log(LIHE) ~", paste(c(cols_model_nal_01, "urbanity_degree"), collapse = " + ", sep = " "))
execute_model <- paste("lm(", formula_blok ,", data = def_wijken_23)")
pool_mod_full <- eval(parse(text = execute_model))
summary(pool_mod_full)
anova(pool_mod_full)
vif(pool_mod_full)
residualPlots( model = pool_mod_full ) 
bptest(pool_mod_full)

dev.off()
di <- cooks.distance(model = pool_mod_full)
plot(di,xlab="Obs", ylab="Cook's distance", pch=16)

boxcox_mod_full <- boxcox(pool_mod_full)
lambda <- boxcox_mod_full$x[which.max(boxcox_mod_full$y)]
lambda

###########################################################################################3
####### reading by using GIS 
crs_info <- st_crs(g_def_wijken_23)
print(crs_info)
crs_info$units

temp_file <- g_def_wijken_23 %>% filter(!is.na(LIHE))
nb <- poly2nb(temp_file, queen=TRUE, snap = 50)
summary(nb)

pts <- st_point_on_surface(st_geometry(temp_file))
plot(nb, pts)

lw <- nb2listw(nb, style="W", zero.policy=TRUE)

temp_file$lag <- lag.listw(lw, temp_file$LIHE)

# Create a regression model
M <- lm(lag ~ LIHE, temp_file)

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

loc_moran <- localmoran(temp_file$LIHE, lw)
temp_file$loc_moran_LIHE <- loc_moran[,1]
temp_file$loc_moran_class_LIHE <- attributes(loc_moran)$quadr$mean


##################################################################
### Local Moran 
temp_file_lihe <- temp_file
local_moral <- localmoran(temp_file_lihe$LIHE, listw = lw, adjust.x=TRUE)
temp_file_lihe <- cbind(temp_file_lihe, local_moral)
colnames(temp_file_lihe)[colnames(temp_file_lihe) == "Pr.z....E.Ii.."] <- 'P_Z_Great_0'

temp_file_lihe$Ii <- temp_file_lihe$Ii - mean(temp_file_lihe$Ii, na.rm = TRUE) 
temp_file_lihe$lag.LIHE<-  lag.listw(lw,temp_file_lihe$LIHE, NAOK = TRUE)

temp_file_lihe$LIHEs <- temp_file_lihe$LIHE - mean(temp_file_lihe$LIHE, na.rm = TRUE) 
temp_file_lihe$lag.LIHE  <- temp_file_lihe$lag.LIHE - mean(temp_file_lihe$lag.LIHE, na.rm = TRUE) 
signif <- 0.05


temp_file_lihe <- temp_file_lihe%>% 
  mutate(quadrant= ifelse(LIHEs>0 & lag.LIHE> 0, 1, 0)) %>% 
  mutate(quadrant= ifelse(LIHEs<0 & lag.LIHE < 0, 2, quadrant)) %>% 
  mutate(quadrant= ifelse(LIHEs<0 & lag.LIHE > 0, 3, quadrant)) %>% 
  mutate(quadrant= ifelse(LIHEs>0 & lag.LIHE < 0, 4, quadrant)) %>%   
  mutate(quadrant= ifelse(P_Z_Great_0 > signif, 0, quadrant)) 


breaks = c(0, 1, 2, 3, 4, 5) 
LISA1<-tm_shape(temp_file_lihe) + 
  tm_fill(col = "quadrant", 
          breaks = breaks, palette=  c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)), labels = c("Not significant", "High-High","Low-Low","Low-High","High-Low"), title="")+
  tm_legend(text.size = 1)  +
  # tm_scale_bar(position = c("LEFT", "BOTTOM"),text.size = 1.0)+
  # tm_compass(type = "8star",   position = c("RIGHT", "BOTTOM"),      show.labels = 2,   text.size = 0.5)+
  tm_borders(alpha=.5) +
  tm_layout( frame = FALSE,  title = "LISA p-values, LIHE, alpha = 0.05")
tmap_save(LISA1, file.path(output_figures_path, "global_local_significant_lihe.png"))

### Local Moran log LIHE 

temp_file_LOGlihe <- temp_file

temp_file_LOGlihe$logLIHE <- log(temp_file_LOGlihe$LIHE)

local_moral <- localmoran(temp_file_LOGlihe$logLIHE, listw = lw, adjust.x=TRUE)
temp_file_LOGlihe <- cbind(temp_file_LOGlihe, local_moral)
colnames(temp_file_LOGlihe)[colnames(temp_file_LOGlihe) == "Pr.z....E.Ii.."] <- 'P_Z_Great_0'

temp_file_LOGlihe$Ii <- temp_file_LOGlihe$Ii - mean(temp_file_LOGlihe$Ii, na.rm = TRUE) 
temp_file_LOGlihe$lag.logLIHE<-  lag.listw(lw,temp_file_LOGlihe$LIHE, NAOK = TRUE)

temp_file_LOGlihe$logLIHEs <- temp_file_LOGlihe$LIHE - mean(temp_file_LOGlihe$logLIHE, na.rm = TRUE) 
temp_file_LOGlihe$lag.logLIHE  <- temp_file_LOGlihe$lag.logLIHE - mean(temp_file_LOGlihe$lag.logLIHE, na.rm = TRUE) 
signif <- 0.05


temp_file_LOGlihe <- temp_file_LOGlihe%>% 
  mutate(quadrant= ifelse(logLIHEs>0 & lag.logLIHE> 0, 1, 0)) %>% 
  mutate(quadrant= ifelse(logLIHEs<0 & lag.logLIHE < 0, 2, quadrant)) %>% 
  mutate(quadrant= ifelse(logLIHEs<0 & lag.logLIHE > 0, 3, quadrant)) %>% 
  mutate(quadrant= ifelse(logLIHEs>0 & lag.logLIHE < 0, 4, quadrant)) %>%   
  mutate(quadrant= ifelse(P_Z_Great_0 > signif, 0, quadrant)) 


breaks = c(0, 1, 2, 3, 4, 5) 
LISA2<-tm_shape(temp_file_LOGlihe) + 
  tm_fill(col = "quadrant", 
          breaks = breaks, 
          palette=  c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)), 
          labels = c("Not significant", "High-High","Low-Low","Low-High","High-Low"), 
          title="")+
  tm_legend(text.size = 1)  +
  # tm_scale_bar(position = c("LEFT", "BOTTOM"),text.size = 1.0)+
  # tm_compass(type = "8star",   position = c("RIGHT", "BOTTOM"),      show.labels = 2,   text.size = 0.5)+
  tm_borders(alpha=.5) +
  tm_layout( frame = FALSE,  title = "LISA p-values, logLIHE, alpha = 0.05")
LISA2

tmap_save(LISA2, file.path(output_figures_path, "global_local_significant_log_lihe.png"))
###### Analysis 


tm_shape(temp_file) +
  tm_dots(col="LIHE", style="quantile", 
          palette="Reds", 
          title = "LIHE", size = 0.5) + 
  tm_layout(legend.outside.size = 1)


tm_shape(temp_file) +
  tm_dots(col="loc_moran_LIHE", style="quantile", 
          palette="Reds", 
          title = "loc_moran_LIHE", size = 0.5) + 
  tm_layout(legend.outside.size = 1)

tm_html_class <- tm_shape(temp_file) +
  tm_fill(col="loc_moran_class_LIHE", midpoint = 0,
          title = "loc_moran_LIHE") + 
  tm_layout(legend.outside.size = 1)

tmap_save(tm_html_class, file.path(output_figures_path, "loc_moran_class_LIHE.html"))
tmap_save(tm_html_class, file.path(output_figures_path, "loc_moran_class_LIHE.png"))
########## Modelling GWR ###############################
temp_file_centroids <- st_centroid(temp_file)
coords<-st_coordinates(temp_file_centroids)
formula_blok <- 
  log(LIHE) ~ p_fragile_health_65 
# + 
#   # p_with_young_children + 
#   p_pension_coverage_rate + p_unemployment_rate + p_precarious_part_time + 
#   p_owner_ocupied_home + p_rental_home + 
#   t_gas_consum_total +  t_elec_consum_total + 
#   # p_individual_CV + p_block_heating +   p_district_heat + p_elec_heat + 
#   p_homes_with_solar_panels + 
#   p_green + p_gray + 
#   CDD + HDD + 
#   p_low_house_energy_eff + p_medium_house_energy_eff + 
#   urbanity_degree
bw <- gwr.sel(formula_blok, data = temp_file_centroids, 
              coords = coords, method = "aic",
              gweight = gwr.Gauss, adapt = TRUE, tol = 0.5)
bw <- 0.2152993

formula_definite <-   LIHE ~ p_fragile_health_65 +
  # p_with_young_children +
  p_pension_coverage_rate + p_unemployment_rate + p_precarious_part_time +
  p_owner_ocupied_home + p_rental_home +
  t_gas_consum_total +  t_elec_consum_total +
  # p_individual_CV + p_block_heating +   p_district_heat + p_elec_heat +
  p_homes_with_solar_panels +
  p_green + p_gray +
  CDD + HDD +
  p_low_house_energy_eff + p_medium_house_energy_eff +
  urbanity_degree

temp_file_nogeom <- st_drop_geometry(temp_file_centroids)  
allvars <- all.vars(formula_definite)
for(ii in 1:length(allvars)){
  if(allvars[ii] != "urbanity_degree"){
    variable <- unlist(temp_file_nogeom[allvars[ii]])
    mean_imput <- mean(variable, na.rm = T)
    print(mean_imput)
    imputation <- ifelse(is.na(variable), imputation, variable)
    temp_file_nogeom[allvars[ii]] <- imputation
  }
}
temp_file_nogeom[, "urbanity_degree"] <- relevel(temp_file_nogeom[, "urbanity_degree"], 
                                                 ref = 'semi_urban')
MOD.GWR <- gwr(formula_definite, 
               data = temp_file_nogeom, 
               coords = coords, adapt = bw, hatmatrix = TRUE)
MOD.GWR
BFC02.gwr.test(MOD.GWR)

res_MOD.GWR <- MOD.GWR


tmap_mode("view")

res_MOD.GWR$SDF <- st_as_sf(res_MOD.GWR$SDF)

st_crs(res_MOD.GWR$SDF) <- 28992

block1_tm <- tm_shape(res_MOD.GWR$SDF) +
  tm_dots(c("p_fragile_health_65", "p_pension_coverage_rate", 
            "p_unemployment_rate", "p_precarious_part_time"), midpoint = 0, style = "quantile") +
  tm_style("col_blind")+
  tm_layout(legend.position = c("right", "top"))


block2_3_tm <- tm_shape(res_MOD.GWR$SDF) +
  tm_dots(c("p_owner_ocupied_home", "p_rental_home", 
            "t_gas_consum_total", "t_elec_consum_total"), midpoint = 0, style = "quantile") +
  tm_style("col_blind")+
  tm_layout(legend.position = c("right", "top"))

block5_6_tm <- tm_shape(res_MOD.GWR$SDF) +
  tm_dots(c("p_homes_with_solar_panels", "p_low_house_energy_eff", 
            "p_medium_house_energy_eff", "p_green", "p_gray"), midpoint = 0, style = "quantile") +
  tm_style("col_blind")+
  tm_layout(legend.position = c("right", "top"))


tmap_save(block1_tm, file.path(output_figures_path, "GWR_block1.html"))
########################################################
summary(pool_mod_2)
anova(pool_mod_2)
print(sort(vif(pool_mod_2)))

summary(pool_mod_3)
anova(pool_mod_3)
print(sort(vif(pool_mod_3)))

summary(pool_mod_1)
anova(pool_mod_1)
print(sort(vif(pool_mod_1)))

robust_se_mod3 <- coeftest(pool_mod3, vcov = vcovHC(pool_mod3, type = "HC0"))
robust_se_mod3

summary(pool_mod1)
anova(pool_mod1)

summary(pool_mod1)
anova(pool_mod1)

summary(pool_mod1)
anova(pool_mod1)

summary(pool_mod1)
anova(pool_mod1)

summary(pool_mod1)
anova(pool_mod1)



pool_mod1 <- lm(LIHE ~ fragile_health_65 + p_young_children + t_pension + t_unemployment + 
                t_precarious_part_time + p_owner_ocupied_home+ p_rental_home+
                t_gas_consum_rental+ t_gas_consum_owner+ t_elec_consum_rental+
                t_elec_consum_owner+ t_gas_consum_total+ t_elec_consum_total+
                p_individual_CV + p_elec_heat+ housing_density+ p_rental_own_hous_association, 
                data = def_wijken_23)
summary(pool_mod1)
anova(pool_mod1)

pool_mod2 <- lm(LIHE ~ fragile_health_65 + p_young_children + t_pension + t_unemployment + 
                  t_precarious_part_time + p_owner_ocupied_home+ p_rental_home+
                  t_gas_consum_rental + t_gas_consum_owner+ t_elec_consum_rental+
                  t_elec_consum_owner + p_individual_CV + p_elec_heat+ housing_density + p_rental_own_hous_association, 
                data = def_wijken_23)
summary(pool_mod2)
anova(pool_mod2)


vif_values <- car::vif(pool_mod2)
print(sort(vif_values))

bptest(pool_mod2) 
robust_se <- coeftest(pool_mod2, vcov = vcovHC(pool_mod2, type = "HC0"))
robust_se


pool_mod3 <- lm(LIHE ~ fragile_health_65 + p_young_children + t_pension + t_unemployment + 
                  t_precarious_part_time + p_owner_ocupied_home+ p_rental_home+
                  t_gas_consum_total+ t_elec_consum_total + 
                  p_individual_CV + p_elec_heat+ housing_density, 
                data = def_wijken_23)
summary(pool_mod3)
anova(pool_mod3)


vif_values_mod3 <- car::vif(pool_mod3)
print(sort(vif_values_mod3))

robust_se_mod3 <- coeftest(pool_mod3, vcov = vcovHC(pool_mod3, type = "HC0"))
robust_se_mod3

#### NA removing 
def_wijken_23_naOmit <- na.omit(def_wijken_23[cols_model])
pool_mod4 <- lm(LIHE ~ fragile_health_65 + p_young_children + t_pension + t_unemployment + 
                  t_precarious_part_time + p_owner_ocupied_home+ p_rental_home+
                  t_gas_consum_total+ t_elec_consum_total + 
                  p_individual_CV + p_elec_heat+ housing_density, 
                data = def_wijken_23_naOmit)

weights_mod4 <- 1 / pool_mod4$residuals^2

# Fit weighted least squares model
pool_mod4_wls <- lm(LIHE ~ fragile_health_65 + p_young_children + t_pension + t_unemployment + 
                      t_precarious_part_time + p_owner_ocupied_home+ p_rental_home+
                      t_gas_consum_total+ t_elec_consum_total + 
                      p_individual_CV + p_elec_heat+ housing_density, 
                    data = def_wijken_23_naOmit, weights = weights_mod4)

summary(pool_mod4_wls)
vif_values_mod4 <- car::vif(pool_mod4_wls)
print(sort(vif_values_mod4))

