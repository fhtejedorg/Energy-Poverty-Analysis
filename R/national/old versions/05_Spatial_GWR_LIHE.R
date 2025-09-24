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
library(leaflet)
library(ggplot2)
library(sp)
library(tmap)
library(tmaptools)
library(splm)
library(data.table)
library(sphet)
library(spgwr)
library(ade4)
library(rgeoda)
library(stargazer)
library(texreg)
library(gstat)
library(ggiraph)
library(SDPDmod)
library(ggpubr)
library(spmoran)
library(GWmodel)
library(viridis)
library(tseries) # for Jarque Bera test
library(xtable) ## Latex export

#### Global variables
## Palette in 8 colors 
pallete_viridis_hex <- c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154")
pallete_magma_hex <- c("#fcfdbf", "#fe9f6d", "#de4968", "#8c2981", "#3b0f70")
pallete_magma_hex <- c("#fcfdbf", "#fe9f6d", "#de4968", "#8c2981", "#3b0f70")
palette_custom <- c(
  "#31688e",  # deep green (negative extreme)
  "#7aae88",  # mid green
  "#c3d5b4",  # pale green
  "#ffffee",  # neutral / midpoint (~0)
  "#ffffff",  # White for neutral
  "#e9ca9c",  # light orange
  "#e18b63",  # mid orange
  "#b63138"   # red (positive extreme)
)

input_data <- "output/R/data/03_Spatial Data Processing/"
outpath_figure <- "output/R/econometric/final_results/"
load(file = file.path(input_data, "g_def_wijken_23.Rdata"))
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
g_def_gemeente_23 <- st_read("data/wijken/00_intermediate_data/gdf_gemeente_2023.shp")
g_def_gemeente_23 <- g_def_gemeente_23 %>% dplyr:::select(GM_CODE, GM_NAAM, STED)

cols_model_nal_01 <- c(
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
  
  ################ Built-infrastructure ######################################
  "p_green", 
  "p_gray", 
  
  ################ Temperature ######################################
  "CDD", 
  "HDD"
)

lengend_name <- c(
  "Frail older adults", 
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
  "Urbanity: Urban",
  "Urbanity: No-Urban",
  
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

#################################################################
### Other variables 
#################################################################

  # "p_rental_unknown_home", # Reference 
  # "t_av_household_size",
  # "p_households_under_around_social_minimum",
  # Energy consumption 
  # "p_district_heat_high_gas_consum", 
  # "p_district_heat_low_gas_consum", 
  # "p_district_heat_no_gas_consum", 
  #### housing market variables 
  # "building_median_age",
  # ethnicity 
  # "ratio_ethnicity_NL_Outside",
  # 
  # "p_pop_western_residents",
  # "p_pop_Non_western_residents",
  # "p_pop_Other_Non_Western", Reference 
  
  # Additional variables 
  # "p_rental_own_hous_association",
  # "GINI", 
#################################################################
#################################################################
crs_info <- st_crs(g_def_wijken_23)
print(crs_info)
crs_info$units

summary(g_def_wijken_23[cols_model_nal_01])
anyNA(g_def_wijken_23[cols_model_nal_01])

var_target <- "log(p_LIHC)"
formula_block <- paste(var_target, " ~", paste(cols_model_nal_01, collapse = " + ", sep = " "))
formula_block <- formula(formula_block)

centroids <- g_def_wijken_23$centroids
coords_centroids <-st_coordinates(centroids)
head(coords_centroids)

### I had to discard for the estimation individual_cv and elec_heat due to NA's 
bw <- gwr.sel(formula_block, data = g_def_wijken_23, 
              coords = coords_centroids, method = "aic",
              gweight = gwr.Gauss, adapt = T, tol = 0.01)
# Bandwidth: 0.381966 AIC: 136.8781 
# Bandwidth: 0.618034 AIC: 184.1882 
# Bandwidth: 0.236068 AIC: 78.51529 
# Bandwidth: 0.145898 AIC: 2.631153 
# Bandwidth: 0.09016994 AIC: -62.98515 
# Bandwidth: 0.05572809 AIC: -86.14043 
# Bandwidth: 0.01294407 AIC: 1040.663 
# Bandwidth: 0.03938605 AIC: -53.63163 
# Bandwidth: 0.06653514 AIC: -84.55766 
# Bandwidth: 0.05906142 AIC: -87.05177 
# Bandwidth: 0.06239476 AIC: -86.94194 
# Bandwidth: 0.05906142 AIC: -87.05177 

round(bw * nrow(g_def_wijken_23)) # 136 neighbhours 
df_def_wijken_23 <- st_drop_geometry(g_def_wijken_23) 
df_def_wijken_23 <- na.omit(df_def_wijken_23[, all.vars(formula_block)])
dim(df_def_wijken_23)

gwr_model_vspgwr  <- gwr(formula_block,
                 data = df_def_wijken_23,
                 coords = coords_centroids,
                 gweight = gwr.Gauss,
                 adapt = bw)
gwr_model_vspgwr
BFC02.gwr.test(gwr_model_vspgwr)

gwr_model$SDF

# sp_g_def_wijken_23 <- st_drop_geometry(g_def_wijken_23) 
# sp_g_def_wijken_23 <- rename(sp_g_def_wijken_23, geometry = centroids)

######### final Model GWR -- suing function modified to improve processing speed #######
sp_g_def_wijken_23 <- as(g_def_wijken_23, "Spatial") 
head(sp_g_def_wijken_23)
bw_GWmodel <- bw.gwr(formula_block, data = g_def_wijken_23, 
                     approach = "AIC", kernel = "gaussian",
                     adaptive = TRUE)
bw_GWmodel


# Take a cup of tea and have a break, it will take a few minutes.
# -----A kind suggestion from GWmodel development group
# Adaptive bandwidth (number of nearest neighbours): 1426 AICc value: 184.7398 
# Adaptive bandwidth (number of nearest neighbours): 889 AICc value: 138.199 
# Adaptive bandwidth (number of nearest neighbours): 557 AICc value: 82.39141 
# Adaptive bandwidth (number of nearest neighbours): 351 AICc value: 9.732982 
# Adaptive bandwidth (number of nearest neighbours): 225 AICc value: -53.312 
# Adaptive bandwidth (number of nearest neighbours): 145 AICc value: -85.82353 
# Adaptive bandwidth (number of nearest neighbours): 98 AICc value: -65.45341 
# Adaptive bandwidth (number of nearest neighbours): 176 AICc value: -79.24804 
# Adaptive bandwidth (number of nearest neighbours): 127 AICc value: -84.62275 
# Adaptive bandwidth (number of nearest neighbours): 157 AICc value: -84.20527 
# Adaptive bandwidth (number of nearest neighbours): 138 AICc value: -86.11721 
# Adaptive bandwidth (number of nearest neighbours): 133 AICc value: -86.80104 
# Adaptive bandwidth (number of nearest neighbours): 130 AICc value: -85.4543 
# Adaptive bandwidth (number of nearest neighbours): 134 AICc value: -86.25559 
# Adaptive bandwidth (number of nearest neighbours): 131 AICc value: -85.94368 
# Adaptive bandwidth (number of nearest neighbours): 133 AICc value: -86.80104

gwr_model_vGWmodel <- gwr.basic2(formula_block, data = sp_g_def_wijken_23, 
                                bw = bw_GWmodel, kernel = "gaussian", adaptive = TRUE, 
                                F123.test = FALSE) # Switch to TRUE to test it 
save(gwr_model_vGWmodel, file = "gwr_model_vGWmodel.Rdata")

#### Small validation 
png(file.path(outpath_figure, "GWR_maps","Robustness_check", "QQPLOT_GWR.png"), 
    res = 1200,, width = 8, height = 8.5, units = "in")
qqPlot(gwr_model_vGWmodel$SDF$residual, ylab = "Residuals GWR log(LIHC)")
dev.off()
png(file.path(outpath_figure, "GWR_maps","Robustness_check", "Residuals_map.png"), 
    res = 1200,, width = 8, height = 8.5, units = "in")
spplot(gwr_model_vGWmodel$SDF, "residual",main = "Residuals GWR model - log(LIHC)")
dev.off()

coefficients <-  names(gwr_model_vGWmodel$lm$coefficients)[-1]
cols_format_map <- data.frame(coefficient = coefficients)
proportion <- cols_format_map[, "coefficient"]
proportion[grep("p_", proportion)] <- "proportion"
proportion <- ifelse(proportion != "proportion", "numeric", proportion)
cols_format_map$format <- proportion
cols_format_map$legend <- lengend_name 

variables <- names(gwr_model_vGWmodel$lm$coefficients)
summary_GWR <- apply(gwr_model_vGWmodel$SDF@data, 2, function(x) {(exp(x)-1)*100})
summary_GWR <- data.frame(summary_GWR)
summary_GWR <- summary_GWR %>% rename(Intercept = (Intercept))
summary_GWR <- summary_GWR %>% select_at(c("Intercept", coefficients))
summary_GWR <- summary_GWR %>% pivot_longer(cols = "Intercept":"HDD")
summary_GWR <- summary_GWR %>% 
  group_by(name) %>% 
  summarise(min = round(min(value), 2), 
            mean = round(mean(value), 2), 
            median = round(median(value), 2), 
            sd = round(sd(value), 2), 
            max =round(max(value), 2))
summary_GWR <- merge(summary_GWR, cols_format_map, by.x = "name", by.y = "coefficient", all.x = TRUE)
summary_GWR <- summary_GWR[match(cols_format_map$coefficient, summary_GWR$name), ]
summary_GWR <- summary_GWR %>% dplyr:::select(legend, min, mean, median, sd, max)
print(xtable(summary_GWR), include.rownames = FALSE)
# jarque.bera.test(gwr_model_vGWmodel$SDF$residual)


p_valuesGWR <- gwr.t.adjust2(gwr_model_vGWmodel)
p_valuesGWR <- p_valuesGWR$results$p[,-1]
gwr_estimates <- gwr_model_vGWmodel$SDF@data
gwr_estimates <- gwr_estimates %>% select_at(coefficients)
gwr_pvalue <- NULL

for(ii in 1:length(coefficients)){
  var <- coefficients[ii]
  estim <- gwr_estimates[, var]
  col_pval <- grep(var, colnames(p_valuesGWR))
  pval  <- p_valuesGWR[, col_pval]
  signif <- ifelse(pval < 0.05, estim, NA)
  signif <- (exp(signif) - 1)*100
  gwr_pvalue <- cbind(gwr_pvalue, signif)
}

colnames(gwr_pvalue) <- coefficients
gwr_significant <- g_def_wijken_23 %>% 
  dplyr:::select(GM_CODE_23_x, WK_CODE_23, GM_NAAM_23_x, WK_NAAM_23_x, p_LIHC)
gwr_significant <- cbind(gwr_significant, gwr_pvalue)

no_classes <- 4

gwr_significant_quantile <- gwr_significant
if_negative_positive <- NULL
for(ii in 1:length(coefficients)){
  col_inter <- coefficients[ii]
  format <- cols_format_map %>% filter(coefficient == col_inter) %>% dplyr:::select(format) %>% unlist()
  #if(format == "proportion"){
  cat("doing: ", col_inter)
  quantiles <- gwr_significant_quantile %>%
    pull(col_inter) %>%
    quantile(probs = seq(0, 1, length.out = no_classes + 1), na.rm = T) %>%
    as.vector() # to remove names of quantiles, so idx below is numeric
  quantiles <- sort(c(0, quantiles))
  
  # if(any(quantiles<0))
  labels <- imap_chr(quantiles, 
                    function(., idx){
                      x <- paste(round(quantiles[idx], 2), "%", "  -  ", round(quantiles[idx+1], 2), "%", sep  ="")  
                      x
                      })
  labels <- labels[1:(length(quantiles)-1)]
  print(labels)
  gwr_significant_quantile <- gwr_significant_quantile %>% 
    mutate(!!col_inter := cut(.data[[col_inter]], breaks = quantiles, labels = labels, include.lowest = T))
  rm(col_inter)
  #}
}

for (ii in 1:length(coefficients)){
  col_inter <- coefficients[ii]
  print(col_inter)
  legend_name <- cols_format_map %>% filter(coefficient == col_inter) %>% dplyr:::select(legend) %>% unlist()
  format <- cols_format_map %>% filter(coefficient == col_inter) %>% dplyr:::select(format) %>% unlist()
  # if(format == "proportion"){
  pp <- ggplot() + 
    geom_sf(data = gwr_significant_quantile, aes(fill = .data[[col_inter]]), 
            linewidth  = 0.05, color = "transparent") + 
    scale_fill_viridis(
      option = "magma",
      name = legend_name,
      na.value = "grey90",
      na.translate = FALSE,
      # na.name = "Non sig.",
      alpha = 0.8, # make fill a bit brighter
      begin = 0.3, 
      end = 0.9,
      discrete = T, # discrete classes, thus guide_legend instead of _colorbar
      direction = -1, # dark is lowest, yellow is highest
      guide = guide_legend(
        keyheight = unit(5, units = "mm"),
        title.position = "top",
        reverse = FALSE 
      )) + 
    geom_sf(data = g_def_gemeente_23, fill = NA, color = "grey35", linewidth  = 0.1) + 
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
          legend.position.inside = c(0.22, 0.80)
          # legend.justification = c(0, 1)
          # legend.background = element_rect(fill = "white", color = "grey80")
          )
  ggsave(plot = pp, filename = file.path(outpath_figure, "GWR_maps", 
                                         paste(col_inter, ".png", sep = "")),
         dpi=300, width = 8, height = 8.5, units = "in")
  # }
}


############3 Plots using gradient for continuos, scaled to showe in percentages 

gwr_significant_gradient <- gwr_significant
for (ii in 1:length(coefficients)){
  col_inter <- coefficients[ii]
  print(col_inter)
  legend_name <- cols_format_map %>% filter(coefficient == col_inter) %>% dplyr:::select(legend) %>% unlist()
  format <- cols_format_map %>% filter(coefficient == col_inter) %>% dplyr:::select(format) %>% unlist()
  mid_val <- 0
  gwr_significant_gradient[[col_inter]] <- gwr_significant_gradient[[col_inter]]/100
  vals <- gwr_significant_gradient[[col_inter]]
  range_vals <- range(vals, na.rm = TRUE)
  pp <- ggplot() + 
    geom_sf(data = gwr_significant_gradient, aes(fill = .data[[col_inter]]),
            linewidth = 0.05, color = "transparent") + 
    geom_sf(data = g_def_gemeente_23, fill = NA, color = "grey35", linewidth  = 0.1) + 
    scale_fill_gradient2(
      low = "#31688e",
      high = "#b63138",
      mid = "#ffffff",
      name = legend_name,
      midpoint = mid_val,
      na.value = "#ffffff",
      limits  = range_vals,
      labels = scales::label_percent(accuracy = 0.1, trim = FALSE),
      guide = guide_colorbar(
        barheight = unit(5.0, "cm"),
        barwidth = unit(0.6, "cm"),
        title.position = "top",
        title.theme = element_text(size = 20, face = "bold"),
        label.theme = element_text(size = 18)
      )
      ) + 
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
        legend.position.inside = c(0.22, 0.80)
        # legend.justification = c(0, 1)
        # legend.background = element_rect(fill = "white", color = "grey80")
      )
    ggsave(plot = pp, filename = file.path(outpath_figure, "GWR_maps","Significance_5Percent", 
                                         paste(col_inter, ".png", sep = "")),
         dpi=300, width = 8, height = 8.5, units = "in")
  # }
}

############## Analysis per regions ########################

gwr_significant_gradient <- merge(gwr_significant_gradient, 
                                  dat_COROP, 
                                  by.x = "GM_CODE_23_x", 
                                  by.y = "GM_CODE", all.x = T)
df_gwr_significant_gradient <- data.frame(gwr_significant_gradient)

fun_positive_negative <- function(x, positive = TRUE){
  if(positive){
    x <- x[x>0]  
    return(mean(x, na.rm = T))
  }else{
    x <- x[x<0]  
    return(mean(x, na.rm = T))
  }
  
}


summ_positive_GWR <- df_gwr_significant_gradient %>% 
  group_by(REGIO_NAAM) %>%
  summarise(across(all_of(coefficients), ~ fun_positive_negative(.x, TRUE)))  %>% 
  as.data.frame

summ_positive_regio_GM_GWR <- df_gwr_significant_gradient %>% 
  group_by(REGIO_NAAM, GM_NAAM_23_x) %>%
  summarise(across(all_of(coefficients), ~ fun_positive_negative(.x, TRUE)))  %>% 
  as.data.frame

ttt <- summ_positive_regio_GM_GWR %>% 
  dplyr:::select(REGIO_NAAM, GM_NAAM_23_x, p_homes_without_solar_panels) %>% 
  arrange(REGIO_NAAM, GM_NAAM_23_x, p_homes_without_solar_panels)

regions <- summ_positive_GWR$REGIO_NAAM
regions <- gsub("-", "_", regions)
rownames(summ_positive_GWR) <- summ_positive_GWR$REGIO_NAAM
summ_positive_GWR$REGIO_NAAM <- NULL
summ_positive_GWR <- transpose(summ_positive_GWR)
colnames(summ_positive_GWR) <- regions
summ_positive_GWR <- data.frame(summ_positive_GWR)
summ_positive_GWR$vars <- coefficients
high_risk_regio <- apply(summ_positive_GWR[, regions], 2, sum, na.rm = T)
high_risk_regio <- apply(summ_positive_GWR[, regions], 2, sum, na.rm = T)

summ_negative_GWR <- df_gwr_significant_gradient %>% 
  group_by(REGIO_NAAM) %>%
  summarise_at(coefficients, fun_positive_negative, FALSE)%>%
  data.frame
regions <- summ_negative_GWR$REGIO_NAAM
regions <- gsub("-", "_", regions)
rownames(summ_negative_GWR) <- summ_negative_GWR$REGIO_NAAM
summ_negative_GWR$REGIO_NAAM <- NULL
summ_negative_GWR <- transpose(summ_negative_GWR)
colnames(summ_negative_GWR) <- regions
summ_negative_GWR <- data.frame(summ_negative_GWR)
summ_negative_GWR$vars <- coefficients
low_risk_regio <- apply(summ_negative_GWR[, regions], 2, sum, na.rm = T)
low_risk_regio <- apply(summ_negative_GWR[, regions], 2, sum, na.rm = T)

###########################################################

no_classes <- 4
range_LIHC <- range(g_def_wijken_23$p_LIHC) 
gwr_significant_quantile$p_log_LIHC <- log(gwr_significant_quantile$p_LIHC)
# quantiles <- gwr_significant_quantile %>%
#   pull(p_LIHC) %>%
#   quantile(probs = seq(0, 1, length.out = no_classes + 1), na.rm = T) %>%
#   as.vector() # to remove names of quantiles, so idx below is numeric
quantiles <- c(range_LIHC[1], 2, 4 ,6, 10, range_LIHC[2])
labels <- imap_chr(quantiles, 
                   function(., idx){
                     x <- paste(round(quantiles[idx], 2), "%", "- ", round(quantiles[idx+1], 2), "%", sep  ="")  
                     x
                   })
labels <- labels[1:(length(quantiles)-1)]
print(labels)
gwr_significant_quantile <- gwr_significant_quantile %>% 
  mutate(p_LIHC := cut(p_LIHC, breaks = quantiles, labels = labels, include.lowest = T))

pp <- ggplot() + 
  # Main fill layer: continuous LIHC values
  geom_sf(
    data = gwr_significant_quantile, 
    aes(fill = p_LIHC), 
    linewidth = 0.05, 
    color = NA  # use NA instead of "transparent" for clarity
  ) + 
  
  # Continuous fill scale using viridis (magma)
  scale_fill_viridis(
    option = "magma",
    name = "LIHC",
    na.value = "grey90",
    na.translate = FALSE,
    # na.name = "Non sig.",
    alpha = 0.8, # make fill a bit brighter
    begin = 0.3, 
    end = 0.9,
    discrete = T, # discrete classes, thus guide_legend instead of _colorbar
    direction = -1, # dark is lowest, yellow is highest
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = FALSE 
      )
    )+
  
  # Municipal borders
  geom_sf(
    data = g_def_gemeente_23, 
    fill = NA, 
    color = "grey35", 
    linewidth = 0.1
  ) + 
  
  # Clean theme and internal legend
  theme_minimal() + 
  theme(
    panel.border = element_blank(), 
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.80)
  )
ggsave(plot = pp, filename = file.path(outpath_figure, "Exploration", 
                                       paste("LIHC", ".png", sep = "")),
       dpi=300, width = 8, height = 8.5, units = "in")

##### overall summary 
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

