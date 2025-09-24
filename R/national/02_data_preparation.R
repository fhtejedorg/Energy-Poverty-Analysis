# ---- title: 02_data_preparation.R ----
# Purpose: Read processed data and align geo dataframe with data.frame 
# Inputs: def_wijken_23.csv, def_wijken_23.shp
# Outputs: def_wijken_23.csv, def_wijken_23.shp
# Author: Fabio Tejedor 
# Date: 2025-01-01
# Usage: R/02_data_preparation.R

# ==== Libraries ================
library(sf)  

# ==== Read data ====
def_wijken_23 <- read.delim("def_wijken_23.csv", header = T, sep = "|")
g_def_wijken_23 <- st_read("def_wijken_23.shp")

# ==== Creating urbanity degree ====

def_wijken_23["urbanity_degree"] <- ifelse(def_wijken_23[,"STED"] %in% c(1,2), "urban", 
                                           ifelse(def_wijken_23$STED %in% c(3,4), "semi_urban", 
                                                  ifelse(def_wijken_23$STED %in% c(5), "no_urban",NA)))
def_wijken_23["urbanity_degree"] <- factor(def_wijken_23[,"urbanity_degree"], 
                                           levels = c("urban", "semi_urban", "no_urban"))
g_def_wijken_23["urbanity_degree"] <- ifelse(g_def_wijken_23$STED %in% c(1,2), "urban", 
                                             ifelse(g_def_wijken_23$STED %in% c(3,4), "semi_urban", 
                                                    ifelse(g_def_wijken_23$STED %in% c(5), "no_urban",NA)))
g_def_wijken_23["urbanity_degree"] <- factor(g_def_wijken_23$urbanity_degree, 
                                             levels = c("urban", "semi_urban", "no_urban"))

# ==== Align column names (excluding geometry) ====
def_wijken_23["geometry"] <- NULL
name_cols <- names(def_wijken_23)
name_cols <- name_cols[!name_cols%in%"geometry"]

name_g_cols <- names(g_def_wijken_23)
name_g_cols <- name_g_cols[!name_g_cols%in%"geometry"]
name_g_cols

rename_columns <- data.frame(cbind(name_g_cols, name_cols))

for(ii in 1:nrow(rename_columns)){
  old <- rename_columns$name_g_cols[ii]
  new <- rename_columns$name_cols[ii]
  colnames(g_def_wijken_23)[colnames(g_def_wijken_23) == old]  <- new
}


def_wijken_23["wijk_predominant_typology"] <- ifelse(def_wijken_23[, "wijk_predominant_typology"] == "", NA, def_wijken_23[, "wijk_predominant_typology"])
g_def_wijken_23$wijk_predominant_typology <- ifelse(g_def_wijken_23$wijk_predominant_typology == "", NA, g_def_wijken_23$wijk_predominant_typology)

cat("Done!!!\nready to use def_wijken_23 and g_def_wijken_23")

