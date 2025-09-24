# =============================================================================
# Script Name: 00_data_collection.R
# Purpose: Analyze quarterly sales data, produce summary statistics and charts
#          to understand trends in regional sales performance.
# 
# Author: Fabio Tejedor
# Date Created: 2024-11-14
# 
# Inputs: "gdf_wijk_2023.shp" - data with district geometries from 2023, and figures  
# Outputs: summary_statistics.csv, sales_trends.png
#
# Description:
# This script loads sales data, calculates key statistics (e.g., average and 
# total sales by region), and generates visualizations of sales trends over time.
# 
# Packages Required:
# - dplyr
# - ggplot2
# - readr
#
# =============================================================================

library(sf)


# =============================================================================
dt_wijken <- read_sf("../data/wijken/gdf_wijk_2022.shp")
head(dt_wijken)

