#' Exploratory plotting functions 
#'
#' @description These functions help to summarise some results during the analysis process 
#' @param vars Numeric vector of values.
#' @param dat_wijk data frame with rows as districts
#' @param plot_type arguments can be: boxplot, correlation, scatter
#' @param var_interest variable y to make a comparison across vars
#' @param param_dist parameter associated to the distance when applying nearest neighborhood method to create contiguity matrix W
#' @param outpath_figure directory path to save plots 
#' @return Series 
#' @examples
#' @export
#' 
fun_plots <- function(vars, dat_wijk, plot_type, mfrow = c(1,1), var_interest){
  if(plot_type == "boxplot"){
    par(mfrow)
    for(ii in vars){
      boxplot(dat_wijk[ii], main = ii)
    }
  }
  if(plot_type == "correlation"){
    cor_matrix <- cor(na.omit(dat_wijk[vars]))
    round(cor_matrix, 2)
    corrplot::corrplot(cor_matrix, method = "circle")
  }
  if(plot_type == "scatter"){
    dat_wijk = dat_wijk %>%
      filter(variable %in% c(vars))
    p1 <- ggplot(dat_wijk, aes(x = .data[["value"]], y = .data[[var_interest]])) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
      facet_wrap(~variable, scales = "free_x") +  # Facet by variable
      labs(
        y = var_interest
      ) +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=16,face="bold"),
            strip.text = element_text(size = 20)) 
    return(p1)
  }
}

plot_neighbours_W <- function(param_dist, outpath_figure){
  nb_dist <- dnearneigh(coords, 0, param_dist, longlat = FALSE) 
  lw_distance <- nb2listw(nb_dist, style="W", zero.policy=TRUE) 
  col_nb1_sf = spdep::nb2lines(lw_distance$neighbours, coords=coords, 
                               proj4string=crs_info$proj4string, as_sf=T) # weights are binary
  n_neigh <- attr(lw_distance$weights, "comp")$d
  name_var <- paste("n_neigh_", param_dist, sep = "")
  n_neigh <- ifelse(n_neigh == 0, NA, n_neigh)
  g_def_wijken_23[, "n_neigh"] <- n_neigh
  title <- paste("distance  = ", param_dist, sep = "")
  pp <- ggplot() +
    geom_sf(data = g_def_wijken_23, fill = alpha("green",0.7), colour = alpha("green",0.2)) +
    geom_sf(data = g_def_wijken_23[, c("X", "Y")], color = "blue", size = 0.01) + 
    geom_sf(data = col_nb1_sf, color = "red", linewidth = 0.01) + 
    ggtitle(title)
  nn <- paste("network_distance_",param_dist , ".png", sep = "")
  ggsave(plot = pp, filename = file.path(outpath_figure, nn), dpi=1000)
  
  pp <- ggplot() +
    geom_sf(data = g_def_wijken_23, aes(fill = n_neigh)) +
    scale_fill_viridis_c(option= "magma",na.value="white", begin = 0.2, direction = -1, name = name_var) +
    ggtitle(title)
  nn <- paste("N_Neighbours_distance_",param_dist , ".png", sep = "")
  ggsave(plot = pp, filename = file.path(outpath_figure, nn), dpi=1000)
}

fun_summary_model <- function(model_inter){
  cat("----- SUMMARY -----------\n")
  print(summary(model_inter))
  cat("----- ANOVA -----------\n")
  print(anova(model_inter))
  cat("----- VIF -----------\n")
  print(vif(model_inter))
}