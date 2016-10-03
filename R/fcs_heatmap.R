#' Plot food consumption scores by region
#' 
#' In its full version, a plot with four parts: (1) a heatmap breaking down the consumption of food
#' groups by region; (2) a kernel density estimate surface of the food consumption scores (FCS) of
#' individual households in the region; (3) a 1 x n heatmap of the average FCS score by region;
#' and (4) individual mini-maps highlighting the region for context.
#' 
#' @import ggplot2 extrafont survey gridExtra
#' 
#' @export

# Requires ggplot2 >

fcs_heatmap <- function(df,
                        
                        # -- map options --
                        map_base = grey15K,
                        map_accent = '#d53e4f',
                        
                        # -- file saving options --
                        filename = NA,
                        width = NA, 
                        height = NA, 
                        units = 'in', 
                        scale = 1) {
  
  # -- SETUP: 
  
  # -- PART 0: calculate weighted averages for values --
  
  
  # -- PART 1: individual maps --
  
  
  ggplot(rel_fcs_heat) +
    geom_tile(aes(x = food, y = regionName, fill = rel_mean), 
              color = 'white', size = 1) +
    scale_fill_gradientn(colours = PlBl, 
                         limits = c(-8.2,8.2)) +
    
    geom_text(aes(y = food, x = regionName, label = round(rel_mean,1)), size = 4) +
    
    
    ggtitle('FCS, relative to the national average') +
    
    # -- force plot to be square --
    coord_fixed(ratio = 1) +
    
    # -- themes --
    theme_xylab() +
    
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          title = element_text(size = 18, family = 'Segoe UI', hjust = 0, color = grey60K))
  
  
  
  
}