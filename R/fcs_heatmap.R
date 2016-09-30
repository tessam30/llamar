#' Simple wrapper to ggsave
#' 
#' Wrapper to ggsave to auto-save the annoying fiddly arguments I always forget
#' 
#' @import ggplot2 extrafont
#' 
#' @export

# Requires ggplot2 > 

fcs_heatmap <- function(filename = NA,
                        width = NA, 
                        height = NA, 
                        units = 'in', 
                        scale = 1) {
  
  
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
  
  
  
  
  if (!is.na(filename)){
    save_plot(filename, width, height, units, scale)
  }
}