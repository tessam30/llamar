#' Calculate and plot a correlation matrix.
#' 
#' @description Plots coefficients from a model, along with standard errors.  Coefficients that are statistically significant at agiven level are highlighted in darker text. Inspired by https://github.com/jaredlander/coefplot/blob/master/R/coefplot.r but works w/ ggplot2 version > 2.2
#' 
# @import ggplot2 tidyr RColorBrewer dplyr extrafont forcats
#' 
#' @param df data frame in a tidy format
#' 
#' @examples 
#' plot_corr(mtcars)

plot_heatmap = function(df,
                        fill_var = 'value',
                        remove_same = TRUE,
                        remove_half = TRUE,
                        label_pct = TRUE,
                        stroke_colour = 'white',
                        stroke_size = 0.5,
                        square_tiles = TRUE,
                        fill_palette = brewer.pal(11, 'RdYlBu'),
                        fill_limits = c(-1, 1),
                        font_normal = 'Lato',
                        font_semi = 'Lato Light',
                        font_light = 'Lato Light'
) {
  
  # check if the fonts are installed.  If not, default to 'sans'.
  font_normal = llamar::replace_font(font_normal)
  font_semi = llamar::replace_font(font_semi)
  font_light = llamar::replace_font(font_light)
  
  
  
  # find indices to label diagonals
  if(remove_half == TRUE) {
    diag_labels = cor_matrix %>% 
      group_by(x) %>% 
      summarise(row_id = min(row_id))
    
    # refactor
    diag_labels$x = factor(diag_labels$x)
    diag_labels$x = forcats::fct_reorder(diag_labels$x, diag_labels$row_id)
  } 
  
  # Remove NA values
  # Must be done after finding diagonal values, but before refactoring x/y
  cor_matrix = cor_matrix %>% filter(!is.na(corr))
  
  # refactor
  cor_matrix$x = factor(cor_matrix$x)
  cor_matrix$x = forcats::fct_reorder(cor_matrix$x, cor_matrix$row_id)
  cor_matrix$y = forcats::fct_reorder(cor_matrix$y, cor_matrix$row_id)
  
  
  p = ggplot(cor_matrix, aes(x = x, y = y)) +
    # -- heatmap --
    geom_tile(aes_string(fill = 'fill_var'),
              colour = stroke_colour, size = stroke_size) +
    
    
    # -- scales --
    scale_fill_gradientn(colours = fill_palette, 
                         limits = fill_limits) +
    
    # -- themes --
    theme_xylab(font_normal = font_normal, 
                font_semi = font_semi,
                font_light = font_light) 
  
  # -- axes --
  if(remove_half == TRUE) {
    p = p + 
      geom_text(aes(x = row_id - 0.75, y = row_id, label = x), 
                hjust = 1, data = diag_labels, 
                colour = grey60K, family = font_light,
                size = 3.52778) +
      theme(axis.text.y = element_blank()) 
  }
  
  # -- labels --
  if(label_pct == TRUE) {
    p = p +
      geom_text(aes(colour = corr, label = percent(corr, 0))) +
      scale_colour_text(data_col = cor_matrix$corr) 
  }
  
  # -- equal, square tiles --
  if(square_tiles == TRUE & remove_half == TRUE) {
    p = p + coord_equal(xlim = c(-1, nrow(diag_labels) + 1), 
                        ylim = c(1, nrow(diag_labels)))
  } else if(square_tiles == TRUE) {
    p =  p + coord_equal()
  } else if (remove_half == TRUE) {
    p = p + scale_x_discrete(expand = c(0.1, 0))
  } 
  
  return(p)
}