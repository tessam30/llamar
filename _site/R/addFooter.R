#' @import lubridate png grid
#' 
#' 
#' @export


add_footer = function(source,
                      exported_filename,
                      img_dir = '~/Creative Cloud Files/MAV/Graphic Resources/logos/',
                      country = 'USAID',
                      country_logo = paste0(country, '_logo.png'),
                      updated = lubridate::today(),
                      abbrev_date = FALSE,
                      width = 10, 
                      stroke_size = 0.5,
                      stroke_colour = '#00ffff',
                      
                      source_size = 5.5,
                      updated_size = 4,
                      email_size = 4,
                      
                      background_colour = grey10K,
                      text_colour = grey60K,
                      
                      font_light = 'Lato Light'
) {
  
  # set defaults -------------------------------------------------------------
  buffer = 0.1
  
  height = max(width/10, 0.5)
  height_buffer = height - 2 * buffer
  
  pct_gc = 0.2 # fraction of footer taken up by GeoCenter logo
  pct_country = 0.3 # fraction of footer taken up by country logo
  pct_date = 0.1 # fraction of the footer taken up by the date, etc.
  
  y_buffer = buffer * height # Buffer for the text argument
  
  cc = 'CC BY-NC 4.0'
  email = 'geocenter@usaid.gov'
  gc_logo = 'geocenter.png'
  
  # check image files exist -------------------------------------------------------------
  if(!country_logo %in% list.files(img_dir)) {
    error('Country logo not found')
  }
  
  
  if(!gc_logo %in% list.files(img_dir)) {
    error('GeoCenter logo not found')
  }
  
  # import images -------------------------------------------------------------
  gc = png::readPNG(paste0(img_dir, gc_logo))
  gc_grob = rasterGrob(gc, interpolate = TRUE,  
                       # width = width * pct_gc, 
                       height = height_buffer * 0.35,
                       hjust = 0, 
                       x = 0,
                       y = (height)/2,
                       default.units = 'in')
  # gc_grob = rasterGrob(gc, interpolate = TRUE, just = 'right')
  
  country = png::readPNG(paste0(img_dir, country_logo))
  country_grob = rasterGrob(country, interpolate = TRUE, 
                            hjust = 0, 
                            # width = width * pct_country, 
                            height = height_buffer,
                            y = (height)/2,
                            x = 0, default.units = 'in')
  # country_grob = rasterGrob(country, interpolate = TRUE, just = 'left')
  
  # format date -------------------------------------------------------------
  updated_label = paste0('Updated ',
                         day(updated), ' ', 
                         month(updated, label = TRUE, abbr = abbrev_date), 
                         ' ', year(updated))
  
  # format source -------------------------------------------------------------
  source_label = paste0('SOURCE: ', source)
  source_label = stringr::str_wrap(source_label, width = 40, exdent = 18.5)
  
  
  # define text location -------------------------------------------------------------
  text_loc = data.frame(x = c(width * pct_country, width - width * (pct_gc + pct_date)),
                        y = c(height - y_buffer, height - y_buffer * 2),
                        label = c(source_label, updated_label),
                        text_size = c(source_size, updated_size))
  
  email_loc = data.frame(x = c(width),
                         y = c( y_buffer),
                         label = c(email),
                         text_size = c(email_size))
  
  cc_loc = data.frame(x = c(width - width * (pct_gc + pct_date)),
                         y = c(y_buffer),
                         label = c(cc),
                         text_size = c(email_size))
  
  p = ggplot(text_loc, aes(x = x, y = y, 
                           label = label, size = text_size)) +
    
    annotation_custom(country_grob, xmin = -Inf, xmax = width * pct_country, 
                      ymin = 0, ymax = height) +
    
    annotation_custom(gc_grob, xmin = width - width * pct_gc, xmax = width, 
                      ymin = 0, ymax = height) +
    
    geom_text(colour = text_colour,
              hjust = 'inward',
              vjust = 'inward',
              family = font_light,
              data = email_loc) +
    
    geom_text(colour = text_colour,
              hjust = 0,
              vjust = 'inward',
              family = font_light,
              data = cc_loc) +
    
    geom_text(colour = text_colour,
              hjust = 0,
              vjust = 1,
              family = font_light,
              lineheight = 0.85) +
    
    geom_hline(yintercept = height,
      size = stroke_size, colour = stroke_colour) +
    
    scale_size_identity() +
    scale_x_continuous(limits = c(0, width)) +
    scale_y_continuous(limits = c(0, height)) +

    theme_blank(background_colour = background_colour)
  
  
  save_plot(exported_filename, width = width, height = height)
  
  return(p)
}