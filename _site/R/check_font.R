#' @title Checks font is installed in the system
#' 
#' @name check_font
#' 
#' @description
#' Checks if a given font is installed in the system.
#'
#' @param font_name string containing the font name
#' 
#' 
#' @import extrafont  
#' @export 


check_font = function(font_name){
  installed_fonts = extrafont::fonts()
  
  if (font_name %in% installed_fonts) {
    return(TRUE) 
  } else {
    return(FALSE)
  }
}

#' @name  replace_font
#' @title replace font
#' @param font_name string containing the font name
#' @param default_font string containng a font to use as a substituted if not found
#' @export
replace_font = function(font_name, default_font = 'sans') {
  if(check_font(font_name) == FALSE) {
    # font isn't installed
    return(default_font)
  } else{
    return(font_name)
  }
}