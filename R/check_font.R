#' @title Checks font is installed in the system
#' 
#' 
#' @description
#' Checks if a given font is installed in the system.
#'
#' @param font_name string containing the font name
#' 
#' 
# @import extrafont  
#' @examples 
#' check_font('Arial')
#' check_font('ARIAL')
#' replace_font('ARIAL') # Returns 'sans'

#' @describeIn check_font Returns TRUE/FALSE if font is installed.
check_font = function(font_name){
  installed_fonts = extrafont::fonts()
  
  if (font_name %in% installed_fonts) {
    return(TRUE) 
  } else {
    return(FALSE)
  }
}

#' @describeIn check_font Replace font if it isn't found in the system
#' @param default_font string containg a font to use as a substituted if not found
replace_font = function(font_name, default_font = 'sans') {
  if(check_font(font_name) == FALSE) {
    # font isn't installed
    return(default_font)
  } else{
    return(font_name)
  }
}