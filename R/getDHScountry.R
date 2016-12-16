#' Create a lookup table for DHS country codes, and find the country code based on the list
#' 
#' Can take either a single country or multiple; does 
#' @import dplyr data.table
#' @export
getDHScountry = function(country_list, return_table = FALSE){
  
  # function to pull the country name
  return_country = function(sel_country){
    
    filtered = DHScountries %>% filter(country %like% sel_country)
    
    return(filtered$code)
  }
  
  # Apply to the list
  if(return_table == TRUE) {
    warning('Using exact matching of country names')
    filtered = DHScountries %>% 
      filter(country %in% country_list)
    
    return(filtered)
  } else{
    # Collapse to a comma-separated list
    codes = lapply(country_list, function(x) return_country(x))
    
    # Convert to a single list
    codes = combine(codes)
    
    # Convert to a comma-separated list
    codes = paste0(codes, collapse=',')
    
    return(codes)
  }
}

#' @import dplyr rvest
importDHScountries = function(save_file = FALSE,
                              file_name = '~/GitHub/llamar/data/DHScountries.rda') {
  dhs_country = read_html('http://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10136')
  
  codes = dhs_country %>%
    html_node('#CS_Element_countrycodes .CS_Textblock_Text [summary="Table Summary"]') %>% 
    html_table(header = TRUE)
  
  
  
  # clean up to remove the crap.
  colnames(codes) = c('code1', 'country1', 'code2', 'country2')
  
  # divide into 2 and bind
  left = codes %>% 
    slice(-1) %>% 
    select(code1, country1) %>% 
    rename(code = code1, 
           country = country1)
  
  right = codes %>% 
    slice(-1) %>% 
    select(code2, country2) %>% 
    rename(code = code2, 
           country = country2)
  
  DHScountries = bind_rows(left, right)
  
  if(save_file == TRUE){
    save(DHScountries, file = file_name)
  }
  
}

