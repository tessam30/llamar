#' Load commonly used functions
#' 
#' This function calls nothing, but preloads commonly used R packages for analysis.
#' @export


loadPkgs <- function(pkgGrp = 'all', quiet = FALSE) {
  
  # Package list ------------------------------------------------------------
  
  
  # -- Data manipulation -- 
  wrangling = c('dplyr', 'tidyr', 'data.table', 'stringr', 
                'lubridate', 'purrr', 'zoo')
  # dplyr: filter, create new variables, summarise, ... Basically, anything you can think to do to a dataset
  # tidyr: Reshape and merge datasets
  # data.table: similar to dplyr but good for large datasets; some extra functionality
  # stringr: String manipulation
  # lubridate: dates
  # zoo: running averages, amongst other things.
  
  
  # -- Plotting / viz functions -- 
  plotting = c('ggplot2', 'ggvis', 'htmltools', 'htmlwidgets',
               'metricsgraphics', 'rCharts', 'plotly', 'ggmap',
               'choroplethrAdmin1', 'choroplethr', 'd3heatmap',
               'DiagrammeR', 'ggrepel', 'hexbin', 'lattice', 
               'latticeExtra', 'packcircles', 'rbokeh', 'waffle',
               'RgoogleMaps', 'shiny', 'shinydashboard', 'shinythemes')  
  # ggplot2: Incredibly powerful plotting library built off of the "Grammer of Graphics"
  # ggmpap: geocoding and geospatial library
  
  # -- publication / appearance / plotting support --
  pub = c('animation', 'gridExtra', 'grid', 'knitr', 'RColorBrewer', 'extrafont')
  # knitr: Helper function to produce RMarkdown documents.
  
  
  # -- Libraries to help import files --
  importing = c('haven', 'readr', 'readxl', 'googlesheets', 'rvest', 'foreign')
  # haven: Imports in files from Stata, SAS, and SPSS
  # readr: An advanced form of the base 'read.csv' file with some added functionality.
  # readxl: Functions to import in multiple sheets from Excel
  # googlesheets: Functions to connect to Google Drive spreadsheets.
  # rvest: Scrapes websites
  
  # -- Developer libraries --
  devPkgs = c('roxygen2', 'testthat', 'jsonlite', 'microbenchmark',
              'profvis')
  
  # -- Fitting libraries --
  fitting = c('MASS', 'sandwich', 'lmtest', 'plm', 'ggalt', 'coefplot', 'broom', 
              'cluster', 'GWmodel')

  

  
  # Pick which packages to load ---------------------------------------------
  
  if (pkgGrp == 'all'){
    pkgs =  c(wrangling, plotting, pub, importing, devPkgs, fitting)
  }
  
  
  
  # Install and load packages -----------------------------------------------
  
  # Check if packages are installed
  alreadyInstalled = installed.packages()[,'Package']
  
  toInstall = pkgs[!pkgs %in% alreadyInstalled]
  
  # Install anything that isn't already installed.
  if(length(toInstall > 0)) { 
    print(paste0('Installing these packages: ', toInstall))
    
    install.packages(toInstall)
  }
  
  # Load packages
  for (i in seq_along(pkgs)) {
    library(pkgs[i], character.only = TRUE, quietly = quiet)
  }
  
  loadfonts(quiet = TRUE)
  
}


