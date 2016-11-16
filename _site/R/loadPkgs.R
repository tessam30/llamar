#' Load commonly used functions
#' 
#' This function calls nothing, but preloads commonly used R packages for analysis.
#' @export


loadPkgs <- function(pkgGrp = "all", quiet = FALSE) {
    
    # Package list ------------------------------------------------------------
    
  # -- Libraries to help import files --
  importing = c("haven", "readr", "readxl", "googlesheets", 
                "rvest", "foreign", "pdftools", "httr")
  # haven: Imports in files from Stata, SAS, and SPSS readr: An advanced form of the base
  # 'read.csv' file with some added functionality.  readxl: Functions to import in
  # multiple sheets from Excel googlesheets: Functions to connect to Google Drive
  # spreadsheets.  rvest: Scrapes websites pdftools: Scrapes pdf files
  # httr: OAuth, API connections
  
    # -- Data manipulation --

    wrangling = c("dplyr", "tidyr", "data.table", "dtplyr",
                  "stringr", "stringdist", "RecordLinkage",
                  "lubridate", "seasonal", "forcats",
                  "purrr", "zoo")
    # dplyr: filter, create new variables, summarise, ... Basically, anything you can think
    # to do to a dataset tidyr: Reshape and merge datasets data.table: similar to dplyr but
    # good for large datasets; some extra functionality stringr: String manipulation
    # lubridate: dates zoo: running averages, amongst other things.
    
    
    # -- Plotting / viz functions --
    plotting = c("ggplot2", "ggvis", "htmltools", "htmlwidgets", "metricsgraphics",  
        "plotly", "d3heatmap", "DiagrammeR", "GGally", "ggdendro", "highcharter",
        "ggrepel", "hexbin", "lattice", "latticeExtra", "packcircles", "rbokeh", "waffle")
    
    interactivity = c("DT",
         "shiny", "shinydashboard", "shinythemes")
    # ggplot2: Incredibly powerful plotting library built off of the 'Grammer of Graphics'
    # ggmpap: geocoding and geospatial library
    
    # -- Mapping --
    mapping = c( "ggmap", "choroplethrAdmin1", "choroplethr", "rmapshaper",
                 "RgoogleMaps", "rgeos", "rgdal", "leaflet")
    
    # -- publication / appearance / plotting support --
    pub = c("animation", "gridExtra", "grid", "knitr", "formattable",
            "colorspace", "RColorBrewer", "viridis", "extrafont")
    # knitr: Helper function to produce RMarkdown documents.
    
    

    
    # -- Developer libraries --
    devPkgs = c("roxygen2", "testthat", "jsonlite", "microbenchmark", "profvis", "packrat")
    
    # -- Fitting libraries --
    fitting = c("MASS", "sandwich", "lmtest", "plm", "ggalt", "coefplot", "broom", "cluster", 
        "GWmodel", "mosaic", "modelr")
    
    # development versions ----------------------------------------------------
    
    devtools::install_github("rstudio/crosstalk")
    devtools::install_github("jcheng5/d3scatter")
    
    
    # Pick which packages to load ---------------------------------------------
    
    if (pkgGrp == "all") {
        pkgs = c(fitting, wrangling, plotting, interactivity, mapping, pub, importing, devPkgs)
    }
    
    
    
    # Install and load packages -----------------------------------------------
    
    # Check if packages are installed
    alreadyInstalled = installed.packages()[, "Package"]
    
    toInstall = pkgs[!pkgs %in% alreadyInstalled]
    
    # Install anything that isn't already installed.
    if (length(toInstall > 0)) {
        print(paste0("Installing these packages: ", paste0(toInstall, collapse = ", ")))
        
        install.packages(toInstall)
    }
    
    
    # Load packages
    for (i in seq_along(pkgs)) {
        library(pkgs[i], character.only = TRUE, quietly = quiet)
    }
    
    loadfonts(quiet = TRUE)
    
}




