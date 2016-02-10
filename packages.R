# File to install R packages I use (for use in updating R versions...)

# general programming / data manipulation
install.packages(c('dplyr', 'tidyr', 'data.table',
                   'animation', 'assertthat', 
                   'devtools', 'foreign', 'haven',
                   'extrafont', 'formatR', 'formattable',
                   'knitr', 'git2r', 'googlesheets', 
                   'gridExtra', 'knitr', 'lubridate', 
                   'microbenchmark', 
                   'readr', 'readxl', 'RCurl', 'roxygen2', 'rvest',
                   'shiny', 'shinydashboard', 'shinythemes', 'stringr',
                   'testthat', 'zoo'
                   ))

# Stats
install.packages(c('broom', 'coefplot', 'cluster',
                   'GWmodel', 'sandwich', 'spatial'))

# vis
install.packages(c('ggplot2', 'ggvis', 
                   'htmlwidgets', 'd3heatmap',
                   'ggmap', 'ggalt', 'GGally', 'ggdendro',
                   'hexamapmaker', 'hexbin',
                   'lattice', 'packcircles',
                   'plotly', 'rbokeh', 'rCharts',
                   'RColorBrewer', 'RgoogleMaps', 'waffle',
                   'choroplethr', 'choroplethrAdmin1'))

# Bio stuff
install.packages(c('Biobase', 'BiocGenerics', 'biomaRt'))

# 'profvis', 'grid'