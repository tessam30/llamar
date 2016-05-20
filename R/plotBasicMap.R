#' Import ESRI shapefiles into R
#' 
#' Import shapefile and convert into lat/lon coordinates to plot in R or Tableau
#'
#' View available projections with 'projInfo(type = 'proj')'
#' View available datum with 'projInfo(type = 'datum')'
#' View available ellipsoids with 'projInfo(type = 'ellps')'
#' @author Laura Hughes, laura.d.hughes@gmail.com
#' @param 
#' @examples
#' 
#' 
#' 

# Basic plot function to check that the data look correct ----------------- Note: if you
# have lots of complex polygons, it'll take awhile to plot.  In that case, it's easier
# to save the plot to a variable and export the plot as a .pdf Don't be alarmed if the
# function takes a minute or two to render
plotMap = function(df, exportPlot = FALSE, fileName = "map.pdf", plotWidth = 6, plotHeight = 6) {
    
    p = ggplot(df, aes(x = long, y = lat, group = group, fill = id)) + geom_polygon() + 
        theme_void() + coord_equal() + theme(legend.position = "none")
    
    if (exportPlot == TRUE) {
        ggsave(filename = fileName, width = plotWidth, height = plotHeight, bg = "transparent", 
            paper = "special", units = "in", useDingbats = FALSE, compress = FALSE, dpi = 300)
    }
    
    return(p)
    
}
