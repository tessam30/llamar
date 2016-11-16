addFooter = function(..., plot1) {
    
    # Get the height of the plot object.
    
    grid.newpage()
    
    pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 1, heights = c(unit(0.9,
        "npc"), unit(0.1, "npc")))))
    
    
    print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    
    print(ggplot(mtcars, aes(x = mpg, y = cyl)) + geom_point(), vp = viewport(layout.pos.row = 2, 
        layout.pos.col = 1))
}
