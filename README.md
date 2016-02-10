# llamar
A set of custom reusable plotting functions in R

## installation
`install.packages("devtools")`

`library(devtools)`

`devtools::install_github("flaneuse/llamar")`

`library(llamar)`

## themes
The primary function of llamar is developing a set of custom, pre-defined clean themes, building off Hadley Wickham's awesome [ggplot2](ggplot2.org) package.

All examples use the inbuilt mtcars dataset:

`p = ggplot(mtcars, aes(x = mpg, y = wt, colour = cyl)) + geom_point() + ggtitle('title')`

### themes with gridlines
#### theme_xylab
`p + theme_xylab()`
![theme_xylab](/img/xylab.png)

### themes without gridlines
#### theme_blank
`p + theme_blank()`
![theme_blank](/img/blank.png)
