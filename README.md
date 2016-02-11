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
#### theme_basic
`p + theme_basic()`

![theme_basic](/img/basic.png)

#### theme_xygrid
`p + theme_xygrid()`

![theme_xygrid](/img/xygrid.png)

#### theme_xgrid
`p + theme_xgrid()`

![theme_xgrid](/img/xgrid.png)

#### theme_ygrid
`p + theme_ygrid()`

![theme_ygrid](/img/ygrid.png)


### themes without gridlines
#### theme_blank
`p + theme_blank()`

![theme_blank](/img/blank.png)

#### theme_xaxis
`p + theme_xaxis()`

![theme_xaxis](/img/xaxis.png)

#### theme_yaxis
`p + theme_yaxis()`

![theme_yaxis](/img/yaxis.png)

#### theme_xylab
`p + theme_xylab()`

![theme_xylab](/img/xylab.png)

#### theme_labelsOnly
`p + theme_labelsOnly()`

![theme_labelsOnly](/img/labelsOnly.png)
