
<!-- README.md is generated from README.Rmd. Please edit that file -->
Foresight
=========

Thanks for gertstulp's work, currently the main framework of this package is based on ggplotgui.
However, Because there is only one script file for ggplotgui,it's difficult to extend, so I try to
reformulate ggplot_shiny() by seperating files for easy writing and extending. More manipulating
please referto https://github.com/gertstulp/ggplotgui/blob/master/README.md



Installation
============

``` r

# In order to install the most recent version of this package, you'll need to use the "devtools"-package
install.packages("devtools")

#currently, this package depends on  ggplot2, plotly.
install.packages("shiny")
install.packages("plotly")

devtools::install_github("ShouyeLiu/Foresight")

```

Usage
=====

There are two ways to use this functionality:

-   By calling it from within R

``` r
# You can call the function with and without passing a dataset
library("Foresight")
initApp()
initApp(iris3) 
```
