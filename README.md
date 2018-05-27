
<!-- README.md is generated from README.Rmd. Please edit that file -->
Foresight

Thanks for gertstulp's work, currently the main framework of this package is based on ggplotgui.
However, due to there is only one script file for ggplotgui, so it's difficult to extend, so I 
reformulated ggplot_shiny() to seperate files for easy writing and extending. more manipulating
please refer to https://github.com/gertstulp/ggplotgui/blob/master/README.md
=========


Installation
============

``` r
install.packages("Foresight")

# In order to install the most recent version of this package, you'll need to use the "devtools"-package
install.packages("devtools")
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
initApp(iris3) # Passing ggplot's mpg dataset
```
