
<!-- README.md is generated from README.Rmd. Please edit that file -->
Foresight
=========


Installation
============

``` r
install.packages("Foresight")

# In order to install the most recent version of this package, you'll need to use the "devtools"-package
install.packages("devtools")
devtools::install_github("ShouyeLiu/Foresight")

library("Foresight")
```

Usage
=====

There are two ways to use this functionality:

-   By calling it from within R

``` r
# You can call the function with and without passing a dataset
initApp()
initApp(iris3) # Passing ggplot's mpg dataset
```
