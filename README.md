
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Calibration Training App <img src="www/university_logo.jpg" align="right" height="200" />

<!-- badges: start -->
<!-- badges: end -->

The goal of the Calibration Admin App is to help the [University of
Bonn](https://www.uni-bonn.de/en/university/) view the individual &
group results of the *calibration* assessments they administer in
real-time…

## Installation

1.  Clone this repository to your local machine

2.  Open the **calibration\_admin.Rproj** file from the directory on
    your local machine where you cloned this repository. This should
    install the {renv} package if you do not already have it installed,
    but if you don’t see that happen in the console, run
    `install.packages("renv")`.

3.  Run `renv::restore()` to install the package dependencies needed to
    run this app successfully

4.  Edit the [global.R](global.R) script as necessary (see
    [below](#app-parameters))

5.  Open the [app.R](app.R) file and execute the code in that script to
    launch the app

### App Parameters

Before a calibration workshop, the [global.R](global.R#L7) script should
be edited to define the following parameter for the workshop:

-   The [{pins}](https://pins.rstudio.com/) board on RStudio Connect
    that has been dedicated for this workshop

------------------------------------------------------------------------

See below for session info for full reproducibility:

``` r
sessionInfo()
#> R version 4.1.1 (2021-08-10)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19042)
#> 
#> Matrix products: default
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.1252 
#> [2] LC_CTYPE=English_United States.1252   
#> [3] LC_MONETARY=English_United States.1252
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.1252    
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices datasets  utils     methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_4.1.1  magrittr_2.0.1  fastmap_1.1.0   htmltools_0.5.2
#>  [5] tools_4.1.1     yaml_2.2.1      stringi_1.7.6   rmarkdown_2.11 
#>  [9] knitr_1.37      stringr_1.4.0   xfun_0.29       digest_0.6.29  
#> [13] rlang_0.4.12    renv_0.14.0     evaluate_0.14
```
