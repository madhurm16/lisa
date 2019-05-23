Labor Share & Aging Population
================
Fabien Petit

``` r
loc_result = file.path(getwd(), "result")
loc_function = file.path(getwd(), "function")
loc_data = file.path(getwd(), "data")
```

``` r
# Load packages
packages <- c("bucky", "dplyr", "ggplot2", "ggrepel", "lmtest", "sandwich", "zoo", "RColorBrewer", "reshape2")
lapply(packages, require, character.only = TRUE)
```

    ## Loading required package: bucky

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: ggplot2

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

    ## Loading required package: ggrepel

    ## Loading required package: lmtest

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: sandwich

    ## Loading required package: RColorBrewer

    ## Loading required package: reshape2

``` r
rm(packages)

# Load functions
sapply(list.files(pattern = "[.]R$", path = loc_function, full.names = TRUE), source)
```
