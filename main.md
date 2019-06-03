Labor Share & Aging Population
================
Fabien Petit

## Data

### Raw data

Data are from :

  - Penn World Table 9.1
  - OECD Database
  - United Nations (World Population Prospects)

<!-- end list -->

    ## # A tibble: 6 x 15
    ##   Country  Year    Ny    No     n     p    p1   dep     K     L     Y theta
    ##   <fct>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 France   1970  1.60 0.588  1.13 0.417 0.583 0.368  1     1     1    0.730
    ## 2 France   1971  1.62 0.598  1.14 0.422 0.591 0.370  1.06  1.00  1.05 0.726
    ## 3 France   1972  1.64 0.610  1.15 0.429 0.597 0.372  1.15  1.01  1.13 0.729
    ## 4 France   1973  1.66 0.619  1.16 0.435 0.603 0.374  1.23  1.02  1.21 0.717
    ## 5 France   1974  1.67 0.623  1.18 0.438 0.607 0.372  1.32  1.04  1.28 0.720
    ## 6 France   1975  1.69 0.620  1.19 0.437 0.609 0.366  1.41  1.03  1.29 0.738
    ## # ... with 3 more variables: tau <dbl>, u <dbl>, k <dbl>

### Data for simulation

    ## # A tibble: 6 x 26
    ## # Groups:   Country [1]
    ##   Country  Year Sequence Period    Ny     No     n     p    p1 eta   AK   
    ##   <fct>   <int>    <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <lgl> <lgl>
    ## 1 France   1970        1      1  1.60  0.588  1.13 0.417 0.583 NA    NA   
    ## 2 France   1980        2      1  1.77  0.564  1.39 0.444 0.613 NA    NA   
    ## 3 France   1990        3      1  1.94  0.691  1.36 0.486 0.667 NA    NA   
    ## 4 France   2000        4      1  2.08  0.794  1.38 0.525 0.683 NA    NA   
    ## 5 France   2010        1      2 NA    NA      1.33 0.583 0.685 NA    NA   
    ## 6 France   2020        2      2 NA    NA      1.13 0.613 0.707 NA    NA   
    ## # ... with 15 more variables: AL <lgl>, k1 <lgl>, k2 <lgl>, k <lgl>,
    ## #   X <lgl>, L <lgl>, w <lgl>, Y <lgl>, u <lgl>, theta <lgl>, tau <lgl>,
    ## #   b <lgl>, h <lgl>, S <lgl>, K <dbl>

## Parameter calibration

The discount factor
![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\\alpha") is
set to 0.669. The relative bargaining power of the union
![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma "\\gamma") is
set to 0.5. *For more details, please consult “gamma.md” file.*

### Production function

I estimate the elasticity of substitution between capital and labor
using the methodology of **Leon-Ledesma, McAdam & Willman (2013)**.
Estimation are done for France and United States between 1970 and 2010,
using Penn World Table 9.1 data. *For more details, please consult
“sigma.md” file.*

    ## 
    ## Call:  lm(formula = k_log ~ Country + THETA_log_neg * Country + t_diff * 
    ##     Country - THETA_log_neg - t_diff, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                      Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                         0.4582565  0.1684809   2.720  0.00809
    ## CountryUnited States               -0.3404410  0.1742999  -1.953  0.05448
    ## CountryFrance:THETA_log_neg        -0.2623562  0.1475026  -1.779  0.07930
    ## CountryUnited States:THETA_log_neg -0.1831404  0.0712163  -2.572  0.01207
    ## CountryFrance:t_diff                0.0229846  0.0026920   8.538 1.03e-12
    ## CountryUnited States:t_diff         0.0130495  0.0003861  33.794  < 2e-16
    ##                                       
    ## (Intercept)                        ** 
    ## CountryUnited States               .  
    ## CountryFrance:THETA_log_neg        .  
    ## CountryUnited States:THETA_log_neg *  
    ## CountryFrance:t_diff               ***
    ## CountryUnited States:t_diff        ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##          Country       phi    sigma          a
    ## 1         France 0.2702488 1.355668 0.02298463
    ## 13 United States 0.3247109 1.224201 0.01304954

### Preferences

The relative per-capita influence of old households
![\\omega](https://latex.codecogs.com/png.latex?%5Comega "\\omega") is
deduced such that the capital-to-labor ratio is matched in 1970. The
preference for government health expenditure
![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\\beta") is
deduced such that the tax rate in 1970 is matched.

    ##          Country     omega      beta
    ## 1         France 0.9180683 0.4687702
    ## 13 United States 1.5163444 0.1042991

### Scale parameter

The scale parameter ![A](https://latex.codecogs.com/png.latex?A "A") is
decuded using grid search to match the average labor share between 2008
and 2012.

### Summary

All parameters are constant over
    time.

    ##          Country     alpha gamma       phi    sigma     omega      beta
    ## 1         France 0.6689718   0.5 0.2702488 1.355668 0.9180683 0.4687702
    ## 13 United States 0.6689718   0.5 0.3247109 1.224201 1.5163444 0.1042991
    ##         A
    ## 1  19.834
    ## 13 23.508

## Simulation

### Baseline model

![](main_files/figure-gfm/Graph%20-%20BModel%20Labor%20Share%20-%20Plot-1.png)<!-- -->

![](main_files/figure-gfm/Graph%20-%20BModel%20Performance%20-%20Plot-1.png)<!-- -->

![](main_files/figure-gfm/Graph%20-%20BModel%20Variables-1.png)<!-- -->![](main_files/figure-gfm/Graph%20-%20BModel%20Variables-2.png)<!-- -->![](main_files/figure-gfm/Graph%20-%20BModel%20Variables-3.png)<!-- -->

### Counterfactual simulation

![](main_files/figure-gfm/Graph%20CModel%20-%20PGSR%20Counterfactual-1.png)<!-- -->

![](main_files/figure-gfm/Graph%20CModel%20-%20DEIE%20Counterfactual-1.png)<!-- -->

![](main_files/figure-gfm/Graph%20CModel%20-%20PGSR%20Decomposition-1.png)<!-- -->

![](main_files/figure-gfm/Graph%20CModel%20-%20DEIE%20Decomposition-1.png)<!-- -->
