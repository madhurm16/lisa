LISA - Gamma specification
================
Fabien Petit

## Data

Data are from Penn World Table 9.1, OECD Database and United Nations
(World Population Prospects).

    ## # A tibble: 6 x 15
    ##   Country  Year    Ny    No     n     p    p1   dep     K     L     Y theta
    ##   <fct>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 France   1970  1.17 0.430  1.13 0.417 0.583 0.368  1     1     1    0.730
    ## 2 France   1971  1.18 0.437  1.14 0.422 0.591 0.370  1.06  1.00  1.05 0.726
    ## 3 France   1972  1.19 0.444  1.15 0.429 0.597 0.372  1.15  1.01  1.13 0.729
    ## 4 France   1973  1.21 0.451  1.16 0.435 0.603 0.374  1.23  1.02  1.21 0.717
    ## 5 France   1974  1.22 0.454  1.18 0.438 0.607 0.372  1.32  1.04  1.28 0.720
    ## 6 France   1975  1.24 0.454  1.19 0.437 0.609 0.366  1.41  1.03  1.29 0.738
    ## # ... with 3 more variables: tau <dbl>, u <dbl>, k <dbl>

## Parameter calibration

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

### Wage bargaining

I create 4 different specifications of the relative bargaining power of
the union.

  - *gamma.cst* : Constant value of 0.5. This specification will be used
    for prediction.
  - *gamma0* : Proxy of the bargaining power. It depends on the trade
    union density and the collective union coverage. I use a square root
    function to have a concave function of both variables and smooth the
    variations in bargaining power.
  - *gamma1* : Rescale gamma0 such that gamma1 = 0.5 in 1970.
  - *gamma2* : Rescale gamma0 such that mean(gamma2) = 0.5.

Both last specifications will be used to improve the quality of the fit
on the period 1970 - 2010. Only the first specification will be used for
prediction.

### Preferences

According to the specification of
![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma "\\gamma"), the
parameters ![\\omega](https://latex.codecogs.com/png.latex?%5Comega
"\\omega") and ![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta
"\\beta") are different. Therefore, I compute parameter values for each
specification.

    ##          Country omega.cst   omega0   omega1   omega2  beta.cst     beta0
    ## 1         France  1.220024 1.216643 1.220024 1.222749 1.4876013 1.4789935
    ## 13 United States  1.907372 1.891257 1.907372 1.924234 0.7278886 0.7072845
    ##        beta1     beta2
    ## 1  1.4876013 1.4945053
    ## 13 0.7278886 0.7490792

## Simulation

I simulate the model first of all with a different
![A](https://latex.codecogs.com/png.latex?A "A") parameter for each
![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma "\\gamma")
specification and then with the same
![A](https://latex.codecogs.com/png.latex?A "A") parameter.

### Different scale parameter (A)

![](gamma_files/figure-gfm/Gamma%20analysis-1.png)<!-- -->

Decreasing ![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma
"\\gamma") does not improve the model performance. Prediction are almost
the same whatever the
specification.

### Same scale parameter (A)

![](gamma_files/figure-gfm/Gamma%20analysis%20with%20same%20A-1.png)<!-- -->

Decreasing ![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma
"\\gamma") does not improve the model performance. Prediction are almost
the same whatever the specification.

## Conclusion

Changing ![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma
"\\gamma") does not improve the results. Relative bargaining power has a
small impact in the model prediction. However, it may mean that the
outside option is the key determinant to improve model predictions.
