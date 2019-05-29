Capital-Labor elasticity of substitution
================
Fabien Petit
24/05/2019

## Methodology

I estimate the elasticity of substitution between capital and labor
using the methodology of **Leon-Ledesma, McAdam & Willman (2013)**.
Estimation are done for France and United States between 1970 and 2010,
using Penn World Table 9.1 data.

Following the methodology of Leon-Ledesma, McAdam & Willman (2013), I
estimate the following equation :

<center>

  
![\\ln k\_t = \\alpha - \\frac{\\sigma}{1-\\sigma} \\ln \\Theta\_t +
\\left( a\_L-a\_K\\right)t +
\\varepsilon\_t](https://latex.codecogs.com/png.latex?%5Cln%20k_t%20%3D%20%5Calpha%20-%20%5Cfrac%7B%5Csigma%7D%7B1-%5Csigma%7D%20%5Cln%20%5CTheta_t%20%2B%20%5Cleft%28%20a_L-a_K%5Cright%29t%20%2B%20%5Cvarepsilon_t
"\\ln k_t = \\alpha - \\frac{\\sigma}{1-\\sigma} \\ln \\Theta_t + \\left( a_L-a_K\\right)t + \\varepsilon_t")  

</center>

Therefore, I need to compute the capital-to-labor ratio
![\~k\_t\~](https://latex.codecogs.com/png.latex?~k_t~ "~k_t~") and the
capital-to-labor income ratio
![\~\\Theta\_t\~](https://latex.codecogs.com/png.latex?~%5CTheta_t~
"~\\Theta_t~").

## Data

``` r
pwt = read.csv(file.path(loc_final, "pwt.csv"), header = TRUE) %>%
  subset(Country %in% c("France", "United States") & Year %in% c(1970:2010) & i_labsh2 == 1) %>%
  select("Country", "Year", "pop", "emp", "avh", "rnna", "lab_sh1", "lab_sh2")

head(pwt)
```

    ##     Country Year      pop      emp      avh    rnna   lab_sh1   lab_sh2
    ## 837  France 1970 52.03510 21.36307 1948.230 3696722 0.7297512 0.6761946
    ## 838  France 1971 52.47982 21.43453 1947.817 3922977 0.7260424 0.6750869
    ## 839  France 1972 52.95780 21.57299 1898.091 4159604 0.7294431 0.6782192
    ## 840  France 1973 53.43918 21.88949 1884.460 4411834 0.7170317 0.6672299
    ## 841  France 1974 53.88050 22.12623 1854.751 4662053 0.7199504 0.6747267
    ## 842  France 1975 54.25202 22.00547 1830.153 4879367 0.7383997 0.6988190

The variables from the **Penn World Table 9.1** are the following :

  - *pop* : Population (in millions.)
  - *emp* : Number of persons engaged (in millions)  
  - *avh* : Average annual hours worked by persons engaged  
  - *rnna* : Capital stock at constant 2011 national prices (in mil.
    2011US$)
  - *lab\_sh1* : Share of labour compensation in GDP at current national
    prices (**Adjustment method 1**)
  - *lab\_sh2* : Share of labour compensation in GDP at current national
    prices (**Adjustment method 2**)

![](sigma_files/figure-gfm/Compare%20both%20labor%20share-1.png)<!-- -->

I use the first adjustment method (see **Frenstra, Inklaar and Timmer
2015** and **Gollin 2002** for more details). An adjustment method is
required to take into account self-employed income.

**Adjustment 1** adds mixed income (MIX) to the compensation of
employees (COMP). Thus,

<center>

  
![LS =
\\frac{COMP+MIX}{GDP}](https://latex.codecogs.com/png.latex?LS%20%3D%20%5Cfrac%7BCOMP%2BMIX%7D%7BGDP%7D
"LS = \\frac{COMP+MIX}{GDP}")  

</center>

**Adjustment 2** assumes the same labor share for mixed income as for
the rest of the economy. Thus,

<center>

  
![LS =
\\frac{COMP}{GDP-MIX}](https://latex.codecogs.com/png.latex?LS%20%3D%20%5Cfrac%7BCOMP%7D%7BGDP-MIX%7D
"LS = \\frac{COMP}{GDP-MIX}")  

</center>

In the model, workers are only young individuals and provide only labor
supply. Therefore, I assume that self-employed people earn an income
that is characterized as a compensation. Hence, I will use the first
adjustment method.

I normalize the capital-to-labor ratio to the initial year (i.e. 1970).

![](sigma_files/figure-gfm/Plot%20k_nor-1.png)<!-- -->

![](sigma_files/figure-gfm/Log%20variables-1.png)<!-- -->

## Estimation

I estimate four versions of the elasticity of substitution :

  - Without any control
  - Control for biased technical change (BTC)
  - Control for average hours worked (AVH)
  - Control for BTC and AVH

### No controls

``` r
# Regression : no control
ols0.no = pwt %>%
  lm(formula = k_nor_log ~ Country + THETA1_log_neg*Country - THETA1_log_neg)
# Robust standard errors
ols0.no = robustify(ols0.no) %>% 
  summary()
# Compute the associated sigma
pwt.no = 1/(1 + ols0.no$coefficients[c(3,4)])
# Visualize summary
ols0.no
```

    ## 
    ## Call:  lm(formula = k_nor_log ~ Country + THETA1_log_neg * Country - 
    ##     THETA1_log_neg, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                     Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                          1.57274    0.07947  19.790  < 2e-16
    ## CountryUnited States                -0.73683    0.23719  -3.106  0.00264
    ## CountryFrance:THETA1_log_neg        -1.28509    0.11724 -10.962  < 2e-16
    ## CountryUnited States:THETA1_log_neg -1.00791    0.37549  -2.684  0.00887
    ##                                        
    ## (Intercept)                         ***
    ## CountryUnited States                ** 
    ## CountryFrance:THETA1_log_neg        ***
    ## CountryUnited States:THETA1_log_neg ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### BTC control

``` r
# Regression : control only for biased technical change
ols0.btc = pwt %>%
  lm(formula = k_nor_log ~ Country + THETA1_log_neg*Country + Country*t - THETA1_log_neg - t)
# Robust standard errors
ols0.btc = robustify(ols0.btc) %>%
  summary()
# Compute the associated sigma
pwt.btc = 1/(1 + ols0.btc$coefficients[c(3,4)])
# Visualize summary
ols0.btc
```

    ## 
    ## Call:  lm(formula = k_nor_log ~ Country + THETA1_log_neg * Country + 
    ##     Country * t - THETA1_log_neg - t, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                       Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                          0.3587716  0.1301944   2.756  0.00733
    ## CountryUnited States                -0.4024138  0.1346488  -2.989  0.00377
    ## CountryFrance:THETA1_log_neg        -0.1970320  0.1138428  -1.731  0.08756
    ## CountryUnited States:THETA1_log_neg  0.0246993  0.0568636   0.434  0.66526
    ## CountryFrance:t                      0.0175840  0.0020540   8.561 9.26e-13
    ## CountryUnited States:t               0.0126770  0.0003109  40.781  < 2e-16
    ##                                        
    ## (Intercept)                         ** 
    ## CountryUnited States                ** 
    ## CountryFrance:THETA1_log_neg        .  
    ## CountryUnited States:THETA1_log_neg    
    ## CountryFrance:t                     ***
    ## CountryUnited States:t              ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### AVH control

``` r
# Regression : control only for hours worked
ols0.avh = pwt %>%
  lm(formula = k_nor_log.avh_correct ~ Country + THETA1_log_neg*Country - THETA1_log_neg)
# Robust standard errors
ols0.avh = robustify(ols0.avh) %>% 
  summary()
# Compute the associated sigma
pwt.avh = 1/(1 + ols0.avh$coefficients[c(3,4)])
# Visualize summary
ols0.avh
```

    ## 
    ## Call:  lm(formula = k_nor_log.avh_correct ~ Country + THETA1_log_neg * 
    ##     Country - THETA1_log_neg, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                     Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                           2.0451     0.1022  20.005  < 2e-16
    ## CountryUnited States                 -1.0219     0.2588  -3.948 0.000172
    ## CountryFrance:THETA1_log_neg         -1.6846     0.1504 -11.200  < 2e-16
    ## CountryUnited States:THETA1_log_neg  -1.2461     0.3996  -3.118 0.002550
    ##                                        
    ## (Intercept)                         ***
    ## CountryUnited States                ***
    ## CountryFrance:THETA1_log_neg        ***
    ## CountryUnited States:THETA1_log_neg ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### AVH/BTC controls

``` r
# Regression : control for both hours worked and biased technical change
ols0.avh.btc = pwt %>%
  lm(formula = k_nor_log.avh_correct ~ Country + THETA1_log_neg*Country + t*Country - THETA1_log_neg - t)
# Robust standard errors
ols0.avh.btc = robustify(ols0.avh.btc) %>% 
  summary()
# Compute the associated sigma
pwt.avh.btc = 1/(1 + ols0.avh.btc$coefficients[c(3,4)])
# Visualize summary
ols0.avh.btc
```

    ## 
    ## Call:  lm(formula = k_nor_log.avh_correct ~ Country + THETA1_log_neg * 
    ##     Country + t * Country - THETA1_log_neg - t, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                       Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                          0.4582565  0.1684809   2.720  0.00809
    ## CountryUnited States                -0.3404410  0.1742999  -1.953  0.05448
    ## CountryFrance:THETA1_log_neg        -0.2623562  0.1475026  -1.779  0.07930
    ## CountryUnited States:THETA1_log_neg -0.1831404  0.0712163  -2.572  0.01207
    ## CountryFrance:t                      0.0229846  0.0026920   8.538 1.03e-12
    ## CountryUnited States:t               0.0130495  0.0003861  33.794  < 2e-16
    ##                                        
    ## (Intercept)                         ** 
    ## CountryUnited States                .  
    ## CountryFrance:THETA1_log_neg        .  
    ## CountryUnited States:THETA1_log_neg *  
    ## CountryFrance:t                     ***
    ## CountryUnited States:t              ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Summary of the results

I gather all estimated
![\\hat{\\sigma}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Csigma%7D
"\\hat{\\sigma}"). Without control for biased technical change (BTC), I
obtain negative
![\\hat{\\sigma}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Csigma%7D
"\\hat{\\sigma}"), which is not possible. Once I control for BTC, I
obtain a positive elasticity without control on the average hours worked
(AVH). United States elasticity is below one but not significantly
different from 1. This case is the Cobb-Douglas specification. However,
without correcting by hours worked I generate a bias toward Cobb-Douglas
for both countries. Finally, on the last estimate, with BTC and AVH
controls, I obtain a capital-labor elasticity of substitution of 1.356
for France and 1.224 for United States (see table below).

    ##         Country data btc avh       value
    ## 1        France  pwt   0   0   -3.507639
    ## 2 United States  pwt   0   0 -126.346016
    ## 3        France  pwt   1   0    1.245380
    ## 4 United States  pwt   1   0    0.975896
    ## 5        France  pwt   0   1   -1.460724
    ## 6 United States  pwt   0   1   -4.063427
    ## 7        France  pwt   1   1    1.355668
    ## 8 United States  pwt   1   1    1.224201
