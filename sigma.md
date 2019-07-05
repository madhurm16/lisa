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
  subset(Country %in% country_set &
           Year %in% c(1970:2010) & i_labsh2 == 1) %>%
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
## THETA 1

# Regression : no control
ols1.no = pwt %>%
  lm(formula = k_nor_log ~ Country + THETA1_log_neg*Country - THETA1_log_neg -1)
# Robust standard errors
ols1.no = robustify(ols1.no) %>% 
  summary()
# Compute the associated sigma
pwt1.no = 1/(1 + ols1.no$coefficients[c((length(country_set)+1):(2*length(country_set)))])
# Visualize summary
ols1.no
```

    ## 
    ## Call:  lm(formula = k_nor_log ~ Country + THETA1_log_neg * Country - 
    ##     THETA1_log_neg - 1, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                     Estimate Std. Error t value Pr(>|t|)
    ## CountryFrance                        1.57274    0.07947  19.790  < 2e-16
    ## CountryUnited States                 0.83590    0.22348   3.740 0.000349
    ## CountryFrance:THETA1_log_neg        -1.28509    0.11724 -10.962  < 2e-16
    ## CountryUnited States:THETA1_log_neg -1.00791    0.37549  -2.684 0.008874
    ##                                        
    ## CountryFrance                       ***
    ## CountryUnited States                ***
    ## CountryFrance:THETA1_log_neg        ***
    ## CountryUnited States:THETA1_log_neg ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## THETA 2

# Regression : no control
ols2.no = pwt %>%
  lm(formula = k_nor_log ~ Country + THETA2_log_neg*Country - THETA2_log_neg -1)
# Robust standard errors
ols2.no = robustify(ols2.no) %>% 
  summary()
# Compute the associated sigma
pwt2.no = 1/(1 + ols2.no$coefficients[c((length(country_set)+1):(2*length(country_set)))])
# Visualize summary
ols2.no
```

    ## 
    ## Call:  lm(formula = k_nor_log ~ Country + THETA2_log_neg * Country - 
    ##     THETA2_log_neg - 1, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                     Estimate Std. Error t value Pr(>|t|)
    ## CountryFrance                        1.44122    0.07978  18.065  < 2e-16
    ## CountryUnited States                 0.91127    0.17351   5.252 1.27e-06
    ## CountryFrance:THETA2_log_neg        -1.39403    0.14969  -9.313 2.67e-14
    ## CountryUnited States:THETA2_log_neg -1.40216    0.36154  -3.878 0.000218
    ##                                        
    ## CountryFrance                       ***
    ## CountryUnited States                ***
    ## CountryFrance:THETA2_log_neg        ***
    ## CountryUnited States:THETA2_log_neg ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### BTC control

``` r
## THETA 1

# Regression : control only for biased technical change
ols1.btc = pwt %>%
  lm(formula = k_nor_log ~ Country + THETA1_log_neg*Country + Country*t - THETA1_log_neg -1 - t)
# Robust standard errors
ols1.btc = robustify(ols1.btc) %>%
  summary()
# Compute the associated sigma
pwt1.btc = 1/(1 + ols1.btc$coefficients[c((length(country_set)+1):(2*length(country_set)))])
# Visualize summary
ols1.btc
```

    ## 
    ## Call:  lm(formula = k_nor_log ~ Country + THETA1_log_neg * Country + 
    ##     Country * t - THETA1_log_neg - 1 - t, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                       Estimate Std. Error t value Pr(>|t|)
    ## CountryFrance                        0.3587716  0.1301944   2.756  0.00733
    ## CountryUnited States                -0.0436422  0.0343471  -1.271  0.20774
    ## CountryFrance:THETA1_log_neg        -0.1970320  0.1138428  -1.731  0.08756
    ## CountryUnited States:THETA1_log_neg  0.0246993  0.0568636   0.434  0.66526
    ## CountryFrance:t                      0.0175840  0.0020540   8.561 9.26e-13
    ## CountryUnited States:t               0.0126770  0.0003109  40.781  < 2e-16
    ##                                        
    ## CountryFrance                       ** 
    ## CountryUnited States                   
    ## CountryFrance:THETA1_log_neg        .  
    ## CountryUnited States:THETA1_log_neg    
    ## CountryFrance:t                     ***
    ## CountryUnited States:t              ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## THETA 2

# Regression : control only for biased technical change
ols2.btc = pwt %>%
  lm(formula = k_nor_log ~ Country + THETA2_log_neg*Country + Country*t - THETA2_log_neg -1 - t)
# Robust standard errors
ols2.btc = robustify(ols2.btc) %>%
  summary()
# Compute the associated sigma
pwt2.btc = 1/(1 + ols2.btc$coefficients[c((length(country_set)+1):(2*length(country_set)))])
# Visualize summary
ols2.btc
```

    ## 
    ## Call:  lm(formula = k_nor_log ~ Country + THETA2_log_neg * Country + 
    ##     Country * t - THETA2_log_neg - 1 - t, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                       Estimate Std. Error t value Pr(>|t|)
    ## CountryFrance                        0.1733702  0.1268525   1.367    0.176
    ## CountryUnited States                -0.0213961  0.0349947  -0.611    0.543
    ## CountryFrance:THETA2_log_neg        -0.0250573  0.1304404  -0.192    0.848
    ## CountryUnited States:THETA2_log_neg -0.0118556  0.0694929  -0.171    0.865
    ## CountryFrance:t                      0.0198701  0.0020821   9.543 1.22e-14
    ## CountryUnited States:t               0.0125877  0.0002966  42.437  < 2e-16
    ##                                        
    ## CountryFrance                          
    ## CountryUnited States                   
    ## CountryFrance:THETA2_log_neg           
    ## CountryUnited States:THETA2_log_neg    
    ## CountryFrance:t                     ***
    ## CountryUnited States:t              ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### AVH control

``` r
## THETA 1

# Regression : control only for hours worked
ols1.avh = pwt %>%
  lm(formula = k_nor_log.avh_correct ~ Country + THETA1_log_neg*Country - THETA1_log_neg -1)
# Robust standard errors
ols1.avh = robustify(ols1.avh) %>% 
  summary()
# Compute the associated sigma
pwt1.avh = 1/(1 + ols1.avh$coefficients[c((length(country_set)+1):(2*length(country_set)))])
# Visualize summary
ols1.avh
```

    ## 
    ## Call:  lm(formula = k_nor_log.avh_correct ~ Country + THETA1_log_neg * 
    ##     Country - THETA1_log_neg - 1, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                     Estimate Std. Error t value Pr(>|t|)
    ## CountryFrance                         2.0451     0.1022  20.005  < 2e-16
    ## CountryUnited States                  1.0232     0.2378   4.303 4.84e-05
    ## CountryFrance:THETA1_log_neg         -1.6846     0.1504 -11.200  < 2e-16
    ## CountryUnited States:THETA1_log_neg  -1.2461     0.3996  -3.118  0.00255
    ##                                        
    ## CountryFrance                       ***
    ## CountryUnited States                ***
    ## CountryFrance:THETA1_log_neg        ***
    ## CountryUnited States:THETA1_log_neg ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## THETA 2

# Regression : control only for hours worked
ols2.avh = pwt %>%
  lm(formula = k_nor_log.avh_correct ~ Country + THETA2_log_neg*Country - THETA2_log_neg -1)
# Robust standard errors
ols2.avh = robustify(ols2.avh) %>% 
  summary()
# Compute the associated sigma
pwt2.avh = 1/(1 + ols2.avh$coefficients[c((length(country_set)+1):(2*length(country_set)))])
# Visualize summary
ols2.avh
```

    ## 
    ## Call:  lm(formula = k_nor_log.avh_correct ~ Country + THETA2_log_neg * 
    ##     Country - THETA2_log_neg - 1, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                     Estimate Std. Error t value Pr(>|t|)
    ## CountryFrance                         1.8745     0.1034  18.124  < 2e-16
    ## CountryUnited States                  1.0823     0.1828   5.922 8.11e-08
    ## CountryFrance:THETA2_log_neg         -1.8303     0.1937  -9.451 1.44e-14
    ## CountryUnited States:THETA2_log_neg  -1.6625     0.3809  -4.364 3.87e-05
    ##                                        
    ## CountryFrance                       ***
    ## CountryUnited States                ***
    ## CountryFrance:THETA2_log_neg        ***
    ## CountryUnited States:THETA2_log_neg ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### AVH/BTC controls

``` r
## THETA 1

# Regression : control for both hours worked and biased technical change
ols1.avh.btc = pwt %>%
  lm(formula = k_nor_log.avh_correct ~ Country + THETA1_log_neg*Country + t*Country - THETA1_log_neg -1 - t)
# Robust standard errors
ols1.avh.btc = robustify(ols1.avh.btc) %>% 
  summary()
# Compute the associated sigma
pwt1.avh.btc = 1/(1 + ols1.avh.btc$coefficients[c((length(country_set)+1):(2*length(country_set)))])
# Visualize summary
ols1.avh.btc
```

    ## 
    ## Call:  lm(formula = k_nor_log.avh_correct ~ Country + THETA1_log_neg * 
    ##     Country + t * Country - THETA1_log_neg - 1 - t, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                       Estimate Std. Error t value Pr(>|t|)
    ## CountryFrance                        0.4582565  0.1684809   2.720  0.00809
    ## CountryUnited States                 0.1178156  0.0446615   2.638  0.01011
    ## CountryFrance:THETA1_log_neg        -0.2623562  0.1475026  -1.779  0.07930
    ## CountryUnited States:THETA1_log_neg -0.1831404  0.0712163  -2.572  0.01207
    ## CountryFrance:t                      0.0229846  0.0026920   8.538 1.03e-12
    ## CountryUnited States:t               0.0130495  0.0003861  33.794  < 2e-16
    ##                                        
    ## CountryFrance                       ** 
    ## CountryUnited States                *  
    ## CountryFrance:THETA1_log_neg        .  
    ## CountryUnited States:THETA1_log_neg *  
    ## CountryFrance:t                     ***
    ## CountryUnited States:t              ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## THETA 2

# Regression : control for both hours worked and biased technical change
ols2.avh.btc = pwt %>%
  lm(formula = k_nor_log.avh_correct ~ Country + THETA2_log_neg*Country + t*Country - THETA2_log_neg -1 - t)
# Robust standard errors
ols2.avh.btc = robustify(ols2.avh.btc) %>% 
  summary()
# Compute the associated sigma
pwt2.avh.btc = 1/(1 + ols2.avh.btc$coefficients[c((length(country_set)+1):(2*length(country_set)))])
# Visualize summary
ols2.avh.btc
```

    ## 
    ## Call:  lm(formula = k_nor_log.avh_correct ~ Country + THETA2_log_neg * 
    ##     Country + t * Country - THETA2_log_neg - 1 - t, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                                       Estimate Std. Error t value Pr(>|t|)
    ## CountryFrance                        0.2213121  0.1634811   1.354  0.17983
    ## CountryUnited States                 0.1329850  0.0450348   2.953  0.00419
    ## CountryFrance:THETA2_log_neg        -0.0452907  0.1692721  -0.268  0.78976
    ## CountryUnited States:THETA2_log_neg -0.2474569  0.0857874  -2.885  0.00510
    ## CountryFrance:t                      0.0259088  0.0027096   9.562 1.13e-14
    ## CountryUnited States:t               0.0128118  0.0003718  34.457  < 2e-16
    ##                                        
    ## CountryFrance                          
    ## CountryUnited States                ** 
    ## CountryFrance:THETA2_log_neg           
    ## CountryUnited States:THETA2_log_neg ** 
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
for France and 1.224 for United States (see table below). With the
second adjustment method, estimates are still biased toward 1 when I do
not control for AVH. However, once done, the elasticity is close to 1
for France (i.e. 1.047) and larger for the US (i.e. 1.329).

    ##          Country data btc avh adj       value
    ## 1         France  pwt   0   0   1   -3.507639
    ## 2  United States  pwt   0   0   1 -126.346016
    ## 3         France  pwt   1   0   1    1.245380
    ## 4  United States  pwt   1   0   1    0.975896
    ## 5         France  pwt   0   1   1   -1.460724
    ## 6  United States  pwt   0   1   1   -4.063427
    ## 7         France  pwt   1   1   1    1.355668
    ## 8  United States  pwt   1   1   1    1.224201
    ## 9         France  pwt   0   0   2   -2.537900
    ## 10 United States  pwt   0   0   2   -2.486578
    ## 11        France  pwt   1   0   2    1.025701
    ## 12 United States  pwt   1   0   2    1.011998
    ## 13        France  pwt   0   1   2   -1.204377
    ## 14 United States  pwt   0   1   2   -1.509395
    ## 15        France  pwt   1   1   2    1.047439
    ## 16 United States  pwt   1   1   2    1.328828

## Break Year

The purpose of this section is to investigate whether there is a break
in the estimated elasticity of substitution. In particular for France.
Indeed, there capital-per-worker
![k\_t](https://latex.codecogs.com/png.latex?k_t "k_t") grows at a
relatively constant rate while the labor share
![\\Theta\_t](https://latex.codecogs.com/png.latex?%5CTheta_t
"\\Theta_t") faces a huge decrease during the 80’s.

I consider only France with adjustment method 1 and capital with average
hours worked correction. Estimation is done with biased technical
change.

This is the baseline regression, where I estimate the capital-labor
elasticity of substitution for the whole sample.

    ## 
    ## Call:  lm(formula = k_nor_log ~ THETA_log_neg + t, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.458257   0.168481   2.720  0.00979 ** 
    ## THETA_log_neg -0.262356   0.147503  -1.779  0.08330 .  
    ## t              0.022985   0.002692   8.538 2.28e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

There is a biased technical change, which is constant and significant. I
have to keep the same rate of technical change. Because if I split the
sample, I would obtain two different BTC growth rates. Thus, a way is to
detrend the capital-per-worker and the capital-to-labor income ratio.
Such a methodology comes from the **Frisch–Waugh–Lovell theorem**.

![](sigma_files/figure-gfm/Break%20Year%20-%20Detrend-1.png)<!-- -->

This is an example of the regressions when I break the sample in
1985.

![](sigma_files/figure-gfm/Break%20Year%20-%20Plot%20regimes-1.png)<!-- -->

It seems that there are two regimes. There are three main potential
breaks around the years 1983, 1985 and 1989.

    ## # A tibble: 3 x 7
    ##   Country  Year k_nor_log THETA_log_neg     t k_nor_log_detre~
    ##   <fct>   <int>     <dbl>         <dbl> <dbl>            <dbl>
    ## 1 France   1982     0.614         1.01     13           0.0873
    ## 2 France   1985     0.725         0.864    16           0.120 
    ## 3 France   1989     0.789         0.648    20           0.0782
    ## # ... with 1 more variable: THETA_log_neg_detrend <dbl>

However, the break may have occur during the 80’s. I have to determine
which year optimize the split. To do so, I use a grid-search approach
between 1980 and 1990. Below is the baseline regression leading to an
elasticity of 1.356.

    ## 
    ## Call:  lm(formula = k_nor_log_detrend ~ THETA_log_neg_detrend - 1, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##                       Estimate Std. Error t value Pr(>|t|)  
    ## THETA_log_neg_detrend  -0.2624     0.1475  -1.779   0.0829 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

I run a regression for each subsample, the subsample are separated with
a break year between 1980 and 1990. Then, I repeat this for each break
year on this interval. I report on the graph below the p-values of both
regressions according to the break year considered. I only show those
where both p-values do not exceed the one of the baseline
regression.

![](sigma_files/figure-gfm/Break%20Year%20-%20Split%20loop-1.png)<!-- -->

Therefore, all the break years above are candidate. However, there is
another condition within the model that ensure a positive wage and
capital-to-labor ratio at the equilibrium. This condition is that ![−
\\log^{-1}(x\_t) \>
\\sigma](https://latex.codecogs.com/png.latex?%E2%88%92%20%5Clog%5E%7B-1%7D%28x_t%29%20%3E%20%5Csigma
"− \\log^{-1}(x_t) \> \\sigma") where
![x\_t](https://latex.codecogs.com/png.latex?x_t "x_t") is the
unemployment replacement rate at time
![t](https://latex.codecogs.com/png.latex?t "t") from data. As long as
![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma "\\sigma") is
lower than 1, this condition is satisfied. However, the issue raises
when ![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma "\\sigma")
is greater than one and thus after the break year. So I also have to
filter the ![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma
"\\sigma") candidates with this condition. For more details about this
condition, see the file
    *xsigmacdt.md*.

    ##   splitter sigma_before    p_before sigma_after      p_after x_min
    ## 1     1982    0.6848343 0.057640639    2.283375 2.258880e-03 0.660
    ## 2     1983    0.6657488 0.018686477    3.247766 1.136058e-05 0.660
    ## 3     1984    0.6452102 0.008146204    5.026342 4.570342e-10 0.688
    ## 4     1985    0.6316941 0.005723918    6.237710 5.744035e-11 0.688
    ## 5     1986    0.6219518 0.004476779    7.248446 4.350550e-11 0.688
    ## 6     1987    0.6506501 0.010529795    6.145562 3.110334e-11 0.688
    ## 7     1988    0.6920947 0.034369421    5.556700 6.319983e-11 0.688
    ##   sigma_bar xcdt_ok
    ## 1  2.406649    TRUE
    ## 2  2.406649   FALSE
    ## 3  2.674037   FALSE
    ## 4  2.674037   FALSE
    ## 5  2.674037   FALSE
    ## 6  2.674037   FALSE
    ## 7  2.674037   FALSE

Hence, only the break in 1982 complies with the condition to ensure an
equilibrium with positive wage and capital-to-labor ratio. Thus, instead
of using a unique capital-labor elasticity of substitution of 1.356, I
can consider that its value is **0.685** for 1970 and 1980, then
**2.283**.
