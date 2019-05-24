Capital-Labor elasticity of substitution
================
Fabien Petit
24/05/2019

## Methodology

I estimate the elasticity of substitution between capital and labor
using the methodology of Leon-Ledesma, McAdam & Willman (2013).
Estimation are done for France and United States between 1970 and 2010.

## Estimation (PWT only)

``` r
pwt = read.csv(file.path(loc_final, "pwt.csv"), header = TRUE) %>% 
  subset(Country %in% c("France", "United States") & Year %in% c(1970:2010) & i_labsh2 == 1) %>% 
  select("Country", "Year", "pop", "emp", "avh", "rnna", "labsh")

head(pwt)
```

    ##     Country Year      pop      emp      avh    rnna     labsh
    ## 837  France 1970 52.03510 21.36307 1948.230 3696722 0.6761946
    ## 838  France 1971 52.47982 21.43453 1947.817 3922977 0.6750869
    ## 839  France 1972 52.95780 21.57299 1898.091 4159604 0.6782192
    ## 840  France 1973 53.43918 21.88949 1884.460 4411834 0.6672299
    ## 841  France 1974 53.88050 22.12623 1854.751 4662053 0.6747267
    ## 842  France 1975 54.25202 22.00547 1830.153 4879367 0.6988190

The variables from the **Penn World Table 9.1** are the following :

  - *pop* : Population (in millions.)
  - *emp* : Number of persons engaged (in millions)
  - *avh* : Average annual hours worked by persons engaged
  - *rnna* : Capital stock at constant 2011 national prices (in mil.
    2011US$)
  - *labsh* : Share of labour compensation in GDP at current national
    prices

Following the methodology of Leon-Ledesma, McAdam & Willman (2013), I
estimate the following equation :

  
![\\ln k\_t = \\alpha - \\frac{\\sigma}{1-\\sigma} \\ln \\Theta\_t +
\\left( a\_L-a\_K\\right)t +
\\varepsilon\_t](https://latex.codecogs.com/png.latex?%5Cln%20k_t%20%3D%20%5Calpha%20-%20%5Cfrac%7B%5Csigma%7D%7B1-%5Csigma%7D%20%5Cln%20%5CTheta_t%20%2B%20%5Cleft%28%20a_L-a_K%5Cright%29t%20%2B%20%5Cvarepsilon_t
"\\ln k_t = \\alpha - \\frac{\\sigma}{1-\\sigma} \\ln \\Theta_t + \\left( a_L-a_K\\right)t + \\varepsilon_t")  

Therefore, I need to compute the capital-to-labor ratio
![\~k\_t\~](https://latex.codecogs.com/png.latex?~k_t~ "~k_t~") and the
capital-to-labor income ratio
![\~\\Theta\_t\~](https://latex.codecogs.com/png.latex?~%5CTheta_t~
"~\\Theta_t~").

``` r
# Capital in the economy (K)
pwt$K = pwt$rnna
# Capital in the economy (K) // Corrected for hours worked
pwt$K.avh_correct = pwt$rnna / pwt$avh

# Capital-to-labor ratio (k)
pwt$k = pwt$K/pwt$emp
# Capital-to-labor ratio (k) // Corrected for hours workers
pwt$k.avh_correct = pwt$K.avh_correct/pwt$emp

# Capital-to-labor income ratio (THETA)
pwt$THETA = (1-pwt$labsh)/pwt$labsh
```

I normalize the capital-to-labor ratio to the initial year (i.e. 1970).

``` r
pwt = pwt %>% 
  group_by(Country) %>% 
  mutate(k_nor = k / first(k),
         k_nor.avh_correct = k.avh_correct / first(k.avh_correct)) %>% 
  ungroup()
```

``` r
pwt %>% select("Country", "Year", "k_nor", "k_nor.avh_correct") %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  ggplot(aes(x = Year, y = value, color = variable)) +
  geom_line(size = .5) +
  facet_wrap(Country ~ ., dir = "h") +
  theme_classic(base_size = 14) +
  labs(x = "Year", y = "")
```

![](sigma_files/figure-gfm/Plot%20k-1.png)<!-- -->

``` r
# Variable modification for estimation
pwt = pwt %>% 
  group_by(Country) %>% 
  mutate(k_nor_log = log(k_nor),
         k_nor_log.avh_correct = log(k_nor.avh_correct),
         THETA_log_neg = -log(THETA),
         t = Year - first(Year)) %>%
  ungroup()
```

``` r
ols0 = pwt %>% 
  lm(formula = k_nor_log ~ Country + THETA_log_neg*Country - THETA_log_neg)

summary(ols0)
```

    ## 
    ## Call:
    ## lm(formula = k_nor_log ~ Country + THETA_log_neg * Country - 
    ##     THETA_log_neg, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.41474 -0.08709 -0.00268  0.10112  0.26718 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                         1.44122    0.09775  14.744  < 2e-16
    ## CountryUnited States               -0.52995    0.20642  -2.567 0.012162
    ## CountryFrance:THETA_log_neg        -1.39403    0.15332  -9.092 7.16e-14
    ## CountryUnited States:THETA_log_neg -1.40216    0.37546  -3.734 0.000356
    ##                                       
    ## (Intercept)                        ***
    ## CountryUnited States               *  
    ## CountryFrance:THETA_log_neg        ***
    ## CountryUnited States:THETA_log_neg ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1402 on 78 degrees of freedom
    ## Multiple R-squared:  0.7343, Adjusted R-squared:  0.7241 
    ## F-statistic: 71.85 on 3 and 78 DF,  p-value: < 2.2e-16

``` r
beta = summary(ols0)$coefficients[c(3,4)]
sigma = 1/(1+beta)
names(sigma) = c("France", "United States")

sigma
```

    ##        France United States 
    ##     -2.537900     -2.486578

``` r
ols.btc = pwt %>% 
  lm(formula = k_nor_log ~ Country + THETA_log_neg*Country + Country*t - THETA_log_neg - t)

summary(ols.btc)
```

    ## 
    ## Call:
    ## lm(formula = k_nor_log ~ Country + THETA_log_neg * Country + 
    ##     Country * t - THETA_log_neg - t, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.174790 -0.026421 -0.001208  0.036636  0.082635 
    ## 
    ## Coefficients:
    ##                                      Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                         0.1932403  0.0837483   2.307   0.0238
    ## CountryUnited States               -0.2020487  0.1215290  -1.663   0.1005
    ## CountryFrance:THETA_log_neg        -0.0250573  0.1000815  -0.250   0.8030
    ## CountryUnited States:THETA_log_neg -0.0118556  0.1621909  -0.073   0.9419
    ## CountryFrance:t                     0.0198701  0.0012083  16.444   <2e-16
    ## CountryUnited States:t              0.0125877  0.0007997  15.741   <2e-16
    ##                                       
    ## (Intercept)                        *  
    ## CountryUnited States                  
    ## CountryFrance:THETA_log_neg           
    ## CountryUnited States:THETA_log_neg    
    ## CountryFrance:t                    ***
    ## CountryUnited States:t             ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05081 on 76 degrees of freedom
    ## Multiple R-squared:  0.966,  Adjusted R-squared:  0.9638 
    ## F-statistic: 432.1 on 5 and 76 DF,  p-value: < 2.2e-16

``` r
beta.btc = summary(ols.btc)$coefficients[c(3,4)]
sigma.btc = 1/(1+beta.btc)
names(sigma.btc) = c("France", "United States")

sigma.btc
```

    ##        France United States 
    ##      1.025701      1.011998

``` r
ols.avh = pwt %>% 
  lm(formula = k_nor_log.avh_correct ~ Country + THETA_log_neg*Country - THETA_log_neg)

summary(ols.avh)
```

    ## 
    ## Call:
    ## lm(formula = k_nor_log.avh_correct ~ Country + THETA_log_neg * 
    ##     Country - THETA_log_neg, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.52675 -0.08318 -0.00526  0.12810  0.28843 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                          1.8745     0.1163  16.114  < 2e-16
    ## CountryUnited States                -0.7922     0.2457  -3.225 0.001842
    ## CountryFrance:THETA_log_neg         -1.8303     0.1825 -10.031  1.1e-15
    ## CountryUnited States:THETA_log_neg  -1.6625     0.4468  -3.721 0.000373
    ##                                       
    ## (Intercept)                        ***
    ## CountryUnited States               ** 
    ## CountryFrance:THETA_log_neg        ***
    ## CountryUnited States:THETA_log_neg ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1669 on 78 degrees of freedom
    ## Multiple R-squared:  0.7734, Adjusted R-squared:  0.7647 
    ## F-statistic: 88.75 on 3 and 78 DF,  p-value: < 2.2e-16

``` r
beta.avh = summary(ols.avh)$coefficients[c(3,4)]
sigma.avh = 1/(1+beta.avh)
names(sigma.avh) = c("France", "United States")

sigma.avh
```

    ##        France United States 
    ##     -1.204377     -1.509395

``` r
ols.avh.btc = pwt %>% 
  lm(formula = k_nor_log.avh_correct ~ Country + THETA_log_neg*Country + t*Country - THETA_log_neg -t)

summary(ols.avh.btc)
```

    ## 
    ## Call:
    ## lm(formula = k_nor_log.avh_correct ~ Country + THETA_log_neg * 
    ##     Country + t * Country - THETA_log_neg - t, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.213872 -0.028649  0.006386  0.033605  0.121701 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                         0.247221   0.107446   2.301   0.0241
    ## CountryUnited States               -0.101424   0.155917  -0.650   0.5173
    ## CountryFrance:THETA_log_neg        -0.045291   0.128401  -0.353   0.7253
    ## CountryUnited States:THETA_log_neg -0.247457   0.208085  -1.189   0.2381
    ## CountryFrance:t                     0.025909   0.001550  16.713   <2e-16
    ## CountryUnited States:t              0.012812   0.001026  12.488   <2e-16
    ##                                       
    ## (Intercept)                        *  
    ## CountryUnited States                  
    ## CountryFrance:THETA_log_neg           
    ## CountryUnited States:THETA_log_neg    
    ## CountryFrance:t                    ***
    ## CountryUnited States:t             ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06519 on 76 degrees of freedom
    ## Multiple R-squared:  0.9663, Adjusted R-squared:  0.9641 
    ## F-statistic: 436.1 on 5 and 76 DF,  p-value: < 2.2e-16

``` r
beta.avh.btc = summary(ols.avh.btc)$coefficients[c(3,4)]
sigma.avh.btc = 1/(1+beta.avh.btc)
names(sigma.avh.btc) = c("France", "United States")

sigma.avh.btc
```

    ##        France United States 
    ##      1.047439      1.328828
