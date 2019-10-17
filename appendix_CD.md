Appendix CD
================
Fabien Petit
17/10/2019

This notebook refers to the Appendix C **“Estimation of σ”** and the
Appendix D **“Two σ regimes in France”**. All the graphs used in these
appendices are generated in this notebook.

``` r
# Load packages
packages <- c("dplyr", "ggplot2", "RColorBrewer", "reshape2", "bucky", "sandwich")
lapply(packages, require, character.only = TRUE)
rm(packages)

# Define paths
loc_final = file.path("data", "final")
loc_appCD = file.path(getwd(), "result", "appendix_CD")

# Define country set
country_set = c("France", "United States")

# Graphic parameter
scale_graph = 1920/1080
red = brewer.pal(8, "Set1")[1]
```

## Methodology

I estimate the elasticity of substitution between capital and labor with
a single-equation from the two first-order conditions of the profit
maximization. Estimation are done for France and United States between
1970 and 2010, using Penn World Table 9.1 data.

I estimate the following equation :

<center>

  
![\\ln \\Theta\_t = \\alpha + \\frac{1-\\sigma}{\\sigma} \\ln k\_t +
\\frac{1-\\sigma}{\\sigma} \\left( a\_K-a\_L\\right)t +
\\varepsilon\_t](https://latex.codecogs.com/png.latex?%5Cln%20%5CTheta_t%20%3D%20%5Calpha%20%2B%20%5Cfrac%7B1-%5Csigma%7D%7B%5Csigma%7D%20%5Cln%20k_t%20%2B%20%5Cfrac%7B1-%5Csigma%7D%7B%5Csigma%7D%20%5Cleft%28%20a_K-a_L%5Cright%29t%20%2B%20%5Cvarepsilon_t
"\\ln \\Theta_t = \\alpha + \\frac{1-\\sigma}{\\sigma} \\ln k_t + \\frac{1-\\sigma}{\\sigma} \\left( a_K-a_L\\right)t + \\varepsilon_t")  

</center>

Therefore, I need to compute the capital-to-labor ratio
![\~k\_t\~](https://latex.codecogs.com/png.latex?~k_t~ "~k_t~") and the
capital-to-labor income ratio
![\~\\Theta\_t\~](https://latex.codecogs.com/png.latex?~%5CTheta_t~
"~\\Theta_t~").

## Data

``` r
# Penn World Table
df = read.csv(file.path(loc_final, "pwt.csv"), header = TRUE) %>%
  select("Country", "Year", "emp", "avh", "rgdpna", "rnna", "lab_sh1", "lab_sh2") %>% 
  subset(Country %in% country_set & Year >= 1970)

# Variable modifications
df = df %>%
  subset(Year %in% c(1970:2010)) %>% 
  group_by(Country) %>% 
  mutate(L = emp * 1000, # Labor
         K = rnna * 1000, # Capital
         K.avh = rnna / avh * 1000, # Capital // AVH control
         theta1 = lab_sh1, # Labor share // Adjustment 1
         theta2 = lab_sh2, # Labor share // Adjustment 2
         ) %>% 
  select(Country, Year, avh, L, K, K.avh, theta1, theta2) %>% 
  mutate(avh = avh / first(avh), # Normalized avh
         L = L / first(L), # Normalized labor
         K = K / first(K), # Normalized capital
         K.avh = K.avh / first(K.avh), # Normalized capital // AVH Control
         k = K / L, # Normalized capital-labor ratio
         k.avh = K.avh / L, # Normalized capital-labor ratio // AVH Control
         ) %>% # Normalized old population
  ungroup()

# Remove unused countries from Country levels
df$Country = df$Country %>% as.character %>% as.factor()

# Visualization
head(df)
```

    ## # A tibble: 6 x 10
    ##   Country  Year   avh     L     K K.avh theta1 theta2     k k.avh
    ##   <fct>   <int> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
    ## 1 France   1970 1      1     1     1     0.730  0.676  1     1   
    ## 2 France   1971 1.000  1.00  1.06  1.06  0.726  0.675  1.06  1.06
    ## 3 France   1972 0.974  1.01  1.13  1.15  0.729  0.678  1.11  1.14
    ## 4 France   1973 0.967  1.02  1.19  1.23  0.717  0.667  1.16  1.20
    ## 5 France   1974 0.952  1.04  1.26  1.32  0.720  0.675  1.22  1.28
    ## 6 France   1975 0.939  1.03  1.32  1.41  0.738  0.699  1.28  1.36

The variables from the **Penn World Table 9.1** are the following :

  - *emp* : Number of persons engaged (in millions)  
  - *avh* : Average annual hours worked by persons engaged
  - *rnna* : Capital stock at constant 2011 national prices (in mil.
    2011 US$)  
  - *lab\_sh1* : Share of labour compensation in GDP at current national
    prices (**Adjustment method 1**)
  - *lab\_sh2* : Share of labour compensation in GDP at current national
    prices (**Adjustment method 2**)

<!-- end list -->

``` r
df %>% select(Country, Year, theta1, theta2) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  ggplot(aes(x = Year, y = value, color = variable)) +
  scale_color_discrete(name = "Adjustment Method", breaks = c("lab_sh1", "lab_sh2"),
                       labels = c("1st", "2nd")) +
  geom_line(size = 0.5) +
  facet_wrap(Country ~ .) +
  labs(x = "Year", y = "Labor share") +
  theme_classic(base_size = 14) +
  theme(legend.direction = "horizontal", legend.position = "bottom")
```

![](appendix_CD_files/figure-gfm/compare_both_ls-1.png)<!-- -->

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
adjustment method. I normalize the capital-to-labor ratio to the initial
year (i.e. 1970).

``` r
df %>% select("Country", "Year", "k", "k.avh") %>%
  melt(id.vars = c("Country", "Year")) %>%
  ggplot(aes(x = Year, y = value, color = variable)) +
  geom_line(size = .5) +
  facet_wrap(Country ~ .) +
  scale_color_discrete(name = "", breaks = c("k", "k.avh"),
                       labels = c("Standard", "AVH correction")) +
  labs(x = "Year", y = "Normalized capital-to-labor ratio") +
  theme_classic(base_size = 14) +
  theme(legend.direction = "horizontal", legend.position = "bottom")
```

![](appendix_CD_files/figure-gfm/plot_k_normalized-1.png)<!-- -->

``` r
df = df %>% 
  group_by(Country) %>% 
  mutate(
         # Compute the income ratio
         THETA1 = theta1/(1-theta1),
         THETA2 = theta2/(1-theta2),
         THETA1.avh = THETA1*avh,
         THETA2.avh = THETA2*avh,
         # Logarithm
         THETA1_log = log(THETA1),
         THETA2_log = log(THETA2),
         THETA1.avh_log = log(THETA1.avh),
         THETA2.avh_log = log(THETA2.avh),
         k_log = log(k),
         k.avh_log = log(k.avh),
         # Time trend
         t_diff = Year - first(Year))
```

## Appendix C - Estimation of σ

``` r
ols1.raw = df %>% 
  lm(formula = THETA1_log ~ Country + k_log*Country - 1 - k_log)
ols1.raw %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA1_log ~ Country + k_log * Country - 1 - k_log, 
    ##     data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.13627 -0.04381 -0.01094  0.05614  0.16580 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## CountryFrance               1.12963    0.02786  40.542   <2e-16 ***
    ## CountryUnited States        0.63592    0.02047  31.069   <2e-16 ***
    ## CountryFrance:k_log        -0.61434    0.04452 -13.799   <2e-16 ***
    ## CountryUnited States:k_log -0.17690    0.07278  -2.431   0.0174 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07038 on 78 degrees of freedom
    ## Multiple R-squared:  0.9905, Adjusted R-squared:   0.99 
    ## F-statistic:  2026 on 4 and 78 DF,  p-value: < 2.2e-16

``` r
sigma1.raw = 1/(1 + ols1.raw$coefficients[c(3,4)])

ols1.hwc = df %>% 
  lm(formula = THETA1_log ~ Country + k.avh_log*Country - 1 - k.avh_log)
ols1.hwc %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA1_log ~ Country + k.avh_log * Country - 1 - 
    ##     k.avh_log, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.129555 -0.044562 -0.009756  0.053215  0.172112 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## CountryFrance                   1.12292    0.02694  41.683  < 2e-16 ***
    ## CountryUnited States            0.64757    0.02174  29.784  < 2e-16 ***
    ## CountryFrance:k.avh_log        -0.47009    0.03347 -14.047  < 2e-16 ***
    ## CountryUnited States:k.avh_log -0.18940    0.06663  -2.842  0.00571 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06925 on 78 degrees of freedom
    ## Multiple R-squared:  0.9908, Adjusted R-squared:  0.9903 
    ## F-statistic:  2094 on 4 and 78 DF,  p-value: < 2.2e-16

``` r
sigma1.hwc = 1/(1 + ols1.hwc$coefficients[c(3,4)])

ols1.btc = df %>% 
  lm(formula = THETA1_log ~ Country + k_log*Country + t_diff*Country - 1 - k_log - t_diff)
ols1.btc %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA1_log ~ Country + k_log * Country + t_diff * 
    ##     Country - 1 - k_log - t_diff, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.12567 -0.05326 -0.01102  0.05396  0.14250 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## CountryFrance                1.081029   0.035477  30.472   <2e-16 ***
    ## CountryUnited States         0.642892   0.022353  28.761   <2e-16 ***
    ## CountryFrance:k_log         -0.272830   0.164901  -1.655   0.1021    
    ## CountryUnited States:k_log   0.154819   0.473806   0.327   0.7447    
    ## CountryFrance:t_diff        -0.007390   0.003441  -2.148   0.0349 *  
    ## CountryUnited States:t_diff -0.004283   0.006048  -0.708   0.4810    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06902 on 76 degrees of freedom
    ## Multiple R-squared:  0.9911, Adjusted R-squared:  0.9904 
    ## F-statistic:  1405 on 6 and 76 DF,  p-value: < 2.2e-16

``` r
sigma1.btc = 1/(1 + ols1.btc$coefficients[c(3,4)])

ols1.both = df %>% 
  lm(formula = THETA1_log ~ Country + k.avh_log*Country + t_diff*Country - 1 - k.avh_log - t_diff)
ols1.both %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA1_log ~ Country + k.avh_log * Country + t_diff * 
    ##     Country - 1 - k.avh_log - t_diff, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.124454 -0.053017 -0.008123  0.057549  0.146226 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## CountryFrance                   1.079711   0.033404  32.323   <2e-16 ***
    ## CountryUnited States            0.649321   0.021224  30.594   <2e-16 ***
    ## CountryFrance:k.avh_log        -0.218030   0.124877  -1.746   0.0849 .  
    ## CountryUnited States:k.avh_log -0.647231   0.347772  -1.861   0.0666 .  
    ## CountryFrance:t_diff           -0.007132   0.003411  -2.091   0.0399 *  
    ## CountryUnited States:t_diff     0.006393   0.004770   1.340   0.1842    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06747 on 76 degrees of freedom
    ## Multiple R-squared:  0.9915, Adjusted R-squared:  0.9908 
    ## F-statistic:  1471 on 6 and 76 DF,  p-value: < 2.2e-16

``` r
sigma1.both = 1/(1 + ols1.both$coefficients[c(3,4)])
```

``` r
ols2.raw = df %>% 
  lm(formula = THETA2_log ~ Country + k_log*Country - 1 - k_log)
ols2.raw %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA2_log ~ Country + k_log * Country - 1 - k_log, 
    ##     data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.15338 -0.03853 -0.01243  0.05017  0.17619 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## CountryFrance               0.88971    0.02803  31.745  < 2e-16 ***
    ## CountryUnited States        0.53033    0.02059  25.759  < 2e-16 ***
    ## CountryFrance:k_log        -0.46670    0.04478 -10.421  < 2e-16 ***
    ## CountryUnited States:k_log -0.20917    0.07320  -2.857  0.00547 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0708 on 78 degrees of freedom
    ## Multiple R-squared:  0.9851, Adjusted R-squared:  0.9844 
    ## F-statistic:  1291 on 4 and 78 DF,  p-value: < 2.2e-16

``` r
sigma2.raw = 1/(1 + ols2.raw$coefficients[c(3,4)])

ols2.hwc = df %>% 
  lm(formula = THETA2_log ~ Country + k.avh_log*Country - 1 - k.avh_log)
ols2.hwc %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA2_log ~ Country + k.avh_log * Country - 1 - 
    ##     k.avh_log, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.14870 -0.03962 -0.01233  0.04477  0.18091 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## CountryFrance                   0.88504    0.02710  32.652  < 2e-16 ***
    ## CountryUnited States            0.54150    0.02188  24.753  < 2e-16 ***
    ## CountryFrance:k.avh_log        -0.35769    0.03367 -10.623  < 2e-16 ***
    ## CountryUnited States:k.avh_log -0.21477    0.06704  -3.204  0.00197 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06967 on 78 degrees of freedom
    ## Multiple R-squared:  0.9856, Adjusted R-squared:  0.9849 
    ## F-statistic:  1334 on 4 and 78 DF,  p-value: < 2.2e-16

``` r
sigma2.hwc = 1/(1 + ols2.hwc$coefficients[c(3,4)])

ols2.btc = df %>% 
  lm(formula = THETA2_log ~ Country + k_log*Country + t_diff*Country - 1 - k_log - t_diff)
ols2.btc %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA2_log ~ Country + k_log * Country + t_diff * 
    ##     Country - 1 - k_log - t_diff, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.13202 -0.05373 -0.01055  0.04473  0.14686 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## CountryFrance                0.828548   0.035161  23.564  < 2e-16 ***
    ## CountryUnited States         0.533573   0.022154  24.085  < 2e-16 ***
    ## CountryFrance:k_log         -0.036870   0.163433  -0.226  0.82212    
    ## CountryUnited States:k_log  -0.054838   0.469591  -0.117  0.90734    
    ## CountryFrance:t_diff        -0.009301   0.003410  -2.727  0.00793 ** 
    ## CountryUnited States:t_diff -0.001993   0.005994  -0.332  0.74047    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06841 on 76 degrees of freedom
    ## Multiple R-squared:  0.9865, Adjusted R-squared:  0.9854 
    ## F-statistic: 923.3 on 6 and 76 DF,  p-value: < 2.2e-16

``` r
sigma2.btc = 1/(1 + ols2.btc$coefficients[c(3,4)])

ols2.both = df %>% 
  lm(formula = THETA2_log ~ Country + k.avh_log*Country + t_diff*Country - 1 - k.avh_log - t_diff)
ols2.both %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA2_log ~ Country + k.avh_log * Country + t_diff * 
    ##     Country - 1 - k.avh_log - t_diff, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.131033 -0.051529 -0.007271  0.044541  0.148287 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## CountryFrance                   0.830581   0.033105  25.089  < 2e-16 ***
    ## CountryUnited States            0.543151   0.021034  25.822  < 2e-16 ***
    ## CountryFrance:k.avh_log        -0.039997   0.123761  -0.323  0.74745    
    ## CountryUnited States:k.avh_log -0.645344   0.344662  -1.872  0.06500 .  
    ## CountryFrance:t_diff           -0.008988   0.003380  -2.659  0.00955 ** 
    ## CountryUnited States:t_diff     0.006012   0.004728   1.272  0.20739    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06687 on 76 degrees of freedom
    ## Multiple R-squared:  0.9871, Adjusted R-squared:  0.986 
    ## F-statistic: 966.9 on 6 and 76 DF,  p-value: < 2.2e-16

``` r
sigma2.both = 1/(1 + ols2.both$coefficients[c(3,4)])
```

``` r
data.frame(sigma1.raw, sigma1.hwc, sigma1.btc, sigma1.both,
           sigma2.raw, sigma2.hwc, sigma2.btc, sigma2.both)
```

    ##                            sigma1.raw sigma1.hwc sigma1.btc sigma1.both
    ## CountryFrance:k_log          2.592961   1.887125  1.3751943    1.278821
    ## CountryUnited States:k_log   1.214925   1.233652  0.8659363    2.834714
    ##                            sigma2.raw sigma2.hwc sigma2.btc sigma2.both
    ## CountryFrance:k_log          1.875124    1.55688   1.038282    1.041663
    ## CountryUnited States:k_log   1.264496    1.27352   1.058019    2.819637

## Appendix D - Two σ regimes in France

The purpose of this section is to investigate whether there is a break
in the estimated elasticity of substitution. In particular for France.
Indeed, there capital-per-worker
![k\_t](https://latex.codecogs.com/png.latex?k_t "k_t") grows at a
relatively constant rate while the labor income share
![\~\\Theta\_t\~](https://latex.codecogs.com/png.latex?~%5CTheta_t~
"~\\Theta_t~") faces a huge decrease during the 80’s.

I consider only France with adjustment method 1 and capital with average
hours worked correction. Estimation is done with biased technical
change.

``` r
df.fr = df %>% 
  ungroup() %>% 
  subset(Country == "France") %>% 
  select(- Country)

df.fr %>% 
  select(Year, k.avh_log, THETA1_log) %>% 
  melt(id.vars = "Year") %>% 
  
  ggplot(aes(x = Year, y = value, color = variable)) +
  geom_line() +
  geom_vline(xintercept = 1982, linetype = "dashed") +
  scale_color_manual(values = brewer.pal(8, "Set1")) +
  scale_x_continuous(breaks = c(1970, 1980, 1982, 1990, 2000, 2010),
                     labels = c(1970, "", 1982, 1990, 2000, 2010)) +
  theme_classic(base_size = 14) +
  labs(x = "", y = "") +
  theme(legend.position = "none") +
  ggsave(file.path(loc_appCD, "k_Theta_log.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](appendix_CD_files/figure-gfm/break_year_graph_1-1.png)<!-- -->

``` r
# 1982 included in before
df.fr %>% 
  mutate(period = ifelse(Year <= 1983, "before", "after")) %>% 
  group_by(period) %>% 
  summarise(cor(k.avh_log, THETA1_log))
```

    ## # A tibble: 2 x 2
    ##   period `cor(k.avh_log, THETA1_log)`
    ##   <chr>                         <dbl>
    ## 1 after                        -0.759
    ## 2 before                        0.305

``` r
# 1982 included in after
df.fr %>% 
  mutate(period = ifelse(Year < 1983, "before", "after")) %>% 
  group_by(period) %>% 
  summarise(cor(k.avh_log, THETA1_log))
```

    ## # A tibble: 2 x 2
    ##   period `cor(k.avh_log, THETA1_log)`
    ##   <chr>                         <dbl>
    ## 1 after                        -0.787
    ## 2 before                        0.499

This is the baseline regression, where I estimate the capital-labor
elasticity of substitution for the whole sample.

``` r
ols_base = df.fr %>% 
  lm(THETA1_log ~ k.avh_log + t_diff, data = .)
robust.summary(ols_base)
```

    ## 
    ## Call:  lm(formula = THETA1_log ~ k.avh_log + t_diff, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.079711   0.037982  28.427   <2e-16 ***
    ## k.avh_log   -0.218030   0.143714  -1.517   0.1375    
    ## t_diff      -0.007132   0.003852  -1.852   0.0719 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
sigma.fr_all = 1/(1+ols_base$coefficients[2]) %>% unname()
sigma.fr_all
```

    ## [1] 1.278821

There is a biased technical change, which is constant and significant. I
have to keep the same rate of technical change. Because if I split the
sample, I would obtain two different BTC growth rates. Thus, a way is to
detrend the capital-per-worker and the capital-to-labor income ratio.
Such a methodology comes from the **Frisch–Waugh–Lovell theorem**.

``` r
# Detrend k
df.fr$k.avh_log_detrend = df.fr %>% 
  lm(k.avh_log ~ t_diff, data = .) %>%
  residuals()

# Detrend THETA
df.fr$THETA1_log_detrend = df.fr %>% 
  lm(THETA1_log ~ t_diff, data = .) %>%
  residuals()

# Plot detrended variables

## Core
break_year_detrend = df.fr %>% 
  select(Year, k.avh_log_detrend, THETA1_log_detrend) %>% 
  melt(id.vars = "Year") %>% 
  
  ggplot(aes(x = Year, y = value, color = variable, linetype = variable)) +
  geom_line() +
  geom_vline(xintercept = 1982, linetype = "dotted", color = "grey") +
  geom_vline(xintercept = 1985, linetype = "dotted", color = "grey") +
  geom_vline(xintercept = 1989, linetype = "dotted", color = "grey") +
  scale_x_continuous(breaks = c(1970, 1980, 1982, 1985, 1989, 1990, 2000, 2010),
                     labels = c(1970, "", 1982, 1985, 1989, "", 2000, 2010)) +
  theme_classic(base_size = 14) +
  labs(x = "", y = "") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())

## Paper version
break_year_detrend +
  scale_color_grey(start = 0, end = 0) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  ggsave(file.path(loc_appCD, "k_Theta_log_detrend.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](appendix_CD_files/figure-gfm/break_year_detrend-1.png)<!-- -->

``` r
## Color version
break_year_detrend +
  scale_color_manual(values = brewer.pal(8, "Set1")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  ggsave(file.path(loc_appCD, "k_Theta_log_detrend_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](appendix_CD_files/figure-gfm/break_year_detrend-2.png)<!-- -->

This is an example of the regressions when I break the sample in 1985.
It seems that there are two regimes. There are three main potential
breaks around the years 1982, 1985 and 1989.

``` r
# Find the max of the red and blue curves
df.fr %>% 
  subset(k.avh_log_detrend == max(k.avh_log_detrend) | 
           THETA1_log_detrend == max(THETA1_log_detrend) |
           THETA1_log_detrend == min(THETA1_log_detrend))
```

    ## # A tibble: 3 x 22
    ##    Year   avh     L     K K.avh theta1 theta2     k k.avh THETA1 THETA2
    ##   <int> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl>
    ## 1  1982 0.866  1.07  1.71  1.97  0.732  0.700  1.60  1.85   2.74   2.33
    ## 2  1985 0.837  1.06  1.83  2.19  0.703  0.671  1.73  2.07   2.37   2.04
    ## 3  1989 0.839  1.11  2.04  2.43  0.656  0.622  1.85  2.20   1.91   1.64
    ## # ... with 11 more variables: THETA1.avh <dbl>, THETA2.avh <dbl>,
    ## #   THETA1_log <dbl>, THETA2_log <dbl>, THETA1.avh_log <dbl>,
    ## #   THETA2.avh_log <dbl>, k_log <dbl>, k.avh_log <dbl>, t_diff <int>,
    ## #   k.avh_log_detrend <dbl>, THETA1_log_detrend <dbl>

However, the break may have occur during the 80’s. I have to determine
which year optimize the split. To do so, I use a grid-search approach
between 1980 and 1990.

``` r
## Core

break_year_graph_2regimes = df.fr %>% 
  select(Year, k.avh_log_detrend, THETA1_log_detrend) %>% 
  mutate(before_break = as.factor(ifelse(Year <= 1985, "1970-1985", "1986-2010"))) %>% 
  
  ggplot(aes(x = k.avh_log_detrend, y = THETA1_log_detrend, color = before_break, shape = before_break)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", alpha = 0.2) +
  # stat_smooth(method = "lm", color = "black") +
  theme_classic(base_size = 14) +
  labs(x = "", y = "") +
  theme(legend.direction = "vertical", legend.box = "horizontal", 
          legend.position = c(0.02,1), legend.justification = c(0,1))

## Paper version
break_year_graph_2regimes +
  scale_color_grey(name = "", start = 0.1, end = 0.5) +
  scale_shape_manual(name = "", values = c(16,15)) +
  ggsave(file.path(loc_appCD, "k_Theta_log_reg85.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](appendix_CD_files/figure-gfm/break_year_graph_2regimes-1.png)<!-- -->

``` r
## Color version
break_year_graph_2regimes +
  scale_color_manual(name = "", values = brewer.pal(8, "Set1")[c(3,4)]) +
  scale_shape_manual(name = "", values = c(16,15)) +
  ggsave(file.path(loc_appCD, "k_Theta_log_reg85_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](appendix_CD_files/figure-gfm/break_year_graph_2regimes-2.png)<!-- -->

``` r
split.coef = data.frame(matrix(nrow = 0, ncol = 6))
split.R2 = data.frame(matrix(nrow = 0, ncol = 2))

# Loop to analysis splitters
for(splitter in c(1975:1995)){
  
  est_sigma.fr = df.fr %>% 
    mutate(after_split = ifelse(Year > splitter, 1, 0),
           before_split = ifelse(Year <= splitter, 1, 0))
  
  # Break
  ols.france_dummy = est_sigma.fr %>% 
    lm(THETA1_log_detrend ~ k.avh_log_detrend*before_split + k.avh_log_detrend*after_split - 1 -
         k.avh_log_detrend, data = .)
  ols.france_dummy = summary(ols.france_dummy)
  
  ## Gather results
  # Coefs
  split.coef = split.coef %>%
    rbind(data.frame(splitter,
                     var = ols.france_dummy$coefficients %>% row.names(),
                     ols.france_dummy$coefficients,
                     row.names = NULL))
  # R^2
  split.R2 = split.R2 %>% 
    rbind(data.frame(splitter, R2 = ols.france_dummy$r.squared))
  
}

# Sort data
split.coef = split.coef %>% 
  setNames(c("splitter", "variable", "estimate", "se", "tvalue", "pvalue")) %>% 
  mutate(star = ifelse(pvalue > 0.1, NA,
                       ifelse(pvalue > 0.05, "*",
                              ifelse(pvalue > 0.01, "**", "***"))))
```

``` r
split.coef[3:6] = split.coef[3:6] %>% apply(., 2, function(x){round(x, digits = 3)})
split.coef
```

    ##    splitter                       variable estimate    se tvalue pvalue
    ## 1      1975                   before_split    0.028 0.063  0.440  0.663
    ## 2      1975                    after_split    0.015 0.012  1.240  0.223
    ## 3      1975 k.avh_log_detrend:before_split    0.424 0.470  0.903  0.372
    ## 4      1975  k.avh_log_detrend:after_split   -0.554 0.172 -3.220  0.003
    ## 5      1976                   before_split    0.054 0.053  1.015  0.317
    ## 6      1976                    after_split    0.014 0.013  1.099  0.279
    ## 7      1976 k.avh_log_detrend:before_split    0.580 0.421  1.378  0.177
    ## 8      1976  k.avh_log_detrend:after_split   -0.540 0.174 -3.094  0.004
    ## 9      1977                   before_split    0.062 0.044  1.418  0.165
    ## 10     1977                    after_split    0.012 0.013  0.954  0.346
    ## 11     1977 k.avh_log_detrend:before_split    0.634 0.371  1.709  0.096
    ## 12     1977  k.avh_log_detrend:after_split   -0.529 0.174 -3.033  0.004
    ## 13     1978                   before_split    0.070 0.036  1.969  0.057
    ## 14     1978                    after_split    0.009 0.013  0.727  0.472
    ## 15     1978 k.avh_log_detrend:before_split    0.694 0.322  2.158  0.038
    ## 16     1978  k.avh_log_detrend:after_split   -0.520 0.171 -3.045  0.004
    ## 17     1979                   before_split    0.075 0.030  2.470  0.018
    ## 18     1979                    after_split    0.006 0.013  0.477  0.636
    ## 19     1979 k.avh_log_detrend:before_split    0.725 0.286  2.534  0.016
    ## 20     1979  k.avh_log_detrend:after_split   -0.514 0.166 -3.095  0.004
    ## 21     1980                   before_split    0.080 0.025  3.146  0.003
    ## 22     1980                    after_split    0.002 0.012  0.148  0.883
    ## 23     1980 k.avh_log_detrend:before_split    0.765 0.251  3.046  0.004
    ## 24     1980  k.avh_log_detrend:after_split   -0.516 0.158 -3.268  0.002
    ## 25     1981                   before_split    0.079 0.021  3.760  0.001
    ## 26     1981                    after_split   -0.002 0.012 -0.206  0.838
    ## 27     1981 k.avh_log_detrend:before_split    0.762 0.217  3.517  0.001
    ## 28     1981  k.avh_log_detrend:after_split   -0.533 0.148 -3.604  0.001
    ## 29     1982                   before_split    0.077 0.016  4.738  0.000
    ## 30     1982                    after_split   -0.007 0.010 -0.703  0.487
    ## 31     1982 k.avh_log_detrend:before_split    0.736 0.168  4.392  0.000
    ## 32     1982  k.avh_log_detrend:after_split   -0.607 0.130 -4.676  0.000
    ## 33     1983                   before_split    0.071 0.013  5.623  0.000
    ## 34     1983                    after_split   -0.012 0.009 -1.329  0.192
    ## 35     1983 k.avh_log_detrend:before_split    0.677 0.131  5.178  0.000
    ## 36     1983  k.avh_log_detrend:after_split   -0.690 0.113 -6.112  0.000
    ## 37     1984                   before_split    0.062 0.011  5.599  0.000
    ## 38     1984                    after_split   -0.015 0.008 -1.841  0.074
    ## 39     1984 k.avh_log_detrend:before_split    0.580 0.114  5.090  0.000
    ## 40     1984  k.avh_log_detrend:after_split   -0.764 0.108 -7.042  0.000
    ## 41     1985                   before_split    0.053 0.010  5.541  0.000
    ## 42     1985                    after_split   -0.019 0.008 -2.499  0.017
    ## 43     1985 k.avh_log_detrend:before_split    0.482 0.098  4.923  0.000
    ## 44     1985  k.avh_log_detrend:after_split   -0.870 0.104 -8.353  0.000
    ## 45     1986                   before_split    0.043 0.011  4.075  0.000
    ## 46     1986                    after_split   -0.022 0.009 -2.431  0.020
    ## 47     1986 k.avh_log_detrend:before_split    0.365 0.106  3.444  0.001
    ## 48     1986  k.avh_log_detrend:after_split   -0.928 0.124 -7.488  0.000
    ## 49     1987                   before_split    0.034 0.012  2.974  0.005
    ## 50     1987                    after_split   -0.023 0.010 -2.255  0.030
    ## 51     1987 k.avh_log_detrend:before_split    0.282 0.116  2.427  0.020
    ## 52     1987  k.avh_log_detrend:after_split   -0.956 0.145 -6.608  0.000
    ## 53     1988                   before_split    0.024 0.013  1.874  0.069
    ## 54     1988                    after_split   -0.022 0.012 -1.773  0.084
    ## 55     1988 k.avh_log_detrend:before_split    0.202 0.133  1.521  0.137
    ## 56     1988  k.avh_log_detrend:after_split   -0.931 0.173 -5.394  0.000
    ## 57     1989                   before_split    0.016 0.014  1.109  0.275
    ## 58     1989                    after_split   -0.019 0.014 -1.383  0.175
    ## 59     1989 k.avh_log_detrend:before_split    0.131 0.145  0.902  0.373
    ## 60     1989  k.avh_log_detrend:after_split   -0.893 0.198 -4.517  0.000
    ## 61     1990                   before_split    0.009 0.015  0.647  0.522
    ## 62     1990                    after_split   -0.018 0.015 -1.175  0.248
    ## 63     1990 k.avh_log_detrend:before_split    0.082 0.151  0.540  0.592
    ## 64     1990  k.avh_log_detrend:after_split   -0.871 0.216 -4.026  0.000
    ## 65     1991                   before_split    0.005 0.015  0.310  0.758
    ## 66     1991                    after_split   -0.017 0.016 -1.039  0.305
    ## 67     1991 k.avh_log_detrend:before_split    0.041 0.155  0.268  0.791
    ## 68     1991  k.avh_log_detrend:after_split   -0.857 0.235 -3.653  0.001
    ## 69     1992                   before_split    0.000 0.015  0.029  0.977
    ## 70     1992                    after_split   -0.016 0.017 -0.914  0.367
    ## 71     1992 k.avh_log_detrend:before_split    0.004 0.158  0.026  0.979
    ## 72     1992  k.avh_log_detrend:after_split   -0.842 0.257 -3.280  0.002
    ## 73     1993                   before_split   -0.003 0.015 -0.182  0.856
    ## 74     1993                    after_split   -0.016 0.019 -0.863  0.394
    ## 75     1993 k.avh_log_detrend:before_split   -0.028 0.159 -0.174  0.863
    ## 76     1993  k.avh_log_detrend:after_split   -0.851 0.287 -2.966  0.005
    ## 77     1994                   before_split   -0.006 0.015 -0.372  0.712
    ## 78     1994                    after_split   -0.016 0.021 -0.753  0.456
    ## 79     1994 k.avh_log_detrend:before_split   -0.051 0.160 -0.317  0.753
    ## 80     1994  k.avh_log_detrend:after_split   -0.841 0.322 -2.615  0.013
    ## 81     1995                   before_split   -0.008 0.015 -0.512  0.612
    ## 82     1995                    after_split   -0.017 0.024 -0.704  0.486
    ## 83     1995 k.avh_log_detrend:before_split   -0.065 0.160 -0.407  0.686
    ## 84     1995  k.avh_log_detrend:after_split   -0.856 0.367 -2.331  0.025
    ##    star
    ## 1  <NA>
    ## 2  <NA>
    ## 3  <NA>
    ## 4   ***
    ## 5  <NA>
    ## 6  <NA>
    ## 7  <NA>
    ## 8   ***
    ## 9  <NA>
    ## 10 <NA>
    ## 11    *
    ## 12  ***
    ## 13    *
    ## 14 <NA>
    ## 15   **
    ## 16  ***
    ## 17   **
    ## 18 <NA>
    ## 19   **
    ## 20  ***
    ## 21  ***
    ## 22 <NA>
    ## 23  ***
    ## 24  ***
    ## 25  ***
    ## 26 <NA>
    ## 27  ***
    ## 28  ***
    ## 29  ***
    ## 30 <NA>
    ## 31  ***
    ## 32  ***
    ## 33  ***
    ## 34 <NA>
    ## 35  ***
    ## 36  ***
    ## 37  ***
    ## 38    *
    ## 39  ***
    ## 40  ***
    ## 41  ***
    ## 42   **
    ## 43  ***
    ## 44  ***
    ## 45  ***
    ## 46   **
    ## 47  ***
    ## 48  ***
    ## 49  ***
    ## 50   **
    ## 51   **
    ## 52  ***
    ## 53    *
    ## 54    *
    ## 55 <NA>
    ## 56  ***
    ## 57 <NA>
    ## 58 <NA>
    ## 59 <NA>
    ## 60  ***
    ## 61 <NA>
    ## 62 <NA>
    ## 63 <NA>
    ## 64  ***
    ## 65 <NA>
    ## 66 <NA>
    ## 67 <NA>
    ## 68  ***
    ## 69 <NA>
    ## 70 <NA>
    ## 71 <NA>
    ## 72  ***
    ## 73 <NA>
    ## 74 <NA>
    ## 75 <NA>
    ## 76  ***
    ## 77 <NA>
    ## 78 <NA>
    ## 79 <NA>
    ## 80   **
    ## 81 <NA>
    ## 82 <NA>
    ## 83 <NA>
    ## 84   **

``` r
split.R2$R2 = split.R2$R2 %>% round(digits = 3)
split.R2
```

    ##    splitter    R2
    ## 1      1975 0.244
    ## 2      1976 0.239
    ## 3      1977 0.247
    ## 4      1978 0.278
    ## 5      1979 0.317
    ## 6      1980 0.385
    ## 7      1981 0.461
    ## 8      1982 0.596
    ## 9      1983 0.705
    ## 10     1984 0.741
    ## 11     1985 0.780
    ## 12     1986 0.713
    ## 13     1987 0.635
    ## 14     1988 0.507
    ## 15     1989 0.391
    ## 16     1990 0.321
    ## 17     1991 0.270
    ## 18     1992 0.225
    ## 19     1993 0.195
    ## 20     1994 0.168
    ## 21     1995 0.153

``` r
# Max R2 : define the break_year
break_year = split.R2 %>% subset(R2 == max(R2)) %>% pull("splitter")
print(paste0("Break in the regime in : ", break_year))
```

    ## [1] "Break in the regime in : 1985"

``` r
### With BTC
## Before
ols.fr.btc_before = df.fr %>% 
  subset(Year <= break_year) %>% 
  lm(formula = THETA1_log ~ k.avh_log + t_diff)

ols.fr.btc_before %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA1_log ~ k.avh_log + t_diff, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.07578 -0.02134  0.01235  0.03213  0.06067 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.96979    0.03339  29.047 3.29e-13 ***
    ## k.avh_log    1.00248    0.64833   1.546    0.146    
    ## t_diff      -0.05049    0.03061  -1.649    0.123    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04793 on 13 degrees of freedom
    ## Multiple R-squared:  0.2365, Adjusted R-squared:  0.119 
    ## F-statistic: 2.013 on 2 and 13 DF,  p-value: 0.1731

``` r
sigma.fr.btc_before = 1/(1+ols.fr.btc_before$coefficients[2]) %>% unname()
sigma.fr.btc_before
```

    ## [1] 0.4993801

``` r
## After
ols.fr.btc_after = df.fr %>% 
  subset(Year > break_year) %>% 
  lm(formula = THETA1_log ~ k.avh_log + t_diff)

ols.fr.btc_after %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA1_log ~ k.avh_log + t_diff, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.039442 -0.017052 -0.005733  0.013388  0.080225 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.915888   0.160461   5.708 9.67e-06 ***
    ## k.avh_log   -0.322306   0.320510  -1.006    0.326    
    ## t_diff       0.001347   0.005376   0.251    0.804    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02961 on 22 degrees of freedom
    ## Multiple R-squared:  0.5288, Adjusted R-squared:  0.486 
    ## F-statistic: 12.35 on 2 and 22 DF,  p-value: 0.0002542

``` r
sigma.fr.btc_after = 1/(1+ols.fr.btc_after$coefficients[2]) %>% unname()
sigma.fr.btc_after
```

    ## [1] 1.475592

``` r
### Without BTC
ols.fr_before = df.fr %>% 
  subset(Year <= break_year) %>% 
  lm(formula = THETA1_log ~ k.avh_log)

ols.fr_before %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA1_log ~ k.avh_log, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.09758 -0.03345  0.01347  0.04055  0.05054 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.00701    0.02607  38.624 1.26e-15 ***
    ## k.avh_log   -0.06292    0.05833  -1.079    0.299    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05079 on 14 degrees of freedom
    ## Multiple R-squared:  0.07672,    Adjusted R-squared:  0.01077 
    ## F-statistic: 1.163 on 1 and 14 DF,  p-value: 0.299

``` r
sigma.fr_before = 1/(1+ols.fr_before$coefficients[2]) %>% unname()
sigma.fr_before
```

    ## [1] 1.06714

``` r
## After
ols.fr_after = df.fr %>% 
  subset(Year > break_year) %>% 
  lm(formula = THETA1_log ~ k.avh_log)

ols.fr_after %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = THETA1_log ~ k.avh_log, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.039685 -0.017683 -0.008255  0.011113  0.081093 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.87747    0.04636  18.928 1.60e-15 ***
    ## k.avh_log   -0.24294    0.04795  -5.067 3.95e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.029 on 23 degrees of freedom
    ## Multiple R-squared:  0.5275, Adjusted R-squared:  0.5069 
    ## F-statistic: 25.67 on 1 and 23 DF,  p-value: 3.951e-05

``` r
sigma.fr_after = 1/(1+ols.fr_after$coefficients[2]) %>% unname()
sigma.fr_after
```

    ## [1] 1.320901
