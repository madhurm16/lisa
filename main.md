Labor Share & Aging Population
================
Fabien Petit

## Labor Share & Aging Population

``` r
# Country set
country_set = c("France", "United States")

# Simulation periods
sim = seq(1970, 2080, 10)

# Estimation sample
est_sample = c(1970:2010)
```

``` r
# Penn World Table
pwt = read.csv(file.path(loc_final, "pwt.csv"), header = TRUE) %>%
  select("Country", "Year", "emp", "avh", "rgdpna", "rnna", "lab_sh1") %>% 
  subset(Country %in% country_set & Year >= 1970)

# OECD data
oecd = read.csv(file.path(loc_final, "oecd.csv"), header = TRUE) %>% 
  select("Country", "Year", tax_rate = "tax_rev_PC_GDP", union_density = "union_density.lin_inter", union_coverage = "union_coverage.lin_inter") %>% 
  subset(Country %in% country_set & Year >= 1970)

# Demographic data
demo = read.csv(file.path(loc_final, "demo.csv"), header = TRUE) %>% 
  select("Country", "Year", "young", "old", "dep", "n", "p") %>% 
  subset(Country %in% country_set & Year >= 1970)

## Merge
df = merge(merge(pwt, oecd, all = TRUE), demo, all = TRUE)
```

``` r
# Variable modifications
df = df %>%
  group_by(Country) %>% 
  mutate(L = emp * 1000,
         Y = rgdpna / avh * 1000, # AVH control
         K = rnna / avh * 1000, # AVH control
         theta = lab_sh1, # Labor share
         tau = tax_rate / 100, # Tax rate
         u = 1 - L / young, # Unemployment rate
         p1 = lead(p, 40)) %>% 
  select(Country, Year, Ny = young, No = old, n, p, p1, dep, K, L, Y, theta, tau, u) %>% 
  mutate(L = L / first(L), # Normalized labor
         K = K / first(K), # Normalized capital
         Y = Y / first(Y), # Normalized output
         k = K / L, # Normalized capital-labor ratio
         Ny = L / (1-u), # Normalized young population
         No = Ny * dep) # Normalized old population

# Visualization
head(df)
```

    ## # A tibble: 6 x 15
    ## # Groups:   Country [1]
    ##   Country  Year    Ny    No     n     p    p1   dep     K     L     Y theta
    ##   <fct>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 France   1970  1.17 0.430  1.13 0.417 0.583 0.368  1     1     1    0.730
    ## 2 France   1971  1.18 0.437  1.14 0.422 0.591 0.370  1.06  1.00  1.05 0.726
    ## 3 France   1972  1.19 0.444  1.15 0.429 0.597 0.372  1.15  1.01  1.13 0.729
    ## 4 France   1973  1.21 0.451  1.16 0.435 0.603 0.374  1.23  1.02  1.21 0.717
    ## 5 France   1974  1.22 0.454  1.18 0.438 0.607 0.372  1.32  1.04  1.28 0.720
    ## 6 France   1975  1.24 0.454  1.19 0.437 0.609 0.366  1.41  1.03  1.29 0.738
    ## # ... with 3 more variables: tau <dbl>, u <dbl>, k <dbl>

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

``` r
## Generate different specifications for bargaining power
param.gamma = oecd %>% 
  # subset(Year %in% sim) %>% 
  group_by(Country) %>% 
  mutate(gamma.cst = 0.5,
         gamma0 = sqrt(union_density*union_coverage),
         gamma1 = gamma0 / first(gamma0) * gamma.cst, # Rescale gamma such that gamma = 0.5 in 1970
         gamma2 = gamma0 / mean(gamma0, na.rm = TRUE) * gamma.cst) %>% # Rescale gamma such that gamma is normalize to its average value on the estimated sample
  merge(param[, c("Country", "Year")], ., all.x = TRUE) %>% 
  select(Country, Year, starts_with("gamma")) %>% 
  mutate(gamma.cst = 0.5)

param.gamma
```

    ##          Country Year gamma.cst   gamma0    gamma1    gamma2
    ## 1         France 1970       0.5 38.96761 0.5000000 0.6025749
    ## 2         France 1980       0.5 37.60997 0.4825800 0.5815812
    ## 3         France 1990       0.5 30.53909 0.3918522 0.4722407
    ## 4         France 2000       0.5 27.45578 0.3522898 0.4245620
    ## 5         France 2010       0.5 28.01097 0.3594136 0.4331472
    ## 6         France 2020       0.5       NA        NA        NA
    ## 7         France 2030       0.5       NA        NA        NA
    ## 8         France 2040       0.5       NA        NA        NA
    ## 9         France 2050       0.5       NA        NA        NA
    ## 10        France 2060       0.5       NA        NA        NA
    ## 11        France 2070       0.5       NA        NA        NA
    ## 12        France 2080       0.5       NA        NA        NA
    ## 13 United States 1970       0.5 28.68851 0.5000000 0.8142045
    ## 14 United States 1980       0.5 23.48717 0.4093480 0.6665860
    ## 15 United States 1990       0.5 16.78763 0.2925846 0.4764475
    ## 16 United States 2000       0.5 13.86865 0.2417108 0.3936041
    ## 17 United States 2010       0.5 12.21158 0.2128305 0.3465752
    ## 18 United States 2020       0.5       NA        NA        NA
    ## 19 United States 2030       0.5       NA        NA        NA
    ## 20 United States 2040       0.5       NA        NA        NA
    ## 21 United States 2050       0.5       NA        NA        NA
    ## 22 United States 2060       0.5       NA        NA        NA
    ## 23 United States 2070       0.5       NA        NA        NA
    ## 24 United States 2080       0.5       NA        NA        NA

``` r
param.pref = df %>%
  subset(Year == 1970) %>% 
  merge(param %>% select(Country, Year, phi, sigma, a, alpha), .) %>% 
  merge(param.gamma %>% select(Country, Year, gamma.cst, gamma0, gamma1, gamma2), .) %>% 
  mutate(## Variable : X for each gamma specification
         X.cst = (sigma + (1-phi)/phi*(1-gamma.cst*(1-sigma))/gamma.cst)^(-1),
         X0 = (sigma + (1-phi)/phi*(1-gamma0*(1-sigma))/gamma0)^(-1),
         # X1 is the same as X.cst since gamma1 = 0.5 in 1970
         X2 = (sigma + (1-phi)/phi*(1-gamma2*(1-sigma))/gamma2)^(-1),
         
         ## Parameter : OMEGA
         omega.cst = phi/(1-phi)*n/p*(1+alpha*p1)/(1 + exp(-X.cst)*(Ny - 1)),
         omega0 = phi/(1-phi)*n/p*(1+alpha*p1)/(1 + exp(-X0)*(Ny - 1)),
         # omega1 is the same as omega.cst since gamma1 = 0.5 in 1970
         omega2 = phi/(1-phi)*n/p*(1+alpha*p1)/(1 + exp(-X2)*(Ny - 1)),
         
         ## Variable : ETA
         eta.cst = n/p*(1+alpha*p1)/omega.cst,
         eta0 = n/p*(1+alpha*p1)/omega0,
         # eta1 is the same as X.cst since gamma1 = 0.5 in 1970
         eta2 = n/p*(1+alpha*p1)/omega2,
         
         ## Parameter : BETA
         beta.cst = 1/(1 - tau)/phi - 1 - eta.cst,
         beta0 = 1/(1 - tau)/phi - 1 - eta0,
         # beta1 is the same as omega.cst since gamma1 = 0.5 in 1970
         beta2 = 1/(1 - tau)/phi - 1 - eta2
         ) %>% 
  select(Country, starts_with("omega"), starts_with("beta")) %>% 
  merge(param[, c("Country", "Year")], ., all.x = TRUE)

# Visualization
param.pref
```

    ##          Country Year omega.cst   omega0   omega2  beta.cst    beta0
    ## 1         France 1970  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 2         France 1980  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 3         France 1990  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 4         France 2000  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 5         France 2010  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 6         France 2020  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 7         France 2030  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 8         France 2040  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 9         France 2050  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 10        France 2060  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 11        France 2070  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 12        France 2080  1.220024 1.260691 1.222749 1.4876013 1.587524
    ## 13 United States 1970  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 14 United States 1980  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 15 United States 1990  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 16 United States 2000  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 17 United States 2010  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 18 United States 2020  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 19 United States 2030  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 20 United States 2040  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 21 United States 2050  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 22 United States 2060  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 23 United States 2070  1.907372 1.998928 1.924234 0.7278886 0.838648
    ## 24 United States 2080  1.907372 1.998928 1.924234 0.7278886 0.838648
    ##        beta2
    ## 1  1.4945053
    ## 2  1.4945053
    ## 3  1.4945053
    ## 4  1.4945053
    ## 5  1.4945053
    ## 6  1.4945053
    ## 7  1.4945053
    ## 8  1.4945053
    ## 9  1.4945053
    ## 10 1.4945053
    ## 11 1.4945053
    ## 12 1.4945053
    ## 13 0.7490792
    ## 14 0.7490792
    ## 15 0.7490792
    ## 16 0.7490792
    ## 17 0.7490792
    ## 18 0.7490792
    ## 19 0.7490792
    ## 20 0.7490792
    ## 21 0.7490792
    ## 22 0.7490792
    ## 23 0.7490792
    ## 24 0.7490792
