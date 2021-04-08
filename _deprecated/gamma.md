LISA - Gamma specification
================
Fabien Petit

## Data

Data are from Penn World Table 9.1, OECD Database and the United Nations
database (World Population Prospects). The purpose of this section is to
explore the impact of ![\\gamma \\in
(0,1)](https://latex.codecogs.com/png.latex?%5Cgamma%20%5Cin%20%280%2C1%29
"\\gamma \\in (0,1)"), i.e. the bargaining power.

``` r
# Define paths
loc_result = file.path(getwd(), "result")
loc_sim = file.path(loc_result, "sim")
loc_function = file.path(getwd(), "function")
loc_final = file.path(getwd(), "data", "final")

# Load packages
packages <- c("bucky", "dplyr", "ggplot2", "ggrepel", "lmtest", "sandwich", "zoo", "RColorBrewer", "reshape2")
lapply(packages, require, character.only = TRUE)
rm(packages)

# Load functions
sapply(list.files(pattern = "[.]R$", path = loc_function, full.names = TRUE), source)

# Country set
country_set = c("France", "United States")

# Simulation periods
sim = seq(1970, 2080, 10)

# Estimation sample
est_sample = c(1970:2010)
```

``` r
# Penn World Table 9.1
pwt = read.csv(file.path(loc_final, "pwt.csv"), header = TRUE) %>%
  select("Country", "Year", "emp", "avh", "rgdpna", "rnna", "lab_sh1") %>% 
  subset(Country %in% country_set & Year >= 1970)

# OECD data
oecd = read.csv(file.path(loc_final, "oecd.csv"), header = TRUE) %>% 
  select("Country", "Year", tax_rate = "tax_rev_PC_GDP", 
         union_density = "union_density.lin_inter", union_coverage = "union_coverage.lin_inter") %>% 
  subset(Country %in% country_set & Year >= 1970)

# Demographic data
demo = read.csv(file.path(loc_final, "demo.csv"), header = TRUE) %>% 
  select("Country", "Year", "young", "young_1564", "old", "dep", "n", "p") %>% 
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
         u = 1 - L / (young_1564), # Unemployment rate
         p1 = lead(p, 40)) %>% 
  select(Country, Year, Ny = young, No = old, n, p, p1, dep, K, L, Y, theta, tau, u) %>% 
  mutate(L = L / first(L), # Normalized labor
         K = K / first(K), # Normalized capital
         Y = Y / first(Y), # Normalized output
         k = K / L, # Normalized capital-labor ratio
         Ny = L / (1-u), # Normalized young population
         No = Ny * dep) %>% # Normalized old population
  ungroup()

# Remove unused countries from Country levels
df$Country = df$Country %>% as.character %>% as.factor()

# Visualization
head(df)
```

    ## # A tibble: 6 x 15
    ##   Country  Year    Ny    No     n     p    p1   dep     K     L     Y theta
    ##   <fct>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 France   1970  1.49 0.546  1.13 0.417 0.583 0.368  1     1     1    0.730
    ## 2 France   1971  1.50 0.554  1.14 0.422 0.591 0.370  1.06  1.00  1.05 0.726
    ## 3 France   1972  1.51 0.564  1.15 0.429 0.597 0.372  1.15  1.01  1.13 0.729
    ## 4 France   1973  1.53 0.571  1.16 0.435 0.603 0.374  1.23  1.02  1.21 0.717
    ## 5 France   1974  1.54 0.574  1.18 0.438 0.607 0.372  1.32  1.04  1.28 0.720
    ## 6 France   1975  1.56 0.570  1.19 0.437 0.609 0.366  1.41  1.03  1.29 0.738
    ## # ... with 3 more variables: tau <dbl>, u <dbl>, k <dbl>

## Parameter calibration

*For more details on the calibration, please consult “main.md”
file.*

``` r
param_base = data.frame("Country" = rep(c("France", "United States"), each = length(sim)),
                   "Year" = sim,
                   "phi" = NA, "sigma" = NA, "a" = NA, "alpha" = (0.99)^40, "gamma" = NA,
                   "omega" = NA, "beta" = NA, A = NA)
```

### Production function

I estimate the elasticity of substitution between capital and labor
using the first order conditions from the profit maximization.
Estimation are done for France and United States between 1970 and 2010,
using Penn World Table 9.1 data. *For more details, please consult
“sigma.md” file.*

``` r
# Estimation
est_sigma = df %>% 
  group_by(Country) %>% 
  mutate(k_log = log(k),
         THETA_log = log(theta/(1-theta)),
         t_diff = Year - first(Year)) %>% 
  select(Country, Year, k_log, THETA_log, t_diff) %>% 
  subset(Year %in% c(1970:2010)) %>% 
  ungroup()

### Regression France : one sigma for the whole period
  ols.france_all = est_sigma %>% 
    subset(Country == "France") %>% 
    lm(THETA_log ~ k_log + t_diff, data = .)
  # Report sigma
    sigma.fr_all = 1/(1 + ols.france_all$coefficients[2]) %>% unname()

# Result visualization
robust.summary(ols.france_all)
```

    ## 
    ## Call:  lm(formula = THETA_log ~ k_log + t_diff, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.079711   0.037982  28.427   <2e-16 ***
    ## k_log       -0.218030   0.143714  -1.517   0.1375    
    ## t_diff      -0.007132   0.003852  -1.852   0.0719 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
### Regression United-States : one sigma for the whole period
  ols.us_all = est_sigma %>% 
    subset(Country == "United States") %>% 
    lm(THETA_log ~ k_log, data = .)
  # Report Sigma
  sigma.us_all = 1/(1 + ols.us_all$coefficients[2]) %>% unname()

# Result visualization
robust.summary(ols.us_all)
```

    ## 
    ## Call:  lm(formula = THETA_log ~ k_log, data = .)
    ## 
    ## Call (vcov):  vcovHC(x = ..., type = "HC0")
    ## 
    ## t test of coefficients with robust standard errors:
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.64757    0.01662  38.975  < 2e-16 ***
    ## k_log       -0.18940    0.05015  -3.776 0.000531 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Add production function parameters
param_base = param_base %>% 
  mutate("phi" = rep(1 - df %>% subset(Year == 1970) %>% pull("theta"), each = length(sim)),
         "sigma" = rep(c(sigma.fr_all, sigma.us_all), each = length(sim)))
```

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

``` r
## Generate different specifications for bargaining power
param.gamma = oecd %>% 
  # subset(Year %in% sim) %>% 
  group_by(Country) %>% 
  mutate(gamma.cst = 0.5,
         gamma0 = sqrt(union_density/100*union_coverage/100),
         gamma1 = gamma0 / first(gamma0) * gamma.cst, # Rescale gamma such that gamma = 0.5 in 1970
         gamma2 = gamma0 / mean(gamma0, na.rm = TRUE) * gamma.cst) %>% # Rescale gamma such that gamma is normalize to its average value on the estimated sample
  merge(param_base[, c("Country", "Year")], ., all.x = TRUE) %>% 
  select(Country, Year, starts_with("gamma")) %>% 
  mutate(gamma.cst = 0.5)

# List gamma specification
gamma_specification = c(".cst", "0", "1", "2")

## Predictions about gamma to be able to run the model
# Constant interpolation
param.gamma[param.gamma$Year ==  2080 , c("gamma0","gamma1","gamma2")] = param.gamma %>%
  subset(Year == 2010) %>%
  select(gamma0, gamma1, gamma2)

param.gamma = param.gamma %>%
  interpol_group(method_use = "constant")
```

    ## [1] "Number of NA values removed for the variable gamma.cst: 0 on 0 (NaN%)"
    ## [1] "Number of NA values removed for the variable gamma0: 12 on 12 (100%)"
    ## [1] "Number of NA values removed for the variable gamma1: 12 on 12 (100%)"
    ## [1] "Number of NA values removed for the variable gamma2: 12 on 12 (100%)"

``` r
# Visualization
param.gamma
```

    ##          Country Year gamma.cst    gamma0    gamma1    gamma2
    ## 1         France 1970       0.5 0.3896761 0.5000000 0.6025749
    ## 2         France 1980       0.5 0.3760997 0.4825800 0.5815812
    ## 3         France 1990       0.5 0.3053909 0.3918522 0.4722407
    ## 4         France 2000       0.5 0.2745578 0.3522898 0.4245620
    ## 5         France 2010       0.5 0.2801097 0.3594136 0.4331472
    ## 6         France 2020       0.5 0.2801097 0.3594136 0.4331472
    ## 7         France 2030       0.5 0.2801097 0.3594136 0.4331472
    ## 8         France 2040       0.5 0.2801097 0.3594136 0.4331472
    ## 9         France 2050       0.5 0.2801097 0.3594136 0.4331472
    ## 10        France 2060       0.5 0.2801097 0.3594136 0.4331472
    ## 11        France 2070       0.5 0.2801097 0.3594136 0.4331472
    ## 12        France 2080       0.5 0.2801097 0.3594136 0.4331472
    ## 13 United States 1970       0.5 0.2868851 0.5000000 0.8142045
    ## 14 United States 1980       0.5 0.2348717 0.4093480 0.6665860
    ## 15 United States 1990       0.5 0.1678763 0.2925846 0.4764475
    ## 16 United States 2000       0.5 0.1386865 0.2417108 0.3936041
    ## 17 United States 2010       0.5 0.1221158 0.2128305 0.3465752
    ## 18 United States 2020       0.5 0.1221158 0.2128305 0.3465752
    ## 19 United States 2030       0.5 0.1221158 0.2128305 0.3465752
    ## 20 United States 2040       0.5 0.1221158 0.2128305 0.3465752
    ## 21 United States 2050       0.5 0.1221158 0.2128305 0.3465752
    ## 22 United States 2060       0.5 0.1221158 0.2128305 0.3465752
    ## 23 United States 2070       0.5 0.1221158 0.2128305 0.3465752
    ## 24 United States 2080       0.5 0.1221158 0.2128305 0.3465752

### Preferences

According to the specification of
![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma "\\gamma"), the
parameters ![\\omega](https://latex.codecogs.com/png.latex?%5Comega
"\\omega") and ![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta
"\\beta") are different. Therefore, I compute parameter values for each
specification.

``` r
## Generate different parameter values according to the value of gamma
param.pref = df %>%
  subset(Year == 1970) %>% 
  merge(param_base %>% select(Country, Year, phi, sigma, a, alpha), .) %>% 
  merge(param.gamma %>% select(Country, Year, gamma.cst, gamma0, gamma1, gamma2), .) %>% 
  mutate(## Variable : X for each gamma specification
         X.cst = (sigma + (1-phi)/phi*(1-gamma.cst*(1-sigma))/gamma.cst)^(-1),
         X0 = (sigma + (1-phi)/phi*(1-gamma0*(1-sigma))/gamma0)^(-1),
         X1 = X.cst, # X1 is the same as X.cst since gamma1 = 0.5 in 1970
         X2 = (sigma + (1-phi)/phi*(1-gamma2*(1-sigma))/gamma2)^(-1),
         
         ## Parameter : OMEGA
         omega.cst = phi/(1-phi)*n/p*(1+alpha*p1)/(1 + exp(-X.cst)*(Ny - 1)),
         omega0 = phi/(1-phi)*n/p*(1+alpha*p1)/(1 + exp(-X0)*(Ny - 1)),
         omega1 = omega.cst, # omega1 is the same as omega.cst since gamma1 = 0.5 in 1970
         omega2 = phi/(1-phi)*n/p*(1+alpha*p1)/(1 + exp(-X2)*(Ny - 1)),
         
         ## Variable : ETA
         eta.cst = n/p*(1+alpha*p1)/omega.cst,
         eta0 = n/p*(1+alpha*p1)/omega0,
         eta1 = eta.cst, # eta1 is the same as X.cst since gamma1 = 0.5 in 1970
         eta2 = n/p*(1+alpha*p1)/omega2,
         
         ## Parameter : BETA
         beta.cst = 1/(1 - tau)/phi - 1 - eta.cst,
         beta0 = 1/(1 - tau)/phi - 1 - eta0,
         beta1 = beta.cst, # beta1 is the same as omega.cst since gamma1 = 0.5 in 1970
         beta2 = 1/(1 - tau)/phi - 1 - eta2
         ) %>% 
  select(Country, starts_with("omega"), starts_with("beta")) %>% 
  merge(param_base[, c("Country", "Year")], ., all.x = TRUE)

# Visualization
param.pref %>% subset(Year ==  1970) %>% select(-Year)
```

    ##          Country omega.cst    omega0    omega1    omega2  beta.cst
    ## 1         France 0.9825708 0.9758192 0.9825708 0.9881093 0.7390045
    ## 13 United States 1.5332631 1.5052224 1.5332631 1.5631226 0.1378635
    ##         beta0     beta1     beta2
    ## 1  0.71239242 0.7390045 0.7605632
    ## 13 0.08182374 0.1378635 0.1953278

## Simulation

I simulate the model with a different
![A](https://latex.codecogs.com/png.latex?A "A") parameter for each
![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma "\\gamma")
specification and then with the same
![A](https://latex.codecogs.com/png.latex?A "A") parameter.

``` r
init_seq = seq(1970, 2000, 10)

# Baseline data
data_base = df %>% 
  group_by(Country) %>% 
  subset(Year %in% sim) %>% 
  select(Country, Year, Ny, No, n, p, p1, K) %>% 
  mutate(Sequence = ((sim - 1970)/10) %% 4 + 1,
         Period = (sim - init_seq) / 40 + 1,
         Ny = ifelse(Year == 2010, NA, Ny),
         No = ifelse(Year == 2010, NA, No),
         K = ifelse(Year == 2010, NA, K),
         # Complete NA values for p1 in 2070 and 2080
         p1 = ifelse(Year >= 2070, p1[Year == 2060] + 
                       (Year - 2060)/10 * mean(p1/lag(p1)-1, na.rm = T), p1), 
         ## Create empty variables
         eta = NA, AK = NA, AL = NA, k1 = NA, k2 = NA, k = NA, X = NA, L = NA, w = NA,
         r = NA, Y = NA, u = NA, theta = NA, tau = NA, b = NA, h = NA, S = NA) 

# Re-order variables
data_base = data_base %>% select(Country, Year, Sequence, Period, Ny, No, n, p, p1, eta, AK, AL,
                                 k1, k2, k, X, L, w, r, Y, u, theta, tau, b, h, S, K)
  
# Visualization
head(data_base)
```

    ## # A tibble: 6 x 27
    ## # Groups:   Country [1]
    ##   Country  Year Sequence Period    Ny     No     n     p    p1 eta   AK   
    ##   <fct>   <int>    <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <lgl> <lgl>
    ## 1 France   1970        1      1  1.49  0.546  1.13 0.417 0.583 NA    NA   
    ## 2 France   1980        2      1  1.62  0.516  1.39 0.444 0.613 NA    NA   
    ## 3 France   1990        3      1  1.76  0.629  1.36 0.486 0.667 NA    NA   
    ## 4 France   2000        4      1  1.82  0.694  1.38 0.525 0.683 NA    NA   
    ## 5 France   2010        1      2 NA    NA      1.33 0.583 0.685 NA    NA   
    ## 6 France   2020        2      2 NA    NA      1.13 0.613 0.707 NA    NA   
    ## # ... with 16 more variables: AL <lgl>, k1 <lgl>, k2 <lgl>, k <lgl>,
    ## #   X <lgl>, L <lgl>, w <lgl>, r <lgl>, Y <lgl>, u <lgl>, theta <lgl>,
    ## #   tau <lgl>, b <lgl>, h <lgl>, S <lgl>, K <dbl>

``` r
# Prepare dataset
data = data_base %>% 
  # Demography
  demo_changer(init_spe = TRUE)

# Prepare parameters
param = param_base
```

### Different scale parameter (A)

``` r
# Initialize final dataset for gamma comparison
final_gamma_diffA = data.frame(matrix(ncol = ncol(data)+ ncol(param) -2 + 1, nrow = 0)) %>%
  setNames(c("Specification", names(data), names(param)[-c(1,2)]))

# Data save
data_save = data

for(gamma_spe in gamma_specification){
  
  # Prepare data
  data = data_save %>% 
    # Re-initialize data and define specification of the model
    mutate(Specification = gamma_spe) %>% 
    # Put Specification as 1st column
    select(Specification, everything()) %>% 
    # Select gamma / omega / beta : choose between ".cst" / "0" / "1" / "2"
    gob_finder(param, gamma_spe = gamma_spe) %>% 
    # No BTC
    mutate(AK = 1, AL = 1) %>% 
    # Demography
    demo_changer(AFinder = TRUE)
  
  # A Finder script
  source("./script/sim_AFinder.R")
  
  # Empty dataframe for results
  result = data.frame(matrix(ncol = ncol(data), nrow = 0)) %>%
    setNames(names(data))
  
  # Simulate model for country_set
  for(country in country_set){
    
    result = data %>%
      subset(Country == country) %>% 
      model(time = 2) %>% 
      rbind(result, .)
    
  }
  
  # Regroup results
  final_gamma_diffA = rbind(final_gamma_diffA, result)
  
}

# Write final_gamma
write.csv(final_gamma_diffA, file.path(loc_sim, "final_gamma_diffA.csv"), row.names = FALSE)
```

``` r
final_gamma_diffA %>% 
  select(Specification, Country, Year, theta) %>% 
  subset(Year %in% seq(1970, 2010, 10)) %>% 
  
  ggplot(aes(x = Year, y = theta, color = Specification)) +
  geom_line(size = 0.5) +
  facet_wrap(Country ~ .) +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(x = "Year", y = "Labor share")
```

![](gamma_files/figure-gfm/gamma_diffA_plot-1.png)<!-- -->

Decreasing ![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma
"\\gamma") does not improve the model performance. The predictions are
almost the same whatever the specification of
![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma "\\gamma").

### Same scale parameter (A)

``` r
# Initialize final dataset for gamma comparison
final_gamma_sameA = data.frame(matrix(ncol = ncol(data)+ ncol(param) -2 + 1, nrow = 0)) %>%
  setNames(c("Specification", names(data), names(param)[-c(1,2)]))

## Find A for all, from baseline estimation
data = data_save %>% 
    # Re-initialize data and define specification of the model
    mutate(Specification = ".cst") %>% 
    # Put Specification as 1st column
    select(Specification, everything()) %>% 
    # Select gamma / omega / beta : choose between ".cst" / "0" / "1" / "2"
    gob_finder(param, gamma_spe = ".cst") %>% 
    # No BTC
    mutate(AK = 1, AL = 1) %>% 
    # Demography
    demo_changer(AFinder = TRUE)


# A Finder script
source("./script/sim_AFinder.R")

# Assign A scale parameter to param_base
param$A = data$A

for(gamma_spe in gamma_specification){
  
  # Prepare data
  data = data_save %>% 
    # Re-initialize data and define specification of the model
    mutate(Specification = gamma_spe) %>% 
    # Put Specification as 1st column
    select(Specification, everything()) %>% 
    # Select gamma / omega / beta : choose between ".cst" / "0" / "1" / "2"
    gob_finder(param, gamma_spe = gamma_spe) %>% 
    # No BTC
    mutate(AK = 1, AL = 1) %>% 
    # Demography
    demo_changer(AFinder = TRUE)
  
  # Empty dataframe for results
  result = data.frame(matrix(ncol = ncol(data), nrow = 0)) %>%
    setNames(names(data))
  
  # Simulate model for country_set
  for(country in country_set){
    
    result = data %>%
      subset(Country == country) %>% 
      model(time = 2) %>% 
      rbind(result, .)
    
  }
  
  # Regroup results
  final_gamma_sameA = rbind(final_gamma_sameA, result)
  
}

# Write final_gamma2
write.csv(final_gamma_sameA, file.path(loc_sim, "final_gamma_sameA.csv"), row.names = FALSE)
```

``` r
final_gamma_sameA %>% 
  select(Specification, Country, Year, theta) %>% 
  subset(Year %in% seq(1970, 2010, 10)) %>% 
  
  ggplot(aes(x = Year, y = theta, color = Specification)) +
  geom_line(size = 0.5) +
  facet_wrap(Country ~ ., scale = "free") +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(x = "Year", y = "Labor share")
```

![](gamma_files/figure-gfm/gamma_sameA_plot-1.png)<!-- -->

Decreasing ![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma
"\\gamma") does not improve the model performance. The predictions are
almost the same whatever the specification of
![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma "\\gamma").

## Conclusion

Changing ![\\gamma](https://latex.codecogs.com/png.latex?%5Cgamma
"\\gamma") does not improve the results. Relative bargaining power has a
small impact in the model prediction. However, it may mean that the
outside option is the key determinant to improve model predictions.
