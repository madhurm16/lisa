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

``` r
# Write df
write.csv(df, file.path(loc_sim, "data.csv"), row.names = FALSE)
```

### Data for simulation

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
data = data_base %>% 
  # No biased technical change
  mutate(AK = 1, AL = 1) %>% 
  # Population dynamics
  demo_changer(init_spe = TRUE)
```

## Parameter calibration

The discount factor *alpha* is set to 0.669. The relative bargaining
power of the union *gamma* is set to 0.5. *For more details, please
consult “gamma.md” file.*

``` r
param_base = data.frame("Country" = rep(c("France", "United States"), each = length(sim)),
                   "Year" = sim, "alpha" = (0.99)^40, "gamma" = 0.5)
```

### Production function

I estimate the elasticity of substitution between capital and labor
using the first order conditions from the profit maximization.
Estimation are done for France and United States between 1970 and 2010,
using Penn World Table 9.1 data. *For more details, please consult
“sigma.md” file.*

``` r
# Dataframe for estimation
est_sigma = df %>% 
  group_by(Country) %>% 
  mutate(k_log = log(k),
         THETA_log = log(theta/(1-theta)),
         t_diff = Year - first(Year)) %>% 
  select(Country, Year, k_log, THETA_log, t_diff) %>% 
  subset(Year %in% c(1970:2010)) %>% 
  ungroup()
```

#### France

``` r
### Regression : one sigma for the whole period
  ols.france_all = est_sigma %>% 
    subset(Country == "France") %>% 
    lm(THETA_log ~ k_log + t_diff, data = .)
  # Report sigma
    sigma.fr_all = 1/(1 + ols.france_all$coefficients[2]) %>% unname()

### Regression : sigma before the change in the regime (1985)
  ols.france_before = est_sigma %>% 
    subset(Country == "France" & Year <= 1985) %>% 
    lm(THETA_log ~ k_log, data = .)
# Report sigma
  # Significant
    # sigma.fr_before = 1/(1 + ols.france_before$coefficients[2]) %>% unname()
  # Not significant => sigma.fr_before = 1
    sigma.fr_before = 1

### Regression : sigma after the change in the regime (1985)
  ols.france_after = est_sigma %>%
    subset(Country == "France" & Year > 1985) %>% 
    lm(THETA_log ~ k_log, data = .)
  # Report sigma
    sigma.fr_after = 1/(1 + ols.france_after$coefficients[2]) %>% unname()
    
### Report all sigmas
  print(paste0("One for all : ", round(sigma.fr_all,3)))
```

    ## [1] "One for all : 1.279"

``` r
  print(paste0("Before 1985 : ", round(sigma.fr_before,3)))
```

    ## [1] "Before 1985 : 1"

``` r
  print(paste0("After 1985 : ", round(sigma.fr_after,3)))
```

    ## [1] "After 1985 : 1.321"

#### United States

``` r
### Regression : one sigma for the whole period
  ols.us_all = est_sigma %>% 
    subset(Country == "United States") %>% 
    lm(THETA_log ~ k_log, data = .)
  # Report Sigma
  sigma.us_all = 1/(1 + ols.us_all$coefficients[2]) %>% unname()
  print(paste0("One for all : ", round(sigma.us_all,3)))
```

    ## [1] "One for all : 1.234"

#### Summary

``` r
# Add production function parameters
param_base = param_base %>%
  mutate("phi" = rep(1 - df$theta[df$Year == 1970], each = length(sim)), 
         "sigma" = ifelse(Country == "France", sigma.fr_all, sigma.us_all),
         "a" = NA)

# Visualization
param_base %>% subset(Year == 1970) %>% select(Country, phi, sigma, a)
```

    ##          Country       phi    sigma  a
    ## 1         France 0.2702488 1.278821 NA
    ## 13 United States 0.3247109 1.233652 NA

### Preferences

The relative per-capita influence of old households *omega* is deduced
such that the capital-to-labor ratio is matched in 1970. The preference
for government health expenditure *beta* is deduced such that the tax
rate in 1970 is matched.

``` r
# Compute omega and beta
param_base = param_base %>%
  merge(., df) %>% 
  mutate(X = (sigma + (1-phi)/phi*(1-gamma*(1-sigma))/gamma*k^((1-sigma)/sigma))^(-1),
         eta = (1-phi)/phi*k^((1-sigma)/sigma) * ((Ny/K*k-1)*exp(-X) + 1),
         omega = n/p*(1+alpha*p1)/eta,
         beta = 1/(1 - tau)/(1-theta) - 1 - eta) %>% 
  group_by(Country) %>% 
  mutate(omega = first(omega),
         beta = first(beta)) %>% 
  select(Country, Year, alpha, gamma, phi, sigma, a, omega, beta) %>% 
  ungroup()

# Visualization
param_base %>% subset(Year == 1970) %>% select(Country, omega, beta)
```

    ## # A tibble: 2 x 3
    ##   Country       omega  beta
    ##   <fct>         <dbl> <dbl>
    ## 1 France        0.983 0.739
    ## 2 United States 1.53  0.138

### Scale parameter

The scale parameter *A* is decuded using grid search to match the
average labor share between 2008 and 2012.

``` r
data = data_base %>% 
  # No biased technical change
  mutate(AK = 1, AL = 1) %>% 
  # Add parameters
  merge(param_base) %>% 
  # Demography
  demo_changer(AFinder = TRUE)

# A Finder script
source("./script/sim_AFinder.R")

# Assign A scale parameter to param_base
param_base$A = data$A
```

### Summary

All parameters are constant over time.

``` r
# Visualization
param_base %>% subset(Year == 1970) %>% select(-Year, -a) 
```

    ## # A tibble: 2 x 8
    ##   Country       alpha gamma   phi sigma omega  beta     A
    ##   <fct>         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 France        0.669   0.5 0.270  1.28 0.983 0.739  28.2
    ## 2 United States 0.669   0.5 0.325  1.23 1.53  0.138  22.8

``` r
# Assign values
param = param_base 
```

## Simulation

``` r
breaks_10_years = seq(1970, 2080, 10)
labs_20_years = as.vector(rbind(seq(1970, 2080, 20), rep("", length(breaks_10_years)/2)))
scale_graph = 1920/1080
```

### Demography : break years

Prepare the dataset for the counterfactual analysis.

``` r
breakers = demo %>% 
  ## Compute demographic data for 1960
  subset(Year == 1960) %>% 
  mutate(Sequence = NA, Period = NA, Ny = NA, No = NA, p1 = NA) %>% 
  select(Country, Year, Sequence, Period, Ny, No, n, p, p1) %>% 
  rbind(., data_base %>% 
          select(Country, Year, Sequence, Period, Ny, No, n, p, p1) %>% 
          subset(Year == 2000) %>% 
          ungroup()) %>% 
  group_by(Country) %>% 
  mutate(p1 = ifelse(Year == 1960, dplyr::lead(p), p1),
         Ny = ifelse(Year == 1960, dplyr::lead(Ny)/dplyr::lead(n), Ny),
         No = ifelse(Year == 1960, p/n*Ny, No),
         Sequence = ifelse(Year == 1960, 4, Sequence),
         Period = ifelse(Year == 1960, 0, Period)) %>% 
  subset(Year == 1960) %>% 
  ungroup() %>% 
  ## Demographic data for 1960 : done
  rbind(., data_base %>% 
          select(Country, Year, Sequence, Period, Ny, No, n, p, p1) %>% 
          ungroup()) %>% 
  arrange(Country) %>% 
  merge(., param_base %>% select(Country, alpha, omega) %>% unique()) %>% 
  group_by(Country, Sequence) %>% 
  mutate(eta = n/p*(1+alpha*p1)/omega,
         Ny = ifelse(Period == 2, lag(Ny,1)*n, Ny),
         No = ifelse(Period == 2, lag(Ny,1)*p, No),
         Ny = ifelse(Period == 3, lag(Ny,1)*n, Ny),
         No = ifelse(Period == 3, lag(Ny,1)*p, No),) %>% 
  select(-alpha, -omega) %>% 
  ungroup()

# Visualization
breakers
```

    ## # A tibble: 26 x 10
    ##    Country  Year Sequence Period    Ny    No     n     p    p1   eta
    ##    <fct>   <int>    <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 France   1960        4      0  1.32 0.433 1.14  0.372 0.525  4.20
    ##  2 France   1970        1      1  1.49 0.546 1.13  0.417 0.583  3.85
    ##  3 France   1980        2      1  1.62 0.516 1.39  0.444 0.613  4.50
    ##  4 France   1990        3      1  1.76 0.629 1.36  0.486 0.667  4.13
    ##  5 France   2000        4      1  1.82 0.694 1.38  0.525 0.683  3.89
    ##  6 France   2010        1      2  1.97 0.867 1.33  0.583 0.685  3.38
    ##  7 France   2020        2      2  1.84 0.992 1.13  0.613 0.707  2.78
    ##  8 France   2030        3      2  1.86 1.17  1.05  0.667 0.743  2.41
    ##  9 France   2040        4      2  1.82 1.24  0.998 0.683 0.774  2.26
    ## 10 France   2050        1      3  1.92 1.35  0.974 0.685 0.801  2.22
    ## # ... with 16 more rows

### Baseline model

``` r
# Specification
spe = "baseline"

# Initialize result dataset
final = data.frame(matrix(ncol = ncol(data)+ ncol(param) -2 + 1, nrow = 0)) %>%
  setNames(c("Specification", names(data), names(param)[-c(1,2)]))

# Prepare data for simulation
data = data_base %>% 
  # Merge with parameters
  merge(param) %>% 
  # No BTC
  mutate(AK = 1, AL = 1) %>% 
  # Demography
  demo_changer() %>% 
  # Define specification model
  mutate(Specification = spe) %>%
  # Reorder variables
  select(Specification, everything())

# Prepare result data frame to loop on
result = final

# Simulate the baseline model for each country
for(country in country_set){
    
    result = data %>%
      subset(Country == country) %>% 
      model(time = 3) %>% 
      rbind(result, .)
    
}

# Regroup result
baseline = result

# Write final_baseline
write.csv(baseline, file.path(loc_sim, paste0("final_", spe, ".csv")), row.names = FALSE)
```

#### Performance

``` r
# Prepare data and regression
perf = baseline %>% 
  subset(Specification == "baseline") %>% 
  select(Country, Year, theta) %>% 
  merge(., 
        df %>% select(Country, Year, theta) %>% setNames(c("Country", "Year", "labsh")),
        all = TRUE
        ) %>% 
  interpol_group() %>% 
  filter(complete.cases(.)) %>% 
  mutate(Year1 = Year - 1970 + 1,
         Year2 = Year1^2,
         Year3 = Year1^3,
         # Year4 = Year1^4,
         # Year5 = Year1^5,
         labsh.fitted = lm(data = .,
                           formula = labsh 
                           ~ Year1*Country 
                           + Year2*Country
                           + Year3*Country 
                           # + Year4*Country 
                           # + Year5*Country 
                           - Country) %>%
           fitted.values() %>% unname())
```

    ## [1] "Number of NA values removed for the variable theta: 198 on 238 (83.2%)"
    ## [1] "Number of NA values removed for the variable labsh: 0 on 166 (0%)"

``` r
# Core plot
perf_plot = perf %>% 
  select(Country, Year, theta, labsh, labsh.fitted) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = variable)) +
  geom_line() +
  facet_wrap(Country ~ .) +
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.direction = "vertical",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Year", y = "Labor income share")

## Paper version
perf_plot +
  scale_linetype_manual(name = "",
                     breaks = c("theta", "labsh", "labsh.fitted"), labels = c("Model", "Data", "Smoothed data"),
                     values = c("dotted", "solid", "dashed"))
```

![](main_files/figure-gfm/sim_bmodel_perf-1.png)<!-- -->

``` r
# Performance regression France
perf %>% 
  subset(Country == "France") %>% 
  lm(labsh.fitted ~ theta, data = .) %>% 
  summary()
```

    ## 
    ## Call:
    ## lm(formula = labsh.fitted ~ theta, data = .)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0287146 -0.0135657  0.0004674  0.0125856  0.0249528 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.19123    0.06732  -2.841  0.00669 ** 
    ## theta        1.28038    0.09913  12.916  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01462 on 46 degrees of freedom
    ## Multiple R-squared:  0.7839, Adjusted R-squared:  0.7792 
    ## F-statistic: 166.8 on 1 and 46 DF,  p-value: < 2.2e-16

``` r
# Performance regression United-States
perf %>% 
  subset(Country == "United States") %>% 
  lm(labsh.fitted ~ theta, data = .) %>% 
  summary()
```

    ## 
    ## Call:
    ## lm(formula = labsh.fitted ~ theta, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.019688 -0.012128 -0.001630  0.009962  0.041203 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.23666    0.08093   2.924  0.00534 ** 
    ## theta        0.62636    0.12462   5.026 8.05e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01389 on 46 degrees of freedom
    ## Multiple R-squared:  0.3545, Adjusted R-squared:  0.3405 
    ## F-statistic: 25.26 on 1 and 46 DF,  p-value: 8.052e-06

### Fill the gap : France 1980

``` r
# Change param for France
param = param %>% 
  mutate(sigma = ifelse(Country == "France",
                        ifelse(Year <= 1980, sigma.fr_before, sigma.fr_after), sigma))

# Change A
data = data_base %>%
  # No biased technical change
  mutate(AK = 1, AL = 1) %>%
  # Add parameters
  merge(param) %>%
  # Remove the previous A
  select(-A) %>%
  # Demography
  demo_changer(AFinder = TRUE)

# A Finder script
source("./script/sim_AFinder.R")

# Assign A scale parameter to param_base
param$A = data$A

# Visualization : New A
param %>% subset(Year == 1970) %>% select(Country, A)
```

    ## # A tibble: 2 x 2
    ##   Country           A
    ##   <fct>         <dbl>
    ## 1 France         23.9
    ## 2 United States  22.8

``` r
# Specification
spe = "fillgap"

# Initialize result dataset
fillgap = data.frame(matrix(ncol = ncol(data)+ ncol(param) -2 + 1, nrow = 0)) %>%
  setNames(c("Specification", names(data), names(param)[-c(1,2)]))

# Prepare data for simulation
data = data_base %>% 
  # Merge with parameters
  merge(param) %>% 
  # No BTC
  mutate(AK = 1, AL = 1) %>% 
  # Demography
  demo_changer() %>% 
  # Define specification model
  mutate(Specification = spe) %>%
  # Reorder variables
  select(Specification, everything())

# Simulate the fillthegap model for France
fillgap = data %>%
  subset(Country == "France") %>%
  model(time = 3)

# Write fillgap
write.csv(fillgap, file.path(loc_sim, paste0("final_", spe, ".csv")), row.names = FALSE)
```

``` r
## Core
bmodel_plot_LS7010 = baseline %>% 
  subset(Specification == "baseline") %>% 
  select(Country, Year, theta) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  mutate(variable = ifelse(Country == "France", "base.fr", "base.us")) %>% 
  rbind(.,
        fillgap %>% select(Country, Year, theta) %>% setNames(c("Country", "Year", "fill.fr")) %>% 
          melt(id.vars = c("Country", "Year"))) %>% 
  rbind(.,
        df %>% select(Country, Year, theta) %>% setNames(c("Country", "Year", "data")) %>% 
          melt(id.vars = c("Country", "Year"))
        ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(fill = ifelse(variable == "fill.fr", "fill", "other")) %>% 
  subset(Year %in% c(1970:2010)) %>% 
  # Trick to put data line under others
  mutate(variable = as.character(variable),
         variable = ifelse(variable == "data", "a.data", variable),
         variable = as.factor(variable)) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = variable, color = variable)) +
  geom_line() +
  facet_wrap(Country ~ ., scales = "fixed") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
bmodel_plot_LS7010 +
  scale_linetype_manual(breaks = c("base.fr", "base.us", "a.data", "fill.fr"), 
                        values = c("dashed", "dashed", "solid", "dotted") %>% 
                        setNames(c("base.fr", "base.us", "a.data", "fill.fr"))) +
  scale_color_manual(breaks = c("base.fr", "base.us", "a.data", "fill.fr"), 
                        values = rep("black", 4) %>% 
                        setNames(c("base.fr", "base.us", "a.data", "fill.fr"))) +
  ggsave(file.path(loc_result, "baseline7010.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/bmodel_plot_LS7010-1.png)<!-- -->

``` r
## Color version
bmodel_plot_LS7010 +
  scale_linetype_manual(breaks = c("base.fr", "base.us", "a.data", "fill.fr"), 
                        values = c("solid", "solid", "solid", "dashed") %>% 
                        setNames(c("base.fr", "base.us", "a.data", "fill.fr"))) +
  scale_color_manual(breaks = c("base.fr", "base.us", "a.data", "fill.fr"), 
                     values = c(brewer.pal(8, "Set1")[c(1,2)], "black", brewer.pal(8, "Set1")[1]) %>%
                       setNames(c("base.fr", "base.us", "a.data", "fill.fr"))) +
  ggsave(file.path(loc_result, "baseline7010_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/bmodel_plot_LS7010-2.png)<!-- -->

``` r
## Core
bmodel_plot_LS7080 = baseline %>% 
  subset(Specification == "baseline") %>% 
  select(Country, Year, theta) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  mutate(variable = ifelse(Country == "France", "base.fr", "base.us")) %>% 
  rbind(.,
        fillgap %>% select(Country, Year, theta) %>% setNames(c("Country", "Year", "fill.fr")) %>% 
          melt(id.vars = c("Country", "Year"))) %>% 
  rbind(.,
        df %>% select(Country, Year, theta) %>% setNames(c("Country", "Year", "data")) %>% 
          melt(id.vars = c("Country", "Year"))
        ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(fill = ifelse(variable == "fill.fr", "fill", "other")) %>% 
  subset(Year %in% c(1970:2080)) %>%
  # Trick to put data line under others
  mutate(variable = as.character(variable),
         variable = ifelse(variable == "data", "a.data", variable),
         variable = as.factor(variable)) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = variable, color = variable)) +
  geom_line() +
  facet_wrap(Country ~ ., scales = "fixed") +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
bmodel_plot_LS7080 +
  scale_linetype_manual(breaks = c("base.fr", "base.us", "a.data", "fill.fr"), 
                        values = c("dashed", "dashed", "solid", "dotted") %>% 
                        setNames(c("base.fr", "base.us", "a.data", "fill.fr"))) +
  scale_color_manual(breaks = c("base.fr", "base.us", "a.data", "fill.fr"),
                    values = rep("black", 4) %>%
                      setNames(c("base.fr", "base.us", "a.data", "fill.fr"))) +
  ggsave(file.path(loc_result, "baseline7080.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/bmodel_plot_LS7080-1.png)<!-- -->

``` r
## Color version
bmodel_plot_LS7080 +
  scale_color_manual(breaks = c("base.fr", "base.us", "a.data", "fill.fr"),
                    values = c(brewer.pal(8, "Set1")[c(1,2)], "black", brewer.pal(8, "Set1")[1]) %>%
                      setNames(c("base.fr", "base.us", "a.data", "fill.fr"))) +
  scale_linetype_manual(breaks = c("base.fr", "base.us", "a.data", "fill.fr"), 
                        values = c("solid", "solid", "solid", "dashed") %>% 
                        setNames(c("base.fr", "base.us", "a.data", "fill.fr"))) +
  ggsave(file.path(loc_result, "baseline7080_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/bmodel_plot_LS7080-2.png)<!-- -->

### Variables dynamics

``` r
loc_dev = file.path(loc_result, "deviation")

# Computation average growth rate of p on 10 years per country
baseline %>% 
  select(Country, Year, p) %>% 
  subset(Year >= 2010) %>% 
  group_by(Country) %>% 
  mutate(p = p/dplyr::lag(p) -1) %>% 
  summarize(mean(p, na.rm = T))
```

    ## # A tibble: 2 x 2
    ##   Country       `mean(p, na.rm = T)`
    ##   <fct>                        <dbl>
    ## 1 France                      0.0414
    ## 2 United States               0.0400

#### Demography

``` r
## Core
dev_demo7010 = baseline %>%
  # subset(Country == "France") %>% 
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>% 
  group_by(from) %>% 
  mutate(
    # Deviation from the 1970 steady state
         "n" = n/n[Year == 1970] -1,
         "p" = p/p[Year == 1970] -1,
         "p[+1]" = p1/p1[Year == 1970] -1,
         "eta" = eta/eta[Year == 1970] -1) %>% 
  ungroup() %>% 
  select(Specification, Country , Year,
         "Population~growth~(n)"= "n", 
         "Survival~rate~(p)"= "p", 
         "Exp.~survival~rate~(p[+1])"= "p[+1]", 
         "Youth~political~weight~(eta)" = "eta") %>% 
  subset(Year %in% c(1970:2010)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  subset(Specification == "baseline") %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_x_continuous(labels = c("1970", "", "1990", "", "2010")) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_demo7010 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_demo7010.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_demo7010-1.png)<!-- -->

``` r
## Color version
dev_demo7010 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_demo7010_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_demo7010-2.png)<!-- -->

``` r
## Paper version
dev_demo1080 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>% 
  group_by(from) %>% 
  mutate(
    # Deviation from the 2010 steady state
         "n" = n/n[Year == 2010] -1,
         "p" = p/p[Year == 2010] -1,
         "p[+1]" = p1/p1[Year == 2010] -1,
         "eta" = eta/eta[Year == 2010] -1) %>% 
  ungroup() %>% 
  select(Specification, Country , Year,
         "Population~growth~(n)"= "n", 
         "Survival~rate~(p)"= "p", 
         "Exp.~survival~rate~(p[+1])"= "p[+1]", 
         "Youth~political~weight~(eta)" = "eta") %>% 
  subset(Year %in% c(2010:2080)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  subset(Specification == "baseline") %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_x_continuous(breaks = seq(2010, 2080, 10), 
                     labels = c("2010","", "2030", "", "2050", "", "2070", "")) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "") 

## Paper version
dev_demo1080 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_demo1080.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_demo1080-1.png)<!-- -->

``` r
## Color version
dev_demo1080 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_demo1080_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_demo1080-2.png)<!-- -->

##### Public

``` r
## Core

dev_public7010 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>% 
  group_by(from) %>% 
  mutate(`b share` = b*u*Ny/tau/Y,
         `h share` = h*No/tau/Y,
         bh = b/h,
    # Deviation from the 1970 steady state
         "eta" = eta/eta[Year == 1970] -1,
         "tau" = tau/tau[Year == 1970] -1,
         "b" = b/b[Year == 1970]-1,
         "h" = h/h[Year == 1970]-1,
         "`b share`" = `b share`/`b share`[Year == 1970]-1,
         "`h share`" = `h share`/`h share`[Year == 1970]-1,
         "b/h" = bh/bh[Year == 1970]-1) %>% 
  ungroup() %>% 
  select(Specification, Country , Year,
         "Youth~political~weight~(eta)" = "eta", 
         "Tax~rate~(tau)" = "tau", 
         "Unemp.~benefits~share" = "`b share`") %>% 
  subset(Year %in% c(1970:2010)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_x_continuous(labels = c("1970", "", "1990", "", "2010")) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_public7010 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_public7010.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_public7010-1.png)<!-- -->

``` r
## Color version
dev_public7010 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_public7010_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_public7010-2.png)<!-- -->

``` r
## Core
dev_public1080 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>% 
  group_by(from) %>% 
  mutate(`b share` = b*u*Ny/tau/Y,
         `h share` = h*No/tau/Y,
          bh = b/h,
    # Deviation from the 2010 steady state
         "eta" = eta/eta[Year == 2010] -1,
         "tau" = tau/tau[Year == 2010] -1,
         "b" = b/b[Year == 2010]-1,
         "h" = h/h[Year == 2010]-1,
         "`b share`" = `b share`/`b share`[Year == 2010]-1,
         "`h share`" = `h share`/`h share`[Year == 2010]-1,
         "b/h" = bh/bh[Year == 2010]-1) %>% 
  ungroup() %>% 
  select(Specification, Country , Year,
         "Youth~political~weight~(eta)" = "eta", 
         "Tax~rate~(tau)" = "tau", 
         "Unemp.~benefits~share" = "`b share`") %>% 
  subset(Year %in% c(2010:2080)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_x_continuous(breaks = seq(2010,2080,10), 
                     labels = c("2010","", "2030", "", "2050", "", "2070", "")) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_public1080 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_public1080.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_public1080-1.png)<!-- -->

``` r
## Color version
dev_public1080 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_public1080_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_public1080-2.png)<!-- -->

##### Outside

``` r
## Core 
dev_outside7010 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  group_by(from) %>% 
  mutate(net = 1-tau,
         outside = b/(1-tau),
         # Deviation from the 1970 steady state
         "b" = b/b[Year == 1970] -1,
         "1-tau" = net/net[Year == 1970] -1,
         "b/(1-tau)" = outside/outside[Year == 1970] -1) %>% 
  ungroup() %>% 
  select(Specification, Country, Year,
         "Unemployment~benefits~(b)" = "b", 
         "Disposable~income~rate~(1-tau)" = "1-tau", 
         "Outside~option" = "b/(1-tau)") %>% 
  subset(Year %in% c(1970:2010)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_x_continuous(labels = c("1970", "", "1990", "", "2010")) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_outside7010 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_outside7010.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_outside7010-1.png)<!-- -->

``` r
## Color version
dev_outside7010 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_outside7010_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_outside7010-2.png)<!-- -->

``` r
## Core
dev_outside1080 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  group_by(from) %>% 
  mutate(net = 1-tau,
         outside = b/(1-tau),
         # Deviation from the 2010 steady state
         "b" = b/b[Year == 2010] -1,
         "1-tau" = net/net[Year == 2010] -1,
         "b/(1-tau)" = outside/outside[Year == 2010] -1) %>% 
  ungroup() %>% 
  select(Specification, Country, Year, 
         "Unemployment~benefits~(b)" = "b", 
         "Disposable~income~rate~(1-tau)" = "1-tau", 
         "Outside~option" = "b/(1-tau)") %>% 
  subset(Year %in% c(2010:2080)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_x_continuous(breaks = seq(2010,2080,10), 
                     labels = c("2010","", "2030", "", "2050", "", "2070", "")) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_outside1080 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_outside1080.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_outside1080-1.png)<!-- -->

``` r
## Color version
dev_outside1080 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_outside1080_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_outside1080-2.png)<!-- -->

##### Bargaining

``` r
## Core
dev_bargaining7010 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  group_by(from) %>% 
  mutate(outside = b/(1-tau),
         # Deviation from the 1970 steady state
         "b/(1-tau)" = outside/outside[Year == 1970] -1,
         "w" = w/w[Year == 1970] -1,
         "X" = X/X[Year == 1970] -1) %>% 
  ungroup() %>% 
  select(Specification, Country, Year,
         "Outside~option" = "b/(1-tau)", 
         "Wage~rate~(w)" = "w", 
         "Job~value~added~(X)" = "X") %>% 
  subset(Year %in% c(1970:2010)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_x_continuous(labels = c("1970", "", "1990", "", "2010")) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_bargaining7010 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_bargain7010.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_bargaining7010-1.png)<!-- -->

``` r
## Color version
dev_bargaining7010 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_bargain7010_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_bargaining7010-2.png)<!-- -->

``` r
## Core
dev_bargaining1080 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  group_by(from) %>% 
  mutate(outside = b/(1-tau),
         # Deviation from the 2010 steady state
         "eta" = eta/eta[Year == 2010] -1,
         "b/(1-tau)" = outside/outside[Year == 2010] -1,
         "w" = w/w[Year == 2010] -1,
         "X" = X/X[Year == 2010] -1) %>% 
  ungroup() %>% 
  select(Specification, Country, Year,
         "Outside~option" = "b/(1-tau)", 
         "Wage~rate~(w)" = "w", 
         "Job~value~added~(X)" = "X") %>% 
  subset(Year %in% c(2010:2080)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_x_continuous(breaks = seq(2010,2080,10), 
                     labels = c("2010","", "2030", "", "2050", "", "2070", "")) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_bargaining1080 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_bargain1080.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_bargaining1080-1.png)<!-- -->

``` r
## Color version
dev_bargaining1080 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_bargain1080_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_bargaining1080-2.png)<!-- -->

##### Production

``` r
## Core
dev_prod7010 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  group_by(from) %>%  
  mutate(
         # Deviation from the 1970 steady state
         "K" = K/K[Year == 1970] -1,
         "L" = L/L[Year == 1970] -1,
         "Y" = Y/Y[Year == 1970] -1) %>% 
  ungroup() %>% 
  select(Specification, Country, Year, 
         "Capital~stock~(K)" = "K", 
         "Labor~(L)" = "L", 
         "Production~(Y)" = "Y") %>% 
  subset(Year %in% c(1970:2010)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  scale_x_continuous(labels = c("1970", "", "1990", "", "2010")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_prod7010 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_prod7010.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_prod7010-1.png)<!-- -->

``` r
## Color version
dev_prod7010 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_prod7010_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_prod7010-2.png)<!-- -->

``` r
## Core
dev_prod1080 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  group_by(from) %>%  
  mutate(
         # Deviation from the 2010 steady state
         "K" = K/K[Year == 2010] -1,
         "L" = L/L[Year == 2010] -1,
         "Y" = Y/Y[Year == 2010] -1) %>% 
  ungroup() %>% 
  select(Specification, Country, Year, 
         "Capital~stock~(K)" = "K",
         "Labor~(L)" = "L",
         "Production~(Y)" = "Y") %>% 
  subset(Year %in% c(2010:2080)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(2010,2080,10), 
                     labels = c("2010","", "2030", "", "2050", "", "2070", "")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_prod1080 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_prod1080.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_prod1080-1.png)<!-- -->

``` r
## Color version
dev_prod1080 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_prod1080_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_prod1080-2.png)<!-- -->

##### Unemployment

``` r
## Core
dev_unemp7010 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  group_by(from) %>%  
  mutate(
         # Deviation from the 1970 steady state
         "N^y" = Ny/Ny[Year == 1970] -1,
         "L" = L/L[Year == 1970] -1,
         "u" = u/u[Year == 1970] -1) %>% 
  ungroup() %>% 
  select(Specification, Country, Year,
         "Young~households~(N^y)" = "N^y",
         "Labor~(L)" = "L",
         "Unemployment~rate~(u)" = "u") %>% 
  subset(Year %in% c(1970:2010)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  scale_x_continuous(labels = c("1970", "", "1990", "", "2010")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_unemp7010 + 
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_unemp7010.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_unemp7010-1.png)<!-- -->

``` r
## Color version
dev_unemp7010 + 
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_unemp7010_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_unemp7010-2.png)<!-- -->

``` r
## Core
dev_unemp1080 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  group_by(from) %>%  
  mutate(
         # Deviation from the 2010 steady state
         "N^y" = Ny/Ny[Year == 2010] -1,
         "L" = L/L[Year == 2010] -1,
         "u" = u/u[Year == 2010] -1) %>% 
  ungroup() %>% 
  select(Specification, Country, Year, 
         "Young~households~(N^y)" = "N^y", 
         "Labor~(L)" = "L", 
         "Unemployment~rate~(u)" = "u") %>% 
  subset(Year %in% c(2010:2080)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(2010,2080,10), 
                     labels = c("2010","", "2030", "", "2050", "", "2070", "")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_unemp1080 + 
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_unemp1080.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_unemp1080-1.png)<!-- -->

``` r
## Color version
dev_unemp1080 + 
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_unemp1080_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_unemp1080-2.png)<!-- -->

##### Labor share

``` r
## Core
dev_laborshare7010 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  group_by(from) %>%  
  mutate("YL" = Y/L,
         # Deviation from the 1970 steady state
         "w" = w/w[Year == 1970] -1,
         "u" = u/u[Year == 1970] -1,
         "Y/L" = YL/YL[Year == 1970] -1,
         "theta" = theta/theta[Year == 1970] -1) %>% 
  ungroup() %>% 
  select(Specification, Country, Year,
         "Wage~rate~(w)" = "w", 
         "Production-per-worker~(Y/L)" = "Y/L", 
         "Labor~share~(theta)" = "theta") %>% 
  subset(Year %in% c(1970:2010)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  scale_x_continuous(labels = c("1970", "", "1990", "", "2010")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_laborshare7010 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_laborshare7010.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_laborshare7010-1.png)<!-- -->

``` r
## Color version
dev_laborshare7010 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_laborshare7010_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_laborshare7010-2.png)<!-- -->

``` r
## Core
dev_laborshare1080 = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  group_by(from) %>%  
  mutate("YL" = Y/L,
         # Deviation from the 2010 steady state
         "w" = w/w[Year == 2010] -1,
         "u" = u/u[Year == 2010] -1,
         "Y/L" = YL/YL[Year == 2010] -1,
         "theta" = theta/theta[Year == 2010] -1) %>% 
  ungroup() %>% 
  select(Specification, Country, Year,
         "Wage~rate~(w)" = "w", 
         "Production-per-worker~(Y/L)" = "Y/L", 
         "Labor~share~(theta)" = "theta") %>% 
  subset(Year %in% c(2010:2080)) %>% 
  melt(id.vars = c("Specification", "Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(variable ~ ., scales = "fixed", nrow = 1, labeller = label_parsed) +
  scale_x_continuous(breaks = seq(2010, 2080, 10),
                     labels = c("2010","", "2030", "", "2050", "", "2070", "")) +
  scale_linetype_manual(breaks = c("baseline", "fillgap"), values = c("solid", "dashed")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
dev_laborshare1080 +
  scale_color_grey(breaks = c("France", "United States"), start = 0.2, end = 0.8) +
  ggsave(file.path(loc_dev, "dev_laborshare1080.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_laborshare1080-1.png)<!-- -->

``` r
## Color version
dev_laborshare1080 +
  scale_color_manual(breaks = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)]) +
  ggsave(file.path(loc_dev, "dev_laborshare1080_color.png"), width = scale_graph*5, height = scale_graph*5/3)
```

![](main_files/figure-gfm/dev_laborshare1080-2.png)<!-- -->

``` r
# Clean the environment of all "dev_" graphs
rm(list = ls(pattern = "dev_"))
```

### Redistribution

I only consider France in the fillgap specification, so the one with a
break in the regime of the elasticity of substitution. I also consider
the baseline specification for the United-States.

``` r
# Location save graphs
loc_redis = file.path(loc_result, "redistribution")

# Data selection
redis = baseline %>%
  rbind(fillgap) %>% 
  mutate(from = interaction(Specification, Country)) %>%
  subset(from != "baseline.France") %>% 
  select(-from) %>% 
  group_by(Country) %>% 
  
  mutate(
    # Step 1
    wL = w*L,
    rK = r*K,
    # Step 2
    wL_net = (1-tau)*wL,
    rK_net = (1-tau)*rK,
    gov_rev = tau*Y,
    # Step 3
    unemp_ben = b*u*Ny,
    health_spen = h*No,
    # Step 4
    C1 = 1/(1+alpha*p1)*(wL_net+unemp_ben),
    Saving = alpha*p1/(1+alpha*p1)*(wL_net+unemp_ben),
    C2 = rK_net,
    # Incomes
    inc_y = wL_net + unemp_ben,
    inc_o = rK_net,
    # Labor share
    theta = wL/Y
    ) %>% 
  select(Country, Year, wL, rK, wL_net, rK_net, gov_rev, unemp_ben, health_spen,
         C1, Saving, C2, inc_y, inc_o, Y, theta, eta) %>% 
  ungroup()
```

``` r
## Core
redis_step1 = redis %>% 
  mutate(wL_bis = wL) %>% 
  select(Country, Year, rK, wL, wL_bis) %>% 
  melt(id.vars = c("Country", "Year", "wL_bis")) %>% 
  
  ggplot(aes(x = Year)) +
  geom_col(aes(y = value, fill = variable)) +
  geom_line(aes(y = wL_bis), linetype = "dashed", color = "black") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic() +
  theme(legend.position = "bottom", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
redis_step1 +
  scale_fill_grey(name = "", 
                  breaks = c("rK", "wL"), labels = c("Capital income", "Labor income"), 
                  start = 0.5, end = 0.9)
```

![](main_files/figure-gfm/redis_step1-1.png)<!-- -->

``` r
## Color version
redis_step1 +
  scale_fill_manual(name = "",
                    breaks = c("rK", "wL"), labels = c("Capital income", "Labor income"),
                    values = brewer.pal(11, "Set3")[c(4,5)], na.value = "black")
```

![](main_files/figure-gfm/redis_step1-2.png)<!-- -->

``` r
## Core
redis_step1_stacked = redis %>% 
  mutate(wL = wL/Y,
         rK = rK/Y) %>% 
  select(Country, Year, rK, wL, theta) %>% 
  melt(id.vars = c("Country", "Year", "theta")) %>% 
  
  ggplot(aes(x = Year)) +
  geom_col(aes(y = value, fill = variable)) +
  geom_line(aes(y = theta), linetype = "dashed", color = "black") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic() +
  theme(legend.position = "bottom", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank()) +
  labs(x = "", y = "Share of GDP")

## Paper version
redis_step1_stacked +
  scale_fill_grey(name = "", 
                  breaks = c("rK", "wL"), labels = c("Capital income", "Labor income"), 
                  start = 0.5, end = 0.9) +
  ggsave(file.path(loc_redis, "redis_step1_stacked.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/redis_step1_stacked-1.png)<!-- -->

``` r
## Color version
redis_step1_stacked +
  scale_fill_manual(name = "",
                    breaks = c("rK", "wL"), labels = c("Capital income", "Labor income"),
                    values = brewer.pal(11, "Set3")[c(4,5)], na.value = "black") +
  ggsave(file.path(loc_redis, "redis_step1_stacked_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/redis_step1_stacked-2.png)<!-- -->

``` r
## Core
redis_step2 = redis %>% 
  select(Country, Year, rK_net, gov_rev, wL_net, wL) %>% 
  melt(id.vars = c("Country", "Year", "wL")) %>% 
  
  ggplot(aes(x = Year)) +
  geom_col(aes(y = value, fill = variable)) +
  geom_line(aes(y = wL), linetype = "dashed", color = "black") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic() +
  theme(legend.position = "bottom", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
redis_step2 +
  scale_fill_grey(name = "", 
                  breaks = c("rK_net", "gov_rev", "wL_net"), 
                  labels = c("Net capital income", "Government revenue", "Net labor income"), 
                  start = 0.5, end = 0.9)
```

![](main_files/figure-gfm/redis_step2-1.png)<!-- -->

``` r
## Color version
redis_step2 + 
  scale_fill_manual(name = "", 
                    breaks = c("rK_net", "gov_rev", "wL_net"), 
                    labels = c("Net capital income", "Government revenue", "Net labor income"),
                    values = brewer.pal(11, "Set3")[c(4,7,5)], na.value = "black")
```

![](main_files/figure-gfm/redis_step2-2.png)<!-- -->

``` r
## Core
redis_step2_stacked = redis %>% 
  mutate(wL_net = wL_net/Y,
         gov_rev = gov_rev/Y,
         rK_net = rK_net/Y) %>% 
  select(Country, Year, rK_net, gov_rev, wL_net, theta) %>% 
  melt(id.vars = c("Country", "Year", "theta")) %>% 
  
  ggplot(aes(x = Year)) +
  geom_col(aes(y = value, fill = variable)) +
  geom_line(aes(y = theta), linetype = "dashed", color = "black") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic() +
  theme(legend.position = "bottom", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank()) +
  labs(x = "", y = "Share of GDP")

## Paper version
redis_step2_stacked +
  scale_fill_grey(name = "", 
                  breaks = c("rK_net", "gov_rev", "wL_net"), 
                  labels = c("Net capital income", "Government revenue", "Net labor income"), 
                  start = 0.5, end = 0.9)
```

![](main_files/figure-gfm/redis_step2_stacked-1.png)<!-- -->

``` r
  ggsave(file.path(loc_redis, "redis_step2_stacked.png"), width = scale_graph*5, height = scale_graph*5/2)

## Color version
redis_step2_stacked +
  scale_fill_manual(name = "", 
                    breaks = c("rK_net", "gov_rev", "wL_net"), 
                    labels = c("Net capital income", "Government revenue", "Net labor income"),
                    values = brewer.pal(11, "Set3")[c(4,7,5)], na.value = "black") +
  ggsave(file.path(loc_redis, "redis_step2_stacked_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/redis_step2_stacked-2.png)<!-- -->

``` r
## Core
redis_step3 = redis %>% 
  select(Country, Year, rK_net, health_spen, unemp_ben, wL_net, wL) %>% 
  melt(id.vars = c("Country", "Year", "wL")) %>% 
  
  ggplot(aes(x = Year)) +
  geom_col(aes(y = value, fill = variable)) +
  geom_line(aes(y = wL), linetype = "dashed", color = "black") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic() +
  theme(legend.position = "bottom", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
redis_step3 +
  scale_fill_grey(name = "", 
                  breaks = c("rK_net", "health_spen", "unemp_ben", "wL_net"),
                  labels = c("Net capital income", "Health spending", 
                             "Unemployment spending", "Net labor income"),
                  start = 0.5, end = 0.9)
```

![](main_files/figure-gfm/redis_step3-1.png)<!-- -->

``` r
## Color version
redis_step3 + 
  scale_fill_manual(name = "", 
                  breaks = c("rK_net", "health_spen", "unemp_ben", "wL_net"),
                  labels = c("Net capital income", "Health spending", 
                             "Unemployment spending", "Net labor income"),
                    values = brewer.pal(11, "Set3")[c(4,6,3,5)], na.value = "black")
```

![](main_files/figure-gfm/redis_step3-2.png)<!-- -->

``` r
## Core
redis_step3_stacked = redis %>% 
  mutate(wL_net = wL_net/Y,
         unemp_ben = unemp_ben/Y,
         health_spen = health_spen/Y,
         rK_net = rK_net/Y) %>% 
  select(Country, Year, rK_net, health_spen, unemp_ben, wL_net, theta) %>% 
  melt(id.vars = c("Country", "Year", "theta")) %>%
  
  ggplot(aes(x = Year)) +
  geom_col(aes(y = value, fill = variable)) +
  geom_line(aes(y = theta), linetype = "dashed", color = "black") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic() +
  theme(legend.position = "bottom", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank()) +
  labs(x = "", y = "Share of GDP")

## Paper version
redis_step3_stacked +
  scale_fill_grey(name = "", 
                  breaks = c("rK_net", "health_spen", "unemp_ben", "wL_net"),
                  labels = c("Net capital income", "Health spending", 
                             "Unemployment spending", "Net labor income"),
                  start = 0.5, end = 0.9)
```

![](main_files/figure-gfm/redis_step3_stacked-1.png)<!-- -->

``` r
  ggsave(file.path(loc_redis, "redis_step3_stacked.png"), width = scale_graph*5, height = scale_graph*5/2)

## Color version
redis_step3_stacked +
  scale_fill_manual(name = "", 
                    breaks = c("rK_net", "health_spen", "unemp_ben", "wL_net"), 
                    labels = c("Net capital income", "Health spending", 
                               "Unemployment spending", "Net labor income"),
                    values = brewer.pal(11, "Set3")[c(4,6,3,5)], na.value = "black") +
  ggsave(file.path(loc_redis, "redis_step3_stacked_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/redis_step3_stacked-2.png)<!-- -->

``` r
## Core
redis_step3 = redis %>% 
  select(Country, Year, C2, health_spen, Saving, C1, wL) %>% 
  melt(id.vars = c("Country", "Year", "wL")) %>% 
  
  ggplot(aes(x = Year)) +
  geom_col(aes(y = value, fill = variable)) +
  geom_line(aes(y = wL), linetype = "dashed", color = "black") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic() +
  theme(legend.position = "bottom", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
redis_step3 +
  scale_fill_grey(name = "", 
                  breaks = c("C2", "health_spen", "Saving", "C1"),
                  labels = c("Old HH. cons.", "Health spending", "Savings", "Young HH. cons."),
                  start = 0.5, end = 0.9)
```

![](main_files/figure-gfm/redis_step4-1.png)<!-- -->

``` r
## Color version
redis_step3 + 
  scale_fill_manual(name = "", 
                  breaks = c("C2", "health_spen", "Saving", "C1"),
                  labels = c("Old HH. cons.", "Health spending", "Savings", "Young HH. cons."),
                  values = brewer.pal(11, "Set3")[c(4,6,8,10)], na.value = "black")
```

![](main_files/figure-gfm/redis_step4-2.png)<!-- -->

``` r
## Core
redis_step4_stacked = redis %>% 
  mutate(C2 = C2/Y,
         health_spen = health_spen/Y,
         Saving = Saving/Y,
         C1 = C1/Y) %>% 
  select(Country, Year, C2, health_spen, Saving, C1, theta) %>% 
  melt(id.vars = c("Country", "Year", "theta")) %>% 
  
  ggplot(aes(x = Year)) +
  geom_col(aes(y = value, fill = variable)) +
  geom_line(aes(y = theta), linetype = "dashed", color = "black") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic() +
  theme(legend.position = "bottom", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank()) +
  labs(x = "", y = "Share of GDP")

## Paper version
redis_step4_stacked +
  scale_fill_grey(name = "", 
                  breaks = c("C2", "health_spen", "Saving", "C1"),
                  labels = c("Old HH. cons.", "Health spending", "Savings", "Young HH. cons."),
                  start = 0.5, end = 0.9)
```

![](main_files/figure-gfm/redis_step4_stacked-1.png)<!-- -->

``` r
  ggsave(file.path(loc_redis, "redis_step4_stacked.png"), width = scale_graph*5, height = scale_graph*5/2)

## Color version
redis_step4_stacked +
  scale_fill_manual(name = "", 
                  breaks = c("C2", "health_spen", "Saving", "C1"),
                  labels = c("Old HH. cons.", "Health spending", "Savings", "Young HH. cons."),
                  values = brewer.pal(11, "Set3")[c(4,6,8,10)], na.value = "black") +
  ggsave(file.path(loc_redis, "redis_step4_stacked_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/redis_step4_stacked-2.png)<!-- -->

``` r
## Core
ls_raw_vs_net = redis %>% 
  mutate(net_ls = (wL_net + unemp_ben)/(Y - health_spen)) %>% 
  select(Country, Year, theta, net_ls) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = variable, color = Country)) +
  geom_line() +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  scale_linetype_manual(name = "Labor share", breaks = c("theta", "net_ls"),
                        labels = c("Raw", "Net"), values = c("solid", "dashed"), 
                        guide = FALSE) +
  theme_classic(base_size = 14) +
  labs(x = "", y = "") +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(), axis.title.y = element_blank())

## Paper version
ls_raw_vs_net +
  scale_color_manual(name = "", breaks = c("France", "United States"),
                     labels = c("France", "United States"), values = c("black", "black"), 
                     guide = FALSE) +
  ggsave(file.path(loc_redis, "ls_raw_vs_net.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/ls_raw_vs_net-1.png)<!-- -->

``` r
## Color version
ls_raw_vs_net +
  scale_color_manual(name = "", breaks = c("France", "United States"),
                     labels = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)], 
                     guide = FALSE) +
  ggsave(file.path(loc_redis, "ls_raw_vs_net_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/ls_raw_vs_net-2.png)<!-- -->

``` r
## Core
dev_ls_raw_vs_net = redis %>% 
  group_by(Country) %>% 
  mutate(
    net_ls = (wL_net + unemp_ben)/(Y),
    theta = theta/theta[Year == 1970] -1,
    net_ls = net_ls/net_ls[Year == 1970] -1,
    ) %>% 
  select(Country, Year, theta, net_ls) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = variable, color = Country)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  scale_linetype_manual(name = "Labor share", breaks = c("theta", "net_ls"),
                        labels = c("Raw", "Net"), values = c("solid", "dashed"),
                        guide = FALSE) +
  theme_classic(base_size = 14) +
  labs(x = "", y = "") +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(), axis.title.y = element_blank())

## Paper version
dev_ls_raw_vs_net +
  scale_color_manual(name = "", breaks = c("France", "United States"),
                     labels = c("France", "United States"), values = c("black", "black"), 
                     guide = FALSE) +
  ggsave(file.path(loc_redis, "dev_ls_raw_vs_net.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/dev_ls_raw_vs_net-1.png)<!-- -->

``` r
## Color version
dev_ls_raw_vs_net +
  scale_color_manual(name = "", breaks = c("France", "United States"),
                     labels = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)], 
                     guide = FALSE) +
  ggsave(file.path(loc_redis, "dev_ls_raw_vs_net_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/dev_ls_raw_vs_net-2.png)<!-- -->

``` r
## Core
incratio_raw_vs_net = redis %>% 
  select(Country, Year, inc_y, inc_o, wL, rK, health_spen, eta) %>% 
  mutate(inc_ratio = inc_y/inc_o,
         inc_ratio_with_h = inc_y/(inc_o + health_spen),
         lab_to_cap_ratio = wL/rK) %>% 
  select(Country, Year, lab_to_cap_ratio, inc_ratio, inc_ratio_with_h) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = variable)) +
  geom_line() +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  scale_linetype_manual(name = "", breaks = c("lab_to_cap_ratio", "inc_ratio", "inc_ratio_with_h"),
                        labels = c("Labor-to-capital income ratio",
                                   "Young-to-old income ratio",
                                   "Young-to-old income ratio (with health spending)"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic(base_size = 14) +
  labs(x = "", y = "") +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(), axis.title.y = element_blank())

## Paper version
incratio_raw_vs_net +
  scale_color_manual(name = "", breaks = c("France", "United States"),
                     labels = c("France", "United States"), values = c("black", "black"), 
                     guide = FALSE) +
  ggsave(file.path(loc_redis, "incratio_raw_vs_net.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/incratio_raw_vs_net-1.png)<!-- -->

``` r
## Color version
incratio_raw_vs_net +
  scale_color_manual(name = "", breaks = c("France", "United States"),
                     labels = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)], 
                     guide = FALSE) +
  ggsave(file.path(loc_redis, "incratio_raw_vs_net_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/incratio_raw_vs_net-2.png)<!-- -->

``` r
## Core
dev_incratio_raw_vs_net = redis %>% 
  select(Country, Year, inc_y, inc_o, wL, rK, health_spen, eta) %>% 
  group_by(Country) %>% 
  mutate(inc_ratio = inc_y/inc_o,
         #inc_ratio_with_h = inc_y/(inc_o + health_spen),
         lab_to_cap_ratio = wL/rK,
         # Deviations
         inc_ratio = inc_ratio/inc_ratio[Year == 1970] -1,
         #inc_ratio_with_h = inc_ratio_with_h/inc_ratio_with_h[Year == 1970] -1,
         lab_to_cap_ratio = lab_to_cap_ratio/lab_to_cap_ratio[Year == 1970] -1,) %>% 
  select(Country, Year, lab_to_cap_ratio, inc_ratio) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = variable, color = Country)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  scale_linetype_manual(name = "", breaks = c("lab_to_cap_ratio",
                                              "inc_ratio"),
                          labels = c("Labor-to-capital income ratio",
                                     "Young-to-old income ratio"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic(base_size = 14) +
  labs(x = "", y = "") +
  theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(), axis.title.y = element_blank())

## Paper version
dev_incratio_raw_vs_net +
  scale_color_manual(name = "", breaks = c("France", "United States"),
                     labels = c("France", "United States"), values = c("black", "black"), 
                     guide = FALSE) +
  ggsave(file.path(loc_redis, "dev_incratio_raw_vs_net.png"),
         width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/dev_incratio_raw_vs_net-1.png)<!-- -->

``` r
## Color version
dev_incratio_raw_vs_net +
  scale_color_manual(name = "", breaks = c("France", "United States"),
                     labels = c("France", "United States"), values = brewer.pal(8, "Set1")[c(1,2)], 
                     guide = FALSE) +
  ggsave(file.path(loc_redis, "dev_incratio_raw_vs_net_color.png"),
         width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/dev_incratio_raw_vs_net-2.png)<!-- -->

### Counterfactual and Decomposition

#### Simulation

``` r
# Re initialize result data frame
result = result[0,]

# Define break_year
break_year = 1970

## Aging effect decomposition : Survival rate versus Population growth
  for(sr in c("TSR", "FSR")){
    for(pg in c("TPG", "FPG")){
      
      spe = paste0("counter_", break_year, "_", pg, ".", sr, ".", "TDE", ".", "TIE")
      
      # Prepare data
      data = data_base %>% 
      # No biased technical change
      mutate(AK = 1, AL = 1) %>% 
      # Add parameters
      merge(param) %>% 
      # Demography
      demo_changer(break_year = break_year, p1_equals_p_fix = FALSE, PG = pg, SR = sr) %>% 
      # Define specification model
      mutate(Specification = spe) %>%
      # Reorder variables
      select(Specification, everything())
      
      
      # Simulate the model for each country
      for(country in country_set){
          
          result = data %>%
            subset(Country == country) %>% 
            model(time = 3) %>% 
            rbind(result, .)
          
      }
    }
  }

## Aging effect decomposition : Direct effect versus Indirect effect
  for(de in c("TDE", "FDE")){
    for(ie in c("TIE", "FIE")){
      
      spe = paste0("counter_", break_year, "_", "TPG", ".", "TSR", ".", de, ".", ie)
      
      # Prepare data
      data = data_base %>% 
      # No biased technical change
      mutate(AK = 1, AL = 1) %>% 
      # Add parameters
      merge(param) %>% 
      # Demography
      demo_changer(break_year = break_year, p1_equals_p_fix = FALSE, DE = de, IE = ie) %>% 
      # Define specification model
      mutate(Specification = spe) %>%
      # Reorder variables
      select(Specification, everything())
      
      
      # Simulate the model for each country
      for(country in country_set){
          
          result = data %>%
            subset(Country == country) %>% 
            model(time = 3) %>% 
            rbind(result, .)
          
      }
    }
  }

# Remove duplicates
final = distinct(result)

# Model specification details
final = final %>% mutate(PG = ifelse(grepl("TPG", Specification), "TPG", "FPG"),
                 SR = ifelse(grepl("TSR", Specification), "TSR", "FSR"),
                 DE = ifelse(grepl("TDE", Specification), "TDE", "FDE"),
                 IE = ifelse(grepl("TIE", Specification), "TIE", "FIE"),
                 break_year = as.numeric(gsub("[^0-9]", "\\1", Specification))) %>% 
  select(Specification, break_year, PG, SR, DE, IE, everything())

# Write final_counter
write.csv(final, file.path(loc_sim, paste0("final_", "counter", ".csv")), row.names = FALSE)
```

##### Counterfactual

``` r
# Decomposition / Counter location
loc_decomp = file.path(loc_result, "decomposition")

# Reshape dataframe
decomp = final %>% 
  select(Country, Specification, Year, theta) %>% 
  rbind(., 
        df %>% 
          mutate(Specification = paste0("data_", break_year)) %>%
          select(Specification, Country, Year, theta))  %>% 
  mutate(from = ifelse(!grepl("data", Specification), "sim", "data")) %>%
  filter(complete.cases(.)) %>% 
  mutate(PG = ifelse(from == "data", NA, ifelse(grepl("TPG", Specification), "TPG", "FPG")),
         SR = ifelse(from == "data", NA, ifelse(grepl("TSR", Specification), "TSR", "FSR")),
         DE = ifelse(from == "data", NA, ifelse(grepl("TDE", Specification), "TDE", "FDE")),
         IE = ifelse(from == "data", NA, ifelse(grepl("TIE", Specification), "TIE", "FIE")),
         break_year = as.numeric(gsub("[^0-9]", "\\1", Specification))) %>% 
  select(Specification, break_year, PG, SR, DE, IE, everything())
```

``` r
## Core
counter_pgsr = decomp %>% 
  subset(DE %in% c("TDE", NA) & IE %in% c("TIE", NA)) %>%
  subset(break_year == break_year) %>% 
  mutate(PGSR = as.character(interaction(PG, SR)),
         PGSR = ifelse(from == "data", "data", PGSR),
         PGSR = as.factor(PGSR)) %>% 
  
  ggplot(aes(x = Year, y = theta, color = PGSR, linetype = PGSR)) +
  facet_wrap(Country ~ .) +
  geom_line() +
  # geom_label_repel(aes(label = round(theta, 3)),
  #                  data = subset(decomp, Year == 2080 & DE == "TDE" & IE == "TIE" &
  #                                  break_year == 1970), 
  #                  nudge_x = 0, na.rm = TRUE,
  #                  segment.color = "transparent") +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.99, 0.99), legend.justification = c("right", "top"),
        legend.direction = "vertical",
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
counter_pgsr +
  scale_linetype_manual(name = "",
                        breaks = c("data","TPG.TSR", "TPG.FSR",
                                "FPG.TSR", "FPG.FSR"),
                        labels = c("Data", "Benchmark", expression(p[1970]),
                                expression(n[1970]), expression(p[1970]*", "*n[1970])),
                        values = c("solid", "dashed", "twodash", "dotted", "solid") %>% 
                       setNames(c("TPG.TSR", "TPG.FSR", "FPG.TSR", "FPG.FSR", "data"))) +
  scale_color_manual(name = "",
                   breaks = c("data","TPG.TSR", "TPG.FSR", "FPG.TSR", "FPG.FSR"), 
                   labels = c("Data", "Benchmark", expression(p[1970]), 
                              expression(n[1970]), expression(p[1970]*", "*n[1970])),
                   values = c("grey", rep("black", 4))) +
  ggsave(file.path(loc_decomp, "counter_PGSR.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/counter_pgsr-1.png)<!-- -->

``` r
## Color version
counter_pgsr +
  scale_linetype_manual(name = "",
                        breaks = c("data","TPG.TSR", "TPG.FSR", "FPG.TSR", "FPG.FSR"),
                        labels = c("Data", "Benchmark", expression(p[1970]),
                                expression(n[1970]), expression(p[1970]*", "*n[1970])),
                        values = c(rep("dashed", 4), "solid") %>% 
                       setNames(c("TPG.TSR", "TPG.FSR", "FPG.TSR", "FPG.FSR", "data"))) +
  scale_color_manual(name = "",
                   breaks = c("data","TPG.TSR", "TPG.FSR", "FPG.TSR", "FPG.FSR"),
                   labels = c("Data", "Benchmark", expression(p[1970]),
                              expression(n[1970]), expression(p[1970]*", "*n[1970])),
                   values = c(brewer.pal(8, "Set1")[c(1:4)], "black") %>% 
                       setNames(c("TPG.TSR", "TPG.FSR", "FPG.TSR", "FPG.FSR", "data"))) +
  ggsave(file.path(loc_decomp, "counter_PGSR_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/counter_pgsr-2.png)<!-- -->

``` r
## Core
counter_deie = decomp %>% 
  subset(PG %in% c("TPG", NA) & SR %in% c("TSR", NA)) %>%
  subset(break_year == break_year) %>% 
  mutate(DEIE = as.character(interaction(DE, IE)),
         DEIE = ifelse(from == "data", "data", DEIE),
         DEIE = as.factor(DEIE)) %>% 
  
  ggplot(aes(x = Year, y = theta, color = DEIE, linetype = DEIE)) +
  facet_wrap(Country ~ .) +
  geom_line() +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.99, 0.99), legend.justification = c("right", "top"),
        legend.direction = "vertical",
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
counter_deie +
  scale_linetype_manual(name = "",
                     breaks = c("data","TDE.TIE", "TDE.FIE", "FDE.TIE", "FDE.FIE"),
                     labels = c("Data", "Benchmark", 
                                expression(eta[1970]),
                                expression(p[1970]*", "*n[1970]),
                                expression(p[1970]*", "*n[1970]*", "*eta[1970])),
                     values = c("solid", "dashed", "twodash", "dotted", "solid") %>% 
                       setNames(c("TDE.TIE", "TDE.FIE", "FDE.TIE", "FDE.FIE", "data"))) +
  scale_color_manual(name = "",
                     breaks = c("data","TDE.TIE", "TDE.FIE", "FDE.TIE", "FDE.FIE"),
                     labels = c("Data", "Benchmark", 
                                expression(eta[1970]),
                                expression(p[1970]*", "*n[1970]),
                                expression(p[1970]*", "*n[1970]*", "*eta[1970])),
                   values = c(rep("black", 4), "grey") %>% 
                       setNames(c("TDE.TIE", "TDE.FIE", "FDE.TIE", "FDE.FIE", "data"))) +
  ggsave(file.path(loc_decomp, "counter_DEIE.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/counter_deie-1.png)<!-- -->

``` r
# Color version
counter_deie +
  scale_linetype_manual(name = "",
                     breaks = c("data","TDE.TIE", "TDE.FIE", "FDE.TIE", "FDE.FIE"),
                     labels = c("Data", "Benchmark", 
                                expression(eta[1970]),
                                expression(p[1970]*", "*n[1970]),
                                expression(p[1970]*", "*n[1970]*", "*eta[1970])),
                     values = c(rep("dashed", 4), "solid") %>% 
                       setNames(c("TDE.TIE", "TDE.FIE", "FDE.TIE", "FDE.FIE", "data"))) +
  scale_color_manual(name = "",
                     breaks = c("data","TDE.TIE", "TDE.FIE", "FDE.TIE", "FDE.FIE"),
                     labels = c("Data", "Benchmark", 
                                expression(eta[1970]),
                                expression(p[1970]*", "*n[1970]),
                                expression(p[1970]*", "*n[1970]*", "*eta[1970])),
                   values = c(brewer.pal(8, "Set1")[c(1,5,8,4)], "black") %>% 
                       setNames(c("TDE.TIE", "TDE.FIE", "FDE.TIE", "FDE.FIE", "data"))) +
  ggsave(file.path(loc_decomp, "counter_DEIE_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/counter_deie-2.png)<!-- -->

#### Decomposition

``` r
decomp2 = final %>% 
  select(Country, Specification, Year, theta) %>% 
  mutate(PG = ifelse(Specification == "data", NA,
                     ifelse(grepl("TPG", Specification), "TPG", "FPG")),
         SR = ifelse(Specification == "data", NA, 
                     ifelse(grepl("TSR", Specification), "TSR", "FSR")),
         DE = ifelse(Specification == "data", NA, 
                     ifelse(grepl("TDE", Specification), "TDE", "FDE")),
         IE = ifelse(Specification == "data", NA, 
                     ifelse(grepl("TIE", Specification), "TIE", "FIE")),
         break_year = as.numeric(gsub("[^0-9]", "\\1", Specification)),
         Spe_PGSR = interaction(PG, SR),
         Spe_DEIE = interaction(DE, IE)) %>% 
  select(Country, Year, break_year, Spe_PGSR, Spe_DEIE, theta) %>% 
  dcast(Country + Year + break_year ~ Spe_PGSR + Spe_DEIE, value.var = "theta") %>% 
  setNames(c("Country", "Year", "break_year",
             "FPG.FSR", "TPG.FSR", "FPG.TSR", 
             "FDE.FIE", "TDE.FIE", "FDE.TIE",
             "benchmark")) %>% 
  mutate(FPG.TSR = (benchmark - FPG.TSR)*100,
         TPG.FSR = (benchmark - TPG.FSR)*100,
         FPG.FSR = (benchmark - FPG.FSR)*100,
         FPG.FSR = FPG.FSR - FPG.TSR - TPG.FSR,
         position_PGSR = (FPG.TSR + TPG.FSR + FPG.FSR),
         FPG.TSR.share = abs(FPG.TSR)/(abs(FPG.TSR) + abs(TPG.FSR) + abs(FPG.FSR)),
         TPG.FSR.share = abs(TPG.FSR)/(abs(FPG.TSR) + abs(TPG.FSR) + abs(FPG.FSR)),
         FPG.FSR.share = abs(FPG.FSR)/(abs(FPG.TSR) + abs(TPG.FSR) + abs(FPG.FSR)),
         FDE.TIE = (benchmark - FDE.TIE)*100,
         TDE.FIE = (benchmark - TDE.FIE)*100,
         FDE.FIE = (benchmark - FDE.FIE)*100,
         FDE.FIE = FDE.FIE - FDE.TIE - TDE.FIE,
         position_DEIE = (FDE.TIE + TDE.FIE + FDE.FIE),
         FDE.TIE.share = abs(FDE.TIE)/(abs(FDE.TIE) + abs(TDE.FIE) + abs(FDE.FIE)),
         TDE.FIE.share = abs(TDE.FIE)/(abs(FDE.TIE) + abs(TDE.FIE) + abs(FDE.FIE)),
         FDE.FIE.share = abs(FDE.FIE)/(abs(FDE.TIE) + abs(TDE.FIE) + abs(FDE.FIE))) %>% 
  melt(id.vars = c("Country", "Year", "break_year", "position_PGSR", "position_DEIE"))
```

``` r
## Core
decomp_pgsr = decomp2 %>% 
  subset(break_year == break_year) %>% 
  subset(variable %in% c("FPG.TSR", "TPG.FSR", "FPG.FSR")) %>% 
  
  ggplot(aes(x = Year)) +
  geom_col(aes(y = value, fill = variable), alpha = .5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  # scale_y_continuous(limits = c(-4,6)) +
  theme_classic(base_size = 14) +
  theme(legend.direction = "horizontal", legend.box = "horizontal", legend.position = "bottom",
        axis.title.x = element_blank()) +
  labs(x = "", y = "Difference with counterfactual (in pp.)")
  
## Paper version
decomp_pgsr + 
  geom_line(aes(y = position_PGSR), color = "black") +
  scale_fill_grey(name = "", breaks = c("FPG.TSR", "TPG.FSR", "FPG.FSR"),
                  labels = c("Population growth effect", "Survival rate effect", "Interaction effect"),
                  start = 0.2, end = 0.8) +
  ggsave(file.path(loc_decomp, "decomp_PGSR.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/decomp_pgsr-1.png)<!-- -->

``` r
## Color version
decomp_pgsr + 
  geom_line(aes(y = position_PGSR), color = brewer.pal(8, "Set1")[1]) +
  scale_fill_manual(name = "", breaks = c("FPG.TSR", "TPG.FSR", "FPG.FSR"),
                  labels = c("Population growth effect", "Survival rate effect", "Interaction effect"),
                  values = brewer.pal(8, "Set1")[c(2:4)] %>% setNames(c("TPG.FSR", "FPG.TSR", "FPG.FSR"))) +
  ggsave(file.path(loc_decomp, "decomp_PGSR_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/decomp_pgsr-2.png)<!-- -->

``` r
## Core
decomp_deie = decomp2 %>% 
  subset(variable %in% c("FDE.TIE", "TDE.FIE", "FDE.FIE")) %>% 
  subset(break_year == break_year) %>% 
  
  ggplot(aes(x = Year)) +
  geom_col(aes(y = value, fill = variable), alpha = .5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(Country ~ .) +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  # scale_y_continuous(limits = c(-4,6)) +
  theme_classic(base_size = 14) +
  theme(legend.direction = "horizontal", legend.box = "horizontal",
        legend.position = "bottom", axis.title.x = element_blank()) +
  labs(x = "", y = "Difference with counterfactual (in pp.)")

## Paper version
decomp_deie +
  geom_line(aes(y = position_DEIE), color = "black") +
  scale_fill_grey(name = "", breaks = c("FDE.TIE", "TDE.FIE", "FDE.FIE"), 
                  labels = c("Direct cohort effect", "Indirect cohort effect", "Interaction effect"),
                  start = 0.2, end = 0.8) +
  ggsave(file.path(loc_decomp, "decomp_DEIE.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/decomp_deie-1.png)<!-- -->

``` r
## Color version
decomp_deie +
  geom_line(aes(y = position_DEIE), color = "black") +
  scale_fill_manual(name = "", breaks = c("FDE.TIE", "TDE.FIE", "FDE.FIE"), 
                  labels = c("Direct cohort effect", "Indirect cohort effect", "Interaction effect"),
                  values = brewer.pal(8, "Set1")[c(8,5,4)] %>% setNames(c("FDE.TIE", "TDE.FIE", "FDE.FIE"))) +
  ggsave(file.path(loc_decomp, "decomp_DEIE_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/decomp_deie-2.png)<!-- -->

``` r
# Compute the share of each effect/channel for two sub_periods 1980-2010 & 2020-2050
decomp_mean.period = decomp2 %>% 
  select(-contains("position")) %>% 
  dcast(Country + Year + break_year ~ variable) %>% 
  mutate(Country_break = interaction(break_year, Country, sep = " - "),
         Period = ifelse(Year %in% c(1970:2010), "P1",
                         ifelse(Year %in% c(2020:2050), "P2", "P3"))) %>% 
  subset(Period != "P3") %>% 
  group_by(Country_break, Period) %>% 
  select(Country_break, Period, TPG.FSR.share, FPG.TSR.share, FPG.FSR.share,
         FDE.TIE.share, TDE.FIE.share, FDE.FIE.share) %>% 
  summarise_at(vars(TPG.FSR.share:FDE.FIE.share), mean, na.rm = TRUE)

# Visualization
decomp_mean.period
```

    ## # A tibble: 4 x 8
    ## # Groups:   Country_break [2]
    ##   Country_break Period TPG.FSR.share FPG.TSR.share FPG.FSR.share
    ##   <fct>         <chr>          <dbl>         <dbl>         <dbl>
    ## 1 1970 - France P1             0.545         0.397       0.0581 
    ## 2 1970 - France P2             0.581         0.413       0.00583
    ## 3 1970 - Unite~ P1             0.781         0.206       0.0138 
    ## 4 1970 - Unite~ P2             0.570         0.388       0.0417 
    ## # ... with 3 more variables: FDE.TIE.share <dbl>, TDE.FIE.share <dbl>,
    ## #   FDE.FIE.share <dbl>

``` r
## Core
decomp_country_period = decomp_mean.period %>% 
  melt(id.vars = c("Country_break", "Period")) %>% 
  mutate(value = value*100) %>% 
  subset(Country_break %in% c("1970 - France", "1970 - United States")) %>% 
  mutate(Country = ifelse(Country_break == "1970 - France", "France", "United States"),
         effect = factor(ifelse(variable %>% grepl(pattern = "PG", .), "first", "second")),
         Period = ifelse(Period == "P1", "1980-2010", "2020-2050")) %>% 
  
  ggplot(aes(x = effect, y = value, fill = variable)) +
  facet_grid(Period ~ Country) +
  scale_x_discrete(breaks = c("first", "second"), labels = c("Determinants", "Channels")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  labs(x = "", y = "Percent")

## Paper version
decomp_country_period +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(name = "",
                    breaks = c("TPG.FSR.share", "FPG.TSR.share",
                               "FDE.TIE.share", "TDE.FIE.share", "FDE.FIE.share"),
                    labels = c("Survival rate", "Population growth",
                               "Direct cohort", "Indirect cohort", "Interaction"),
                    values = c("#191919", "#7C7C7C", "#E6E6E6", "#A8A8A8", "#CACACA", "#E6E6E6") %>% 
                      setNames(c("TPG.FSR.share", "FPG.TSR.share", "FPG.FSR.share",
                               "FDE.TIE.share", "TDE.FIE.share", "FDE.FIE.share"))) +
  ggsave(file.path(loc_decomp, "decomp_country_period.png"),
         width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/decomp_country_period-1.png)<!-- -->

``` r
## Color version
decomp_country_period +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.5, color = "black") +
  scale_fill_manual(name = "",
                    breaks = c("TPG.FSR.share", "FPG.TSR.share",
                               "FDE.TIE.share", "TDE.FIE.share", "FDE.FIE.share"),
                    labels = c("Survival rate", "Population growth",
                               "Direct cohort", "Indirect cohort", "Interaction"),
                    values = brewer.pal(8, "Set1")[c(2,3,4,8,5,4)] %>% 
                      setNames(c("TPG.FSR.share", "FPG.TSR.share", "FPG.FSR.share",
                                 "FDE.TIE.share", "TDE.FIE.share", "FDE.FIE.share"))) +
  ggsave(file.path(loc_decomp, "decomp_country_period_color.png"),
         width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/decomp_country_period-2.png)<!-- -->

### Retirement age

``` r
# Define break_year
break_year = 2020
# Define first years after the reform
first_years = seq(break_year+10, (break_year+40), by = 10)

# Location retirement
loc_ret = file.path(loc_result, "retirement")

# Reset ret_age
ret_age = NULL
```

#### Specification 1

``` r
# Re initialize result data frame
result = result[0,]

# Size of the shock (in %)
shock = -1/10

# Specification
ret_spe = 1
spe = paste0("ret_age_", break_year, "_spe", ret_spe)

# Prepare data
data = data_base %>% 
  # No biased technical change
  mutate(AK = 1, AL = 1) %>% 
  # Add parameters
  merge(param) %>% 
  # Demography
  demo_changer(break_year = break_year, p1_equals_p_fix = FALSE) %>% 
  # Define specification model
  mutate(Specification = spe) %>%
  # Reorder variables
  select(Specification, everything()) %>% 
  
  ## Modification of demographic data
  
  group_by(Country, Sequence) %>% 
  mutate(# New p, p1 and n
         p.new = ifelse(Year > break_year, p*(1+shock), p),
         p1.new = ifelse(Year > break_year, p1*(1+shock), p1),
         n.new = ifelse(Year %in% first_years, n*(1-shock), n),
         #*(1-1/40*(Year-break_year-10))
         # Compute Ny and No for the affected periods
         Ny.new = ifelse(Year %in% first_years, Ny*(1-shock), NA),
         No.new = ifelse(Year %in% first_years, No*(1+shock), NA),
         
         # Compute Ny and No for last sequence/periods
         Ny.new = ifelse(Year > max(first_years), lag(Ny.new,1)*n, Ny.new),
         No.new = ifelse(Year > max(first_years), lag(Ny.new,1)*p.new, No.new),
         
         # Recompute eta
         eta.new = n.new/p.new*(1+alpha*p1.new)/omega,
         
         # Replace values
         Ny = ifelse(is.na(Ny.new), Ny, Ny.new),
         No = ifelse(is.na(No.new), No, No.new),
         n = n.new,
         p = p.new,
         p1 = p1.new,
         eta = eta.new,
         
         ) %>% 
  select(Specification, Country, Year, Sequence, Period, Ny, No, n, p, p1, 
         eta, everything(), -contains("new"), ) %>%
  ungroup()

# Simulate the model for each country
for(country in country_set){
  result = data %>%
  subset(Country == country) %>% 
  model(time = 3) %>% 
  rbind(result, .)
}

# Remove duplicates
ret_age = distinct(result)
```

#### Specification 2

``` r
# Re initialize result data frame
result = result[0,]

# Fraction
frac = 0

# Specification
ret_spe = 2
spe = paste0("ret_age_", break_year, "_spe", ret_spe)

# Prepare data
data = data_base %>% 
  # No biased technical change
  mutate(AK = 1, AL = 1) %>% 
  # Add parameters
  merge(param) %>% 
  # Demography
  demo_changer(break_year = break_year, p1_equals_p_fix = FALSE) %>% 
  # Define specification model
  mutate(Specification = spe) %>%
  # Reorder variables
  select(Specification, everything()) %>% 
  # Merge witj break year
  merge(., data_base %>% 
          subset(Year == break_year) %>%
          select(Country, p_fix = p), by = "Country") %>% 
  
  ## Modification of demographic data
  
  group_by(Country, Sequence) %>% 
  mutate(
         p1_to_p_dist = p1 - p,
         # New p and p1
         p.new = ifelse(Year > break_year, frac*(p-p_fix) + p_fix, p),
         p1.new = ifelse(Year > break_year & Period < 3, dplyr::lead(p.new,1),
                         ifelse(Period == 3, p.new + p1_to_p_dist*frac, p1)),
         # Scale for the affected periods
         shock = (p.new - p)/p,
         # New n
         n.new = ifelse(Year %in% first_years, n*(1-shock), n),
         #*(1-1/40*(Year-break_year-10))
         # Compute Ny and No for the affected periods
         Ny.new = ifelse(Year %in% first_years, Ny*(1-shock), NA),
         No.new = ifelse(Year %in% first_years, No*(1+shock), NA),
         
         # Compute Ny and No for last sequence/periods
         Ny.new = ifelse(Year > max(first_years), lag(Ny.new,1)*n, Ny.new),
         No.new = ifelse(Year > max(first_years), lag(Ny.new,1)*p.new, No.new),
         
         # Recompute eta
         eta.new = n.new/p.new*(1+alpha*p1.new)/omega,
         
         # Replace values
         Ny = ifelse(is.na(Ny.new), Ny, Ny.new),
         No = ifelse(is.na(No.new), No, No.new),
         n = n.new,
         p = p.new,
         p1 = p1.new,
         eta = eta.new,
         
         ) %>% 
  select(Specification, Country, Year, Sequence, Period, Ny, No, n, p, p1, 
         eta, everything(), -contains("new"), -shock, -p_fix, -p1_to_p_dist ) %>%
  ungroup()

# Simulate the model for each country
for(country in country_set){
  result = data %>%
  subset(Country == country) %>% 
  model(time = 3) %>% 
  rbind(result, .)
}


# Remove duplicates
ret_age = rbind(ret_age, distinct(result))
```

#### Specification 3

``` r
# Re initialize result data frame
result = result[0,]

# Fraction
frac = 0.5

# Specification
ret_spe = 3
spe = paste0("ret_age_", break_year, "_spe", ret_spe)

# Prepare data
data  = data_base %>% 
  # No biased technical change
  mutate(AK = 1, AL = 1) %>% 
  # Add parameters
  merge(param) %>% 
  # Demography
  demo_changer(break_year = break_year, p1_equals_p_fix = FALSE) %>% 
  # Define specification model
  mutate(Specification = spe) %>%
  # Reorder variables
  select(Specification, everything()) %>% 
  # Merge witj break year
  merge(., data_base %>% 
          subset(Year == break_year) %>%
          select(Country, p_fix = p), by = "Country") %>% 
  
  ## Modification of demographic data
  
  group_by(Country, Sequence) %>% 
  mutate(
         p1_to_p_dist = p1 - p,
         # New p and p1
         p.new = ifelse(Year > break_year, frac*(p-p_fix) + p_fix, p),
         p1.new = ifelse(Year > break_year & Period < 3, dplyr::lead(p.new,1),
                         ifelse(Period == 3, p.new + p1_to_p_dist*frac, p1)),
         # Scale for the affected periods
         shock = (p.new - p)/p,
         # New n
         n.new = ifelse(Year %in% first_years, n*(1-shock), n),
         
         # Compute Ny and No for the affected periods
         Ny.new = ifelse(Year %in% first_years, Ny*(1-shock), NA),
         No.new = ifelse(Year %in% first_years, No*(1+shock), NA),
         
         # Compute Ny and No for last sequence/periods
         Ny.new = ifelse(Year > max(first_years), lag(Ny.new,1)*n, Ny.new),
         No.new = ifelse(Year > max(first_years), lag(Ny.new,1)*p.new, No.new),
         
         # Recompute eta
         eta.new = n.new/p.new*(1+alpha*p1.new)/omega,
         
         # Replace values
         Ny = ifelse(is.na(Ny.new), Ny, Ny.new),
         No = ifelse(is.na(No.new), No, No.new),
         n = n.new,
         p = p.new,
         p1 = p1.new,
         eta = eta.new,
         
         ) %>% 
  select(Specification, Country, Year, Sequence, Period, Ny, No, n, p, p1, 
         eta, everything(), -contains("new"), -shock, -p_fix, -p1_to_p_dist ) %>%
  ungroup()

# Simulate the model for each country
for(country in country_set){
  result = data %>%
  subset(Country == country) %>% 
  model(time = 3) %>% 
  rbind(result, .)
}


# Remove duplicates
ret_age = rbind(ret_age, distinct(result))
```

#### Summary RetAge

``` r
# Write ret_age
write.csv(ret_age, file.path(loc_sim, "ret_age.csv"), row.names = FALSE)
```

``` r
## Core
retage_ls_all = fillgap %>%
  rbind(., baseline %>% subset(Country == "United States")) %>% 
  mutate(Specification = "base2") %>% 
  select(Specification, Country, Year, theta) %>% 
  melt(id.vars = c("Specification" ,"Country", "Year")) %>% 
  rbind(.,
        df %>% 
          mutate(Specification = "data") %>%
          select(Specification, Country, Year, theta) %>%
          setNames(c("Specification","Country", "Year", "data")) %>% 
          melt(id.vars = c("Specification", "Country", "Year"))
        ) %>%
  filter(complete.cases(.)) %>% 
  subset(Year %in% c(1970:2080)) %>%
  # Trick to put data line under others
  mutate(variable = as.character(variable),
         variable = ifelse(variable == "data", "a.data", variable),
         variable = as.factor(variable)) %>% 
  
  rbind(., ret_age %>% 
          select(Specification, Country, Year, theta) %>%
          melt(id.vars = c("Specification" ,"Country", "Year"))) %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Specification)) +
  geom_line() +
  facet_wrap(Country ~ ., scales = "fixed") +
  scale_x_continuous(breaks = breaks_10_years, labels = labs_20_years) +
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.99,0.99),
        legend.justification = c("right", "top"),
        legend.direction = "vertical",
        legend.box = "horizontal",
        panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text.align = 0) +
  labs(x = "", y = "") 

## Paper version
retage_ls_all +
  scale_color_manual(name = "",
                     breaks = c("data", "base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"),
                     labels = c("Data", "Benchmark", "Shift -10%", "Constant", "Half growth"),
                     values = c("grey", rep("black", 4)) %>%
                       setNames(c("data","base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"))) +
  scale_linetype_manual(name = "",
                        breaks = c("data", "base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"),
                        labels = c("Data", "Benchmark", "Shift -10%", "Constant", "Half growth"),
                        values = c("solid", "solid", "dashed", "dotdash", "dotted") %>% 
                          setNames(c("data","base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"))) +
  ggsave(file.path(loc_ret, "retage_ls_all.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/retage_ls_all-1.png)<!-- -->

``` r
## Color version
retage_ls_all +
  aes(color = interaction(variable, Country)) +
  scale_color_manual(name = "",
                     breaks = c("a.data.France", "a.data.United States",
                                "theta.France", "theta.United States"),
                     values = c(rep("black", 2), brewer.pal(8, "Set1")[c(1,2)]) %>%
                       setNames(c("a.data.France", "a.data.United States",
                                "theta.France", "theta.United States")),
                     guide = FALSE) +
  scale_linetype_manual(name = "",
                        breaks = c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"),
                        labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                        values = c("solid", "solid", "dashed", "dotdash", "dotted") %>% 
                          setNames(c("data","base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"))) +
  ggsave(file.path(loc_ret, "retage_ls_all_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/retage_ls_all-2.png)<!-- -->

``` r
## Core
retage_p = fillgap %>%
  rbind(., baseline %>% subset(Country == "United States")) %>% 
  mutate(Specification = "base2") %>% 
  select(Specification, Country, Year, p, p1) %>% 
  melt(id.vars = c("Specification" ,"Country", "Year")) %>% 
  subset(Year %in% c(1970:2080)) %>%
  
  rbind(., ret_age %>% 
          select(Specification, Country, Year, p, p1) %>%
          melt(id.vars = c("Specification" ,"Country", "Year"))) %>% 
  subset(Year >= 2020) %>% 
  
  ggplot(aes(x = Year, y = value, color = Specification, linetype = variable)) +
  geom_line() +
  facet_wrap(Country ~ ., scales = "fixed") +
  scale_x_continuous(breaks = seq(2020, 2080, 10),
                     labels = c("2020", "", "2040", "", "2060", "", "2080")) +
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.85,0.99),
        legend.justification = c("right", "top"),
        legend.direction = "vertical",
        legend.box = "horizontal",
        panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Labor share")

## Paper version
retage_p +
  aes(color = variable, linetype = Specification) +
  scale_linetype_manual(name = "", breaks = c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"), 
                        labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                        values = c("solid", "dashed", "dotdash", "dotted") %>% 
                          setNames(c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2", "ret_age_2020_spe3"))) +
  scale_color_grey(name = "", breaks = c("p", "p1"), 
                     labels = c(expression(p[t]), expression(p[t+1])),
                   start = 0, end = 0.6) +
  ggsave(file.path(loc_ret, "retage_p.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/retage_p-1.png)<!-- -->

``` r
## Color version
retage_p +
  scale_color_manual(name = "", breaks = c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"),
                     labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                    values = c("black", brewer.pal(8, "Set1")[c(1:3)]) %>% 
                      setNames(c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"))) +
  scale_linetype_manual(name = "", breaks = c("p", "p1"),
                     labels = c(expression(p[t]), expression(p[t+1])),
                        values = c("solid", "dashed") %>% setNames(c("p", "p1"))) +
  ggsave(file.path(loc_ret, "retage_p_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/retage_p-2.png)<!-- -->

``` r
## Core
retage_ls = fillgap %>%
  rbind(., baseline %>% subset(Country == "United States")) %>% 
  mutate(Specification = "base2") %>% 
  select(Specification, Country, Year, theta) %>% 
  melt(id.vars = c("Specification" ,"Country", "Year")) %>% 
  subset(Year %in% c(1970:2080)) %>%
  
  rbind(., ret_age %>% 
          select(Specification, Country, Year, theta) %>%
          melt(id.vars = c("Specification" ,"Country", "Year"))) %>% 
  subset(Year >= 2020) %>% 
  
  ggplot(aes(x = Year, y = value, color = Specification, linetype = Specification)) +
  geom_line() +
  facet_wrap(Country ~ ., scales = "fixed") +
  scale_x_continuous(breaks = seq(2020, 2080, 10),
                     labels = c("2020", "", "2040", "", "2060", "", "2080")) +
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.direction = "vertical",
        panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Labor share")

## Paper version
retage_ls +
  scale_color_manual(name = "",
                     breaks = c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"),
                     labels = c("Benchmark", "Shift -10%",
                                "Constant", "Half growth"),
                     values = c(rep("black", 4)) %>% 
                       setNames(c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"))) +
  scale_linetype_manual(name = "",
                        breaks = c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"),
                        labels = c("Benchmark", "Shift -10%",
                                "Constant", "Half growth"),
                        values = c("solid", "dashed", "dotdash", "dotted") %>% 
                          setNames(c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"))) +
  ggsave(file.path(loc_ret, "retage_ls.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/retage_ls-1.png)<!-- -->

``` r
## Color version
retage_ls +
  scale_color_manual(name = "",
                     breaks = c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"),
                     labels = c("Benchmark", "Shift -10%",
                                "Constant", "Half growth"),
                     values = c("black", brewer.pal(8, "Set1")[c(1:3)]) %>% 
                       setNames(c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"))) +
  scale_linetype_manual(name = "",
                        breaks = c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"),
                        labels = c("Benchmark", "Shift -10%",
                                "Constant", "Half growth"),
                        values = c("solid", "dashed", "dashed", "dashed") %>% 
                          setNames(c("base2", "ret_age_2020_spe1", "ret_age_2020_spe2",
                                "ret_age_2020_spe3"))) +
  ggsave(file.path(loc_ret, "retage_ls_color.png"), width = scale_graph*5, height = scale_graph*5/2)
```

![](main_files/figure-gfm/retage_ls-2.png)<!-- -->

``` r
## Core
retage_others = fillgap %>%
  rbind(., baseline %>% subset(Country == "United States")) %>% 
  mutate(Specification = "base2") %>% 
  rbind(ret_age) %>% 
  mutate(source = ifelse(Specification == "base2", "base2","ret_age"),
         from = interaction(Specification, Country)) %>% 
  group_by(from) %>% 
  mutate(
    "YL" = Y/L,
    `b share` = b*u*Ny/tau/Y,
    `h share` = h*No/tau/Y,
    bh = b/h,
    net = 1-tau,
    outside = b/(1-tau),
    # Demography
    "n" = n/n[Year == 2020] -1,
    "p" = p/p[Year == 2020] -1,
    "p[+1]" = p1/p1[Year == 2020] -1,
    "eta" = eta/eta[Year == 2020] -1,
    # Public
    "tau" = tau/tau[Year == 2020] -1,
    # "b" = b/b[Year == 2020]-1,
    # "h" = h/h[Year == 2020]-1,
    "`b share`" = `b share`/`b share`[Year == 2020]-1,
    "`h share`" = `h share`/`h share`[Year == 2020]-1,
    # "b/h" = bh/bh[Year == 2020]-1,
    # Outside option
    "b" = b/b[Year == 2020] -1,
    "b/(1-tau)" = outside/outside[Year == 2020] -1,
    # Bargaining
    "w" = w/w[Year == 2020] -1,
    "X" = X/X[Year == 2020] -1,
    # Production
    "K" = K/K[Year == 2020] -1,
    "L" = L/L[Year == 2020] -1,
    "Y" = Y/Y[Year == 2020] -1,
    # Unemployment
    "N^y" = Ny/Ny[Year == 2020] -1,
    "u" = u/u[Year == 2020] -1,
    # Labor share
    "Y/L" = YL/YL[Year == 2020] -1,
    "theta" = theta/theta[Year == 2020] -1) %>% 
  ungroup() %>% 
  select(from, source, Specification, Country , Year,
         "Population~growth~(n)"= "n", 
         "Survival~rate~(p)"= "p", 
         "Exp.~survival~rate~(p[+1])"= "p[+1]", 
         "Youth~political~weight~(eta)" = "eta",
         "Tax~rate~(tau)" = "tau", 
         "Unemp.~benefits~share" = "`b share`",
         "Unemp.~benefits~(b)" = "b", 
         "Outside~option" = "b/(1-tau)",
         "Wage~rate~(w)" = "w", 
         "Job~value~added~(X)" = "X",
         #"Capital~stock~(K)" = "K", 
         "Labor~(L)" = "L", 
         "Production~(Y)" = "Y",
         "Young~households~(N^y)" = "N^y",
         "Unemployment~rate~(u)" = "u",
         "Prod.~per~worker~(Y/L)" = "Y/L", 
         "Labor~share~(theta)" = "theta") %>% 
  subset(Year %in% c(2020:2080)) %>% 
  melt(id.vars = c("from", "source", "Specification", "Country", "Year"))
```

``` r
## Core
retage_others_fr = retage_others %>% 
  subset(Country == "France") %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Specification)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  geom_line() +
  facet_wrap(.~variable, ncol = 4, labeller = label_parsed) +
  scale_x_continuous(breaks = seq(2020, 2080, 10),
                     labels = c("2020", "", "2040", "", "2060", "", "2080")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
retage_others_fr +
  scale_linetype_manual(name = "",
                        breaks = c("base2", "ret_age_2020_spe1",
                                "ret_age_2020_spe2", "ret_age_2020_spe3"),
                        labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                        values = c("solid", "dashed", "dotdash", "dotted")) +
  scale_color_manual(name = "",
                     breaks = c("base2", "ret_age_2020_spe1",
                                "ret_age_2020_spe2", "ret_age_2020_spe3"),
                     labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                     values = rep("black",4)) +
  ggsave(file.path(loc_ret, "retage_others_fr.png"), width = scale_graph*5, height = scale_graph*5)
```

![](main_files/figure-gfm/retage_others_fr-1.png)<!-- -->

``` r
## Color version
retage_others_fr +
  scale_linetype_manual(name = "",
                        breaks = c("base2", "ret_age_2020_spe1",
                                "ret_age_2020_spe2", "ret_age_2020_spe3"),
                        labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                        values = c("solid", "dashed", "dotdash", "dotted")) +
  scale_color_manual(name = "",
                     breaks = c("base2", "ret_age_2020_spe1",
                                "ret_age_2020_spe2", "ret_age_2020_spe3"),
                     labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                     values = c("black", brewer.pal(8, "Set1")[c(1:3)])) +
  ggsave(file.path(loc_ret, "retage_others_fr_color.png"), width = scale_graph*5, height = scale_graph*5)
```

![](main_files/figure-gfm/retage_others_fr-2.png)<!-- -->

``` r
## Core
retage_others_us = retage_others %>% 
  subset(Country == "United States") %>% 
  
  ggplot(aes(x = Year, y = value, linetype = Specification, color = Specification)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  geom_line() +
  facet_wrap(.~variable, ncol = 4, labeller = label_parsed) +
  scale_x_continuous(breaks = seq(2020, 2080, 10),
                     labels = c("2020", "", "2040", "", "2060", "", "2080")) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        panel.spacing.x = unit(1.5, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "", y = "")

## Paper version
retage_others_us +
  scale_linetype_manual(name = "",
                        breaks = c("base2", "ret_age_2020_spe1",
                                "ret_age_2020_spe2", "ret_age_2020_spe3"),
                        labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                        values = c("solid", "dashed", "dotdash", "dotted")) +
  scale_color_manual(name = "",
                     breaks = c("base2", "ret_age_2020_spe1",
                                "ret_age_2020_spe2", "ret_age_2020_spe3"),
                     labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                     values = rep("black",4)) +
  ggsave(file.path(loc_ret, "retage_others_us.png"), width = scale_graph*5, height = scale_graph*5)
```

![](main_files/figure-gfm/retage_others_us-1.png)<!-- -->

``` r
## Color version
retage_others_us +
  scale_linetype_manual(name = "",
                        breaks = c("base2", "ret_age_2020_spe1",
                                "ret_age_2020_spe2", "ret_age_2020_spe3"),
                        labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                        values = c("solid", "dashed", "dotdash", "dotted")) +
  scale_color_manual(name = "",
                     breaks = c("base2", "ret_age_2020_spe1",
                                "ret_age_2020_spe2", "ret_age_2020_spe3"),
                     labels = c("Benchmark", "Shift -10%", "Constant", "Half growth"),
                     values = c("black", brewer.pal(8, "Set1")[c(1:3)])) +
  ggsave(file.path(loc_ret, "retage_others_us_color.png"), width = scale_graph*5, height = scale_graph*5)
```

![](main_files/figure-gfm/retage_others_us-2.png)<!-- -->

![](main_files/figure-gfm/Other%20variables-1.png)<!-- -->