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
estimate the following equation
:

\[\ln k_t = \alpha - \frac{\sigma}{1-\sigma} \ln \Theta_t + \left( a_L-a_K\right)t + \varepsilon_t\]

Therefore, I need to compute the capital-to-labor ratio \(k_t\) and the
capital-to-labor income ratio \(\Theta_t\).

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
  facet_wrap(Country ~ ., dir = "v") +
  theme_classic(base_size = 14) +
  labs(x = "Year", y = "")
```

![](sigma_files/figure-gfm/Plot%20k-1.png)<!-- -->
