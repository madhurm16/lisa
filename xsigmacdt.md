X & Sigma condition
================
Fabien Petit
11/06/2019

In the model, I obtain a condition on the capital-labor elasticity of
substitution ![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma
"\\sigma") and the unemployment replacement rate which ensures a
positive capital-to-labor ratio and therefore a positive wage. This
condition is the following :   
![
\\frac{1}{\\sigma} \> X\_t \\equiv
\\ln\\left\[\\frac{(1-\\tau\_t)w\_t}{b\_t}\\right\]
](https://latex.codecogs.com/png.latex?%0A%5Cfrac%7B1%7D%7B%5Csigma%7D%20%3E%20X_t%20%5Cequiv%20%5Cln%5Cleft%5B%5Cfrac%7B%281-%5Ctau_t%29w_t%7D%7Bb_t%7D%5Cright%5D%0A
"
\\frac{1}{\\sigma} \> X_t \\equiv \\ln\\left[\\frac{(1-\\tau_t)w_t}{b_t}\\right]
")  
where ![X\_t](https://latex.codecogs.com/png.latex?X_t "X_t") is the
value-added to have a job in utility terms,
![w\_t](https://latex.codecogs.com/png.latex?w_t "w_t") is the wage
rate, ![tau\_t](https://latex.codecogs.com/png.latex?tau_t "tau_t") the
tax rate and ![b\_t](https://latex.codecogs.com/png.latex?b_t "b_t") the
unemployment benefits per capita. Thus, it is possible to rewrite this
condition such that :   
![
e^{-\\frac{1}{\\sigma}} \< \\frac{b\_t}{(1-\\tau\_t)w\_t}
](https://latex.codecogs.com/png.latex?%0Ae%5E%7B-%5Cfrac%7B1%7D%7B%5Csigma%7D%7D%20%3C%20%5Cfrac%7Bb_t%7D%7B%281-%5Ctau_t%29w_t%7D%0A
"
e^{-\\frac{1}{\\sigma}} \< \\frac{b_t}{(1-\\tau_t)w_t}
")  
The right hand side of this inequality corresponds to the unemployment
replacement rate. Hence, I check whether this constraint is feasible for
OECD countries and particulary those considered in the simulation
(i.e. France and United States).

Let plot the function ![f(\\sigma) =
e^{-\\frac{1}{\\sigma}}](https://latex.codecogs.com/png.latex?f%28%5Csigma%29%20%3D%20e%5E%7B-%5Cfrac%7B1%7D%7B%5Csigma%7D%7D
"f(\\sigma) = e^{-\\frac{1}{\\sigma}}") for likely values of
![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma "\\sigma"), so
an interval between 0 and 3 :

![](xsigmacdt_files/figure-gfm/f%20Plot-1.png)<!-- -->

    ## # A tibble: 30 x 7
    ##    Country US100_min UC1000_min US100_mean UC1000_mean US100_first
    ##    <fct>       <dbl>      <dbl>      <dbl>       <dbl>       <dbl>
    ##  1 Austra~     0.149      0.255      0.270       0.594       0.149
    ##  2 Austria     0.357      0.442      0.527       0.647       0.397
    ##  3 Belgium     0.584      0.580      0.639       0.706       0.584
    ##  4 Canada      0.554      0.588      0.611       0.646       0.686
    ##  5 Czech ~     0.383      0.506      0.464       0.555       0.541
    ##  6 Denmark     0.476      0.51       0.688       0.729       0.863
    ##  7 Estonia     0.089      0.157      0.305       0.366       0.133
    ##  8 Finland     0.315      0.479      0.532       0.641       0.403
    ##  9 France      0.592      0.593      0.696       0.696       0.611
    ## 10 Germany     0.6        0.632      0.627       0.698       0.625
    ## # ... with 20 more rows, and 1 more variable: UC1000_first <dbl>

![](xsigmacdt_files/figure-gfm/f%20Plot%20and%20Country-1.png)<!-- -->
