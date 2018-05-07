
<!-- README.md is generated from README.Rmd. Please edit that file -->
JSmediation
===========

[![Travis build status](https://travis-ci.org/cedricbatailler/JSmediation.svg?branch=master)](https://travis-ci.org/cedricbatailler/JSmediation)

The goal of `JSmediation` is to provide a set of functions to conduct a joint significant test in a context of moderation analysis. Note that `JSmediation` is still under active developement.

Installation
------------

<!--
You can install the released version of JSmediation from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("JSmediation")
```
-->
You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cedricbatailler/JSmediation")
```

How to use JSmediation
----------------------

``` r
library(JSmediation)
```

The `JSmediation` package contains several functions as well as an example data set (`ho_et_al`) that can be used as an example. This data set comes from Ho et al. (2017; Exp. 3) and contains variables to test a simple mediation. As a simple example, we will conduct a joint significance test of the indirect effect of discrimination on hypodescent passing by linked fate.

We will first recode `condition` variable which is a character to a contrast code using the `build_contrast` function.

``` r
data("ho_et_al")

ho_et_al$condition_c <- build_contrast(ho_et_al$condition, 
                                       "High discrimination",
                                       "Low discrimination")

head(ho_et_al)
#>   id           condition    sdo linkedfate hypodescent condition_c
#> 1  2  Low discrimination 1.8125      6.000    2.333333         0.5
#> 2  3 High discrimination 1.5625      5.875    6.000000        -0.5
#> 3  4 High discrimination 1.7500      6.625    6.000000        -0.5
#> 4  5  Low discrimination 4.2500      5.125    5.666667         0.5
#> 5  6  Low discrimination 1.9375      4.375    4.000000         0.5
#> 6  9 High discrimination 2.8750      3.750    4.000000        -0.5
```

Now, we can conduct a joint significant test using the `mdt_simple` function.

``` r
JS_model <- mdt_simple(ho_et_al, 
                       DV = hypodescent, 
                       IV = condition_c, 
                       M  = linkedfate)

JS_model
#> Test of mediation (simple mediation)
#> ==============================================
#> 
#> Variables:
#> 
#> - IV: condition_c 
#> - DV: hypodescent 
#> - M: linkedfate 
#> 
#> Paths:
#> 
#> ====  ==============  =====  =======================
#> Path  Point estimate     SE  APA                    
#> ====  ==============  =====  =======================
#> a             -0.772  0.085  t(822) = 9.10, p < .001
#> b              0.187  0.033  t(821) = 5.75, p < .001
#> c             -0.171  0.081  t(822) = 2.13, p = .034
#> c'            -0.027  0.083  t(821) = 0.33, p = .742
#> ====  ==============  =====  =======================
#> 
#> Indirect effect index:
#> 
#> Indirect effect index is not computed by default.
#> Please use add_index() to compute it.
#> 
#> Fitted models:
#> 
#> - X -> Y 
#> - X -> M 
#> - X + M -> Y
```

References
----------

Ho, A. K., Kteily, N. S., & Chen, J. M. (2017). “You’re one of us”: Black Americans’ use of hypodescent and its association with egalitarianism. *Journal of Personality and Social Psychology*, *113*(5), 753‑768. doi: 10.1037/pspi0000107
