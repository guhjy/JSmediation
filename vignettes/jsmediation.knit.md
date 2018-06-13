---
title: "mediation analysis with JSmediation"
author: "Cédric Batailler"
date: "2018-06-13"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{mediation analysis with JSmediation}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# R implementation

`JSmediation` offers helpers to conduct a joint significant mediation test. 
With this package, you can type a mediation model the way you think it, 
`JSmediation` dealing with fitting underlying models.

First, we will load the `JSmediation` package in our environnement so we can use
its functions.


```r
library(JSmediation)
```

We will use a data set from Ho, Kteily and Chen (2017). In this experiment, 
authors hypothised that presenting a text stating that Black-White biracials 
were discriminated would lead Black participants to associate Black-White 
biracials more with their lower status parent group than their higher status 
parent group, according to the rule of hypodescent. In this experiment, the 
authors tested if this effect was mediated by the sense of linked fate between 
discriminated Black-White participants and Black participants. This data set 
is shipped with the `JSmediation` and is called `ho_et_al`.


```r
head(ho_et_al)
#>   id           condition    sdo linkedfate hypodescent
#> 1  2  Low discrimination 1.8125      6.000    2.333333
#> 2  3 High discrimination 1.5625      5.875    6.000000
#> 3  4 High discrimination 1.7500      6.625    6.000000
#> 4  5  Low discrimination 4.2500      5.125    5.666667
#> 5  6  Low discrimination 1.9375      4.375    4.000000
#> 6  9 High discrimination 2.8750      3.750    4.000000
```

The first step to test for a mediation of discrimination on hypodescent by 
linked fate is to build a new variable which will represent the discrimination
condition, but with a contrast code. To do so, we will use the `build_contrast` 
function.


```r
ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                         "Low discrimination",
                                         "High discrimination")

head(ho_et_al)
#>   id           condition    sdo linkedfate hypodescent condition_c
#> 1  2  Low discrimination 1.8125      6.000    2.333333        -0.5
#> 2  3 High discrimination 1.5625      5.875    6.000000         0.5
#> 3  4 High discrimination 1.7500      6.625    6.000000         0.5
#> 4  5  Low discrimination 4.2500      5.125    5.666667        -0.5
#> 5  6  Low discrimination 1.9375      4.375    4.000000        -0.5
#> 6  9 High discrimination 2.8750      3.750    4.000000         0.5
```

Now that we have a data frame we can use for the analysis, we will use the 
`mdt_simple` function to fit a simple mediation model. Note that any model 
`JSmediation` support is implemented in a `mdt_*` function.


```r
my_model <- 
  mdt_simple(ho_et_al,
             IV = condition_c,
             DV = hypodescent,
             M  = linkedfate)
```

We now have an object of class `mediation_model`, we can now wether our data 
support a simple moderation. 

Before looking into the results, we will check for OLS assumptions because 
underlying models used in a joint significant test are actually linear models.

We will extract one model and plot diagnostic plots using the `plot` method for 
object of class `lm`. To do so, we will use the `extract_models` method. To use
this function, we will use the `index` argument which allow us to ask for a 
specific model. Note that `index` allow name of model as well as integer value
to be requested.











