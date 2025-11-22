# Fit Measures By Models

Groups fit measures into a table with models as columns.

## Usage

``` r
fitMeasures_by_models(object_list, ...)
```

## Arguments

- object_list:

  A named list of 'lavaan'-class objects.

- ...:

  Optional arguments to be passed to
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).

## Value

A data-frame-like object of the class `fit_by_models`, which has a
`print` method (see
[`print.fit_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/print.fit_by_models.md)).

## Details

It call
[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
to compute for each model the fit measures supported by `lavaan`, and
combine them into a data frame. Users can then use the print method
([`print.fit_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/print.fit_by_models.md))
to customize the printout.

To be consist with full `lavaan` output, the names used in
[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
are used.

This function is intended for a simple and compact table of fit measures
for quick preview. For a well-organized layout, call
[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
and set `output` to `"text"`.

## See also

[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.

## Examples

``` r
library(lavaan)
set.seed(5478374)
n <- 50
x <- runif(n) - .5
m <- .40 * x + rnorm(n, 0, sqrt(1 - .40))
y <- .30 * m + rnorm(n, 0, sqrt(1 - .30))
dat <- data.frame(x = x, y = y, m = m)
model1 <-
'
m ~ a*x
y ~ b*m
ab := a*b
'
fit1 <- sem(model1, data = dat, fixed.x = FALSE)
model2 <-
'
m ~ a*x
y ~ b*m + x
ab := a*b
'
fit2 <- sem(model2, data = dat, fixed.x = FALSE)

fitMeasures_by_models(list(no_direct = fit1,
                           direct = fit2))
#>                                  no_direct  direct
#> Number of parameters                 5.000   6.000
#> Test statistic                       0.020   0.000
#> Degrees of freedom                   1.000   0.000
#> P-value                              0.887      NA
#> CFI                                  1.000   1.000
#> TLI                                  2.722   1.000
#> AIC                                241.556 243.536
#> BIC                                251.116 255.008
#> Sample-size adjusted BIC (SABIC)   235.422 236.175
#> RMSEA                                0.000   0.000
#> RMSEA CI confidence level            0.900   0.900
#> RMSEA CI: Lower bound                0.000   0.000
#> RMSEA CI: Upper bound                0.181   0.000
#> RMSEA P-Close H0 Value               0.050   0.050
#> P-Value RMSEA P-Close                0.894      NA
#> SRMR                                 0.008   0.000
```
