# Sample Dataset: Simple Mediation

A simple mediation model.

## Usage

``` r
simple_mediation
```

## Format

A data frame with 100 rows and 5 variables:

- x:

  Predictor. Numeric.

- m:

  Mediator. Numeric.

- y:

  Outcome variable. Numeric.

- city:

  Group variable: "City A" or "City B". String.

## Examples

``` r
library(lavaan)
data(simple_mediation)
mod <-
"
m ~ a * x
y ~ b * m + x
ab := a * b
"
fit <- sem(mod, simple_mediation, fixed.x = FALSE)
parameterEstimates(fit)
#>   lhs op rhs label   est    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.374 0.171 2.182  0.029    0.038    0.710
#> 2   y  ~   m     b 0.375 0.173 2.176  0.030    0.037    0.714
#> 3   y  ~   x       0.096 0.303 0.318  0.751   -0.497    0.689
#> 4   m ~~   m       2.777 0.393 7.071  0.000    2.007    3.547
#> 5   y ~~   y       8.272 1.170 7.071  0.000    5.979   10.565
#> 6   x ~~   x       0.946 0.134 7.071  0.000    0.684    1.209
#> 7  ab := a*b    ab 0.140 0.091 1.541  0.123   -0.038    0.319
mod_gp <-
"
m ~ c(a1, a2) * x
y ~ c(b1, b2) * m + x
a1b1 := a1 * b1
a2b2 := a2 * b2
ab_diff := a1b1 - a2b2
"
fit_gp <- sem(mod_gp, simple_mediation, fixed.x = FALSE, group = "city")
parameterEstimates(fit_gp)
#>        lhs op       rhs block group   label    est    se      z pvalue ci.lower
#> 1        m  ~         x     1     1      a1  0.162 0.229  0.708  0.479   -0.287
#> 2        y  ~         m     1     1      b1  0.177 0.243  0.728  0.466   -0.300
#> 3        y  ~         x     1     1          0.305 0.376  0.809  0.418   -0.433
#> 4        m ~~         m     1     1          2.382 0.502  4.743  0.000    1.398
#> 5        y ~~         y     1     1          6.354 1.340  4.743  0.000    3.729
#> 6        x ~~         x     1     1          1.008 0.212  4.743  0.000    0.591
#> 7        m ~1               1     1         12.062 1.200 10.049  0.000    9.709
#> 8        y ~1               1     1          2.075 3.531  0.588  0.557   -4.845
#> 9        x ~1               1     1          5.140 0.150 34.349  0.000    4.846
#> 10       m  ~         x     2     2      a2  0.678 0.235  2.879  0.004    0.216
#> 11       y  ~         m     2     2      b2  0.556 0.257  2.166  0.030    0.053
#> 12       y  ~         x     2     2         -0.199 0.481 -0.413  0.680   -1.141
#> 13       m ~~         m     2     2          2.656 0.507  5.244  0.000    1.663
#> 14       y ~~         y     2     2          9.626 1.836  5.244  0.000    6.028
#> 15       x ~~         x     2     2          0.871 0.166  5.244  0.000    0.546
#> 16       m ~1               2     2         10.336 1.175  8.794  0.000    8.032
#> 17       y ~1               2     2         -0.474 3.471 -0.137  0.891   -7.276
#> 18       x ~1               2     2          4.904 0.126 38.964  0.000    4.657
#> 19    a1b1 :=     a1*b1     0     0    a1b1  0.029 0.057  0.508  0.612   -0.082
#> 20    a2b2 :=     a2*b2     0     0    a2b2  0.377 0.218  1.731  0.083   -0.050
#> 21 ab_diff := a1b1-a2b2     0     0 ab_diff -0.348 0.225 -1.547  0.122   -0.789
#>    ci.upper
#> 1     0.612
#> 2     0.655
#> 3     1.042
#> 4     3.366
#> 5     8.979
#> 6     1.424
#> 7    14.414
#> 8     8.995
#> 9     5.433
#> 10    1.139
#> 11    1.059
#> 12    0.744
#> 13    3.649
#> 14   13.223
#> 15    1.197
#> 16   12.640
#> 17    6.328
#> 18    5.150
#> 19    0.140
#> 20    0.804
#> 21    0.093
```
