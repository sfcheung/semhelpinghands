# Refit a 'lavaan'-Model by Several Estimators

Refit a model in 'lavaan' by several lavaan-supported estimators

## Usage

``` r
compare_estimators(object, estimators = NULL)

se_ratios(fit_list, reference = NULL)
```

## Arguments

- object:

  A 'lavaan'-class object.

- estimators:

  A character vector of the estimator supported by the `estimator`
  argument of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) and
  its wrappers, such as
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- fit_list:

  The output of `compare_estimators()`.

- reference:

  The name of the reference method (ratios will be equal to one). Must
  be one of the estimator used on `compare_estimators()`. If `NULL`, the
  first estimator will be used.

## Value

A list of `lavaan` outputs, each of them is an update of the original
output using one of the estimators.

## Details

The function simply uses
[`lapply()`](https://rdrr.io/r/base/lapply.html) and
[`update()`](https://rdrr.io/r/stats/update.html) to rerun the analysis
once for each of the estimator using `update(object, estimator = "x"`,
`x` being the estimator.

The results can then be compared using
[`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md).

## Functions

- `compare_estimators()`: Refit the model with different estimators.

- `se_ratios()`: A wrapper of
  [`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md)
  that computes the ratios of standard errors of different methods to
  those of one method.

## See also

[`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md)

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
model <-
'
m ~ a*x
y ~ b*m
ab := a*b
'

fit <- sem(model, data = dat, fixed.x = FALSE)

# Refit the model by three different estimators
fit_more <- compare_estimators(fit, estimator = c("GLS", "MLR", "ML"))

# Use group_by_models to compare the estimates
group_by_models(fit_more, col_names = c("est", "pvalue"))
#>   lhs op rhs est_GLS est_MLR est_ML pvalue_GLS pvalue_MLR pvalue_ML
#> 1   m  ~   x   0.569   0.569  0.569      0.100      0.062     0.097
#> 2   y  ~   m   0.219   0.219  0.219      0.157      0.116     0.153
#> 3   m ~~   m   0.470   0.460  0.460      0.000      0.000     0.000
#> 4   x ~~   x   0.080   0.078  0.078      0.000      0.000     0.000
#> 5   y ~~   y   0.581   0.570  0.570      0.000      0.000     0.000
#> 6  ab := a*b   0.125   0.125  0.125      0.288      0.259     0.279

# Use se_ratios to compare standard errors
se_ratios(fit_more, reference = "ML")
#>   lhs op rhs se_GLS se_MLR se_ML ratio_GLS ratio_MLR ratio_ML
#> 1   m  ~   x  0.346  0.305 0.343     1.011     0.889        1
#> 2   y  ~   m  0.155  0.139 0.153     1.010     0.909        1
#> 3   m ~~   m  0.095  0.089 0.092     1.031     0.966        1
#> 4   x ~~   x  0.016  0.011 0.016     1.030     0.719        1
#> 5   y ~~   y  0.118  0.111 0.114     1.030     0.973        1
#> 6  ab := a*b  0.117  0.110 0.115     1.020     0.960        1
```
