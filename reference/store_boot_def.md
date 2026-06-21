# Store Bootstrap Estimates of User-Defined Parameters

It receives a
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
object fitted with bootstrapping standard errors requested, computes the
user-defined parameters in each bootstrap samples, and returns a
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
object with the estimates stored.

## Usage

``` r
store_boot_def(object, force_run = FALSE)

get_boot_def(object)
```

## Arguments

- object:

  A 'lavaan'-class object, fitted with 'se = "boot"'.

- force_run:

  If `TRUE`, will skip checks and run models without checking the
  estimates. For internal use. Default is `FALSE`.

## Value

`store_boot_def()` returns the fit object set to `object`, with the
bootstrap values of user-defined parameters in the bootstrap samples, as
a matrix, stored in the slot `external` of `object` under the name
`shh_boot_def`.

`get_boot_def()` returns a matrix of the stored bootstrap estimates of
user-defined parameters

## Details

**NOTE**: A new package,
[`semboottools`](https://yangzhen1999.github.io/semboottools/) (Yang &
Cheung, 2026), can do what
[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
does, with more features, as well as some diagnostic functions. The
function
[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md),
as well as its helpers, such as `store_boot_def()` and
[`plot_boot()`](https://sfcheung.github.io/semhelpinghands/reference/plot_boot.md),
will stay in this package, but will not be further developed. Users are
recommended to use `semboottools::standardizedSolution_boot()` and other
functions in
[`semboottools`](https://yangzhen1999.github.io/semboottools/) instead
of this function.

[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) and its
wrappers, such as
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html), stores the
estimates of free parameters in each bootstrap sample if bootstrapping
is requested. However, if a model has user-defined parameters, their
values in each bootstrap sample are not stored. `store_boot_def()`
computes the retrieves the stored bootstrap estimates and computes the
values of user-defined parameters. The values are then stored in the
slot `external` of the object, in the element `shh_boot_def`. The
bootstrap estimates can then be used by other functions for diagnostics
purposes.

`get_boot_def()` extracts the bootstrap estimates of user-defined
parameters from a 'lavaan'-class object. If none is stored, `NULL` is
returned.

`store_boot_def()` is usually used with diagnostic functions such as
[`plot_boot()`](https://sfcheung.github.io/semhelpinghands/reference/plot_boot.md).

## References

Yang, W., & Cheung, S. F. (2026). Forming bootstrap confidence intervals
and examining bootstrap distributions of standardized coefficients in
structural equation modelling: A simplified workflow using the R package
*semboottools*. *Behavior Research Methods, 58*(2), 38.
[doi:10.3758/s13428-025-02911-z](https://doi.org/10.3758/s13428-025-02911-z)

## See also

[`plot_boot()`](https://sfcheung.github.io/semhelpinghands/reference/plot_boot.md)

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

# Should set bootstrap to at least 2000 in real studies
fit <- sem(model, data = dat, fixed.x = FALSE,
           se = "boot",
           bootstrap = 100)
summary(fit)
#> lavaan 0.6-21 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                            50
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.020
#>   Degrees of freedom                                 1
#>   P-value (Chi-square)                           0.887
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                            Bootstrap
#>   Number of requested bootstrap draws              100
#>   Number of successful bootstrap draws             100
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   m ~                                                 
#>     x          (a)    0.569    0.325    1.749    0.080
#>   y ~                                                 
#>     m          (b)    0.219    0.146    1.495    0.135
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m                 0.460    0.086    5.381    0.000
#>    .y                 0.570    0.110    5.178    0.000
#>     x                 0.078    0.012    6.782    0.000
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     ab                0.125    0.125    0.997    0.319
#> 

# store_boot_def() is usually used with plot_boot()
# First, store the bootstrap estimates of user-defined
# parameters
fit_with_boot_def <- store_boot_def(fit)
# Second, plot the distribution of the bootstrap estimates of
# 'ab'
plot_boot(fit_with_boot_def, "ab", standardized = FALSE)
```
