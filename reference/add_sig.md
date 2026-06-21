# Add Significant Test Results

It inserts columns to denote whether a parameter is significant.

## Usage

``` r
add_sig(object, ..., standardized = FALSE, na_str = "", use = "pvalue")
```

## Arguments

- object:

  A 'lavaan'-class object or the output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  or
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).
  May also work on an `est_table`-class object returned by functions
  like
  [`group_by_dvs()`](https://sfcheung.github.io/semhelpinghands/reference/group_estimates.md)
  but there is no guarantee.

- ...:

  Optional arguments to be passed to
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  or
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).

- standardized:

  Whether standardized solution is needed. If `TRUE`,
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
  will be called. If `FALSE`, the default,
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  will be called. Ignored if a table if estimates is supplied.

- na_str:

  The string to be used for parameters with no significant tests. For
  example, fixed parameters. Default is `""`.

- use:

  A character vector of one or more strings. If `"pvalue"` is in the
  vector, *p*-values will be used. If `"ci"` is in the vector,
  confidence intervals appeared in `ci.lower` and `ci.upper` will be
  used. If `"boot.ci"` is in the vector and the columns `boot.ci.lower`
  and `boot.ci.upper` are available, these columns will be used. Note
  that `ci.lower` and `ci.upper` can also be bootstrap confidence
  intervals in some tables if `se = "boot"` is used.

## Value

The output of
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
or
[`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html),
with one or two columns inserted after the parameter estimates to denote
the significant test results.

## Details

The function calls
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
or
[`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
and checks the columns `pvalue`, `ci.lower` and `ci.upper`, and/or
`boot.ci.lower` and `boot.ci.upper` and then inserts columns to denote
for each parameter estimate whether it is significant based on the
requested criteria.

## See also

[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
and
[`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

library(lavaan)
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
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

# Add "*" based on 'pvalue'
add_sig(fit)
#>   lhs op rhs label   est sig    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.569     0.343 1.660  0.097   -0.103    1.240
#> 2   y  ~   m     b 0.219     0.153 1.430  0.153   -0.081    0.519
#> 3   m ~~   m       0.460 *** 0.092 5.000  0.000    0.280    0.641
#> 4   y ~~   y       0.570 *** 0.114 5.000  0.000    0.347    0.794
#> 5   x ~~   x       0.078 *** 0.016 5.000  0.000    0.048    0.109
#> 6  ab := a*b    ab 0.125     0.115 1.083  0.279   -0.101    0.350

# Add "*" for standardized solution
add_sig(fit, standardized = TRUE)
#>   lhs op rhs label est.std sig    se      z pvalue ci.lower ci.upper
#> 1   m  ~   x     a   0.229     0.134  1.706  0.088   -0.034    0.491
#> 2   y  ~   m     b   0.198     0.136  1.459  0.145   -0.068    0.464
#> 3   m ~~   m         0.948 *** 0.061 15.466  0.000    0.828    1.068
#> 4   y ~~   y         0.961 *** 0.054 17.840  0.000    0.855    1.066
#> 5   x ~~   x         1.000     0.000     NA     NA    1.000    1.000
#> 6  ab := a*b    ab   0.045     0.041  1.096  0.273   -0.036    0.126

# Add "*" based on confidence interval
add_sig(fit, use = "ci")
#>   lhs op rhs label   est  ci    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.569     0.343 1.660  0.097   -0.103    1.240
#> 2   y  ~   m     b 0.219     0.153 1.430  0.153   -0.081    0.519
#> 3   m ~~   m       0.460 Sig 0.092 5.000  0.000    0.280    0.641
#> 4   y ~~   y       0.570 Sig 0.114 5.000  0.000    0.347    0.794
#> 5   x ~~   x       0.078 Sig 0.016 5.000  0.000    0.048    0.109
#> 6  ab := a*b    ab 0.125     0.115 1.083  0.279   -0.101    0.350

# Add "*" for standardized solution based on confidence interval
add_sig(fit, standardized = TRUE, use = "ci")
#>   lhs op rhs label est.std  ci    se      z pvalue ci.lower ci.upper
#> 1   m  ~   x     a   0.229     0.134  1.706  0.088   -0.034    0.491
#> 2   y  ~   m     b   0.198     0.136  1.459  0.145   -0.068    0.464
#> 3   m ~~   m         0.948 Sig 0.061 15.466  0.000    0.828    1.068
#> 4   y ~~   y         0.961 Sig 0.054 17.840  0.000    0.855    1.066
#> 5   x ~~   x         1.000     0.000     NA     NA    1.000    1.000
#> 6  ab := a*b    ab   0.045     0.041  1.096  0.273   -0.036    0.126

# Add "*" for standardized solution based on confidence interval
# and 'pvalue'.
add_sig(fit, standardized = TRUE, use = c("ci", "pvalue"))
#>   lhs op rhs label est.std sig  ci    se      z pvalue ci.lower ci.upper
#> 1   m  ~   x     a   0.229         0.134  1.706  0.088   -0.034    0.491
#> 2   y  ~   m     b   0.198         0.136  1.459  0.145   -0.068    0.464
#> 3   m ~~   m         0.948 *** Sig 0.061 15.466  0.000    0.828    1.068
#> 4   y ~~   y         0.961 *** Sig 0.054 17.840  0.000    0.855    1.066
#> 5   x ~~   x         1.000         0.000     NA     NA    1.000    1.000
#> 6  ab := a*b    ab   0.045         0.041  1.096  0.273   -0.036    0.126

# Can also accept a parameter estimates table
est <- parameterEstimates(fit)
add_sig(est)
#>   lhs op rhs label   est sig    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.569     0.343 1.660  0.097   -0.103    1.240
#> 2   y  ~   m     b 0.219     0.153 1.430  0.153   -0.081    0.519
#> 3   m ~~   m       0.460 *** 0.092 5.000  0.000    0.280    0.641
#> 4   y ~~   y       0.570 *** 0.114 5.000  0.000    0.347    0.794
#> 5   x ~~   x       0.078 *** 0.016 5.000  0.000    0.048    0.109
#> 6  ab := a*b    ab 0.125     0.115 1.083  0.279   -0.101    0.350

# So it can be used with some other functions in semhelpinghands
add_sig(filter_by(est, op = "~"))
#>   lhs op rhs label   est sig    se    z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.569     0.343 1.66  0.097   -0.103    1.240
#> 2   y  ~   m     b 0.219     0.153 1.43  0.153   -0.081    0.519

# Piping can also be used
est |> filter_by(op = "~") |>
       add_sig()
#>   lhs op rhs label   est sig    se    z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.569     0.343 1.66  0.097   -0.103    1.240
#> 2   y  ~   m     b 0.219     0.153 1.43  0.153   -0.081    0.519
```
