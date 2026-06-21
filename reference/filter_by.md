# Filter a Parameter Estimates Table

Filter parameter estimates table and similar tables in`lavaan` by common
fields such as `op` (operator).

## Usage

``` r
filter_by(object, op = NULL, lhs = NULL, rhs = NULL, group = NULL, fit = NULL)
```

## Arguments

- object:

  The output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html),
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html),
  or a `lavaan.data.frame` object. May also work on an `est_table`-class
  object returned by functions like
  [`group_by_dvs()`](https://sfcheung.github.io/semhelpinghands/reference/group_estimates.md)
  but there is no guarantee.

- op:

  A character vector of the operators (`op`) for filtering. Common
  operators are `"~"`, `"~~"`, `"=~"`, `":="`, and "`~1`".

- lhs:

  A character vector of names in the `lhs` column.

- rhs:

  A character vector of names in the `rhs` column.

- group:

  A vector of either the group numbers in the `group` column of the
  labels of the groups. If labels are supplied, the original fit object
  must be supplied for extracting the group labels.

- fit:

  The original fit object. Usd when `group` is a vector of the group
  labels.

## Value

The filtered version of the input object.

## Details

This functions accepts the output of
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
and
[`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
and filter the rows by commonly used field.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

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

model_gp <-
'
m ~ c(a1, a2)*x
y ~ c(b1, b2)*m
a1b1 := a1*b1
a2b2 := a2*b2
'
dat$gp <- sample(c("gp1", "gp2"), n, replace = TRUE)
fit_gp <- sem(model_gp, dat, group = "gp", warn = FALSE)

est <- parameterEstimates(fit)
est_gp <- parameterEstimates(fit_gp)

filter_by(est, op = "~")
#>   lhs op rhs label   est    se    z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.569 0.343 1.66  0.097   -0.103    1.240
#> 2   y  ~   m     b 0.219 0.153 1.43  0.153   -0.081    0.519

filter_by(est, op = "~", lhs = "y")
#>   lhs op rhs label   est    se    z pvalue ci.lower ci.upper
#> 2   y  ~   m     b 0.219 0.153 1.43  0.153   -0.081    0.519

filter_by(est, rhs = c("m", "x"), op = "~")
#>   lhs op rhs label   est    se    z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.569 0.343 1.66  0.097   -0.103    1.240
#> 2   y  ~   m     b 0.219 0.153 1.43  0.153   -0.081    0.519

filter_by(est_gp, group = 2)
#>    lhs op rhs block group label    est    se      z pvalue ci.lower ci.upper
#> 9    m  ~   x     2     2    a2  0.574 0.417  1.375  0.169   -0.244    1.391
#> 10   y  ~   m     2     2    b2  0.206 0.231  0.892  0.372   -0.246    0.658
#> 11   m ~~   m     2     2        0.389 0.104  3.742  0.000    0.185    0.593
#> 12   y ~~   y     2     2        0.619 0.166  3.742  0.000    0.295    0.944
#> 13   x ~~   x     2     2        0.080 0.000     NA     NA    0.080    0.080
#> 14   m ~1         2     2       -0.144 0.119 -1.206  0.228   -0.377    0.090
#> 15   y ~1         2     2       -0.297 0.154 -1.933  0.053   -0.598    0.004
#> 16   x ~1         2     2       -0.042 0.000     NA     NA   -0.042   -0.042

# If the fit object is supplied, can filter
# by group label
filter_by(est_gp, group = "gp2", fit = fit_gp)
#>   lhs op rhs block group label    est    se      z pvalue ci.lower ci.upper
#> 1   m  ~   x     1     1    a1  0.526 0.569  0.925  0.355   -0.589    1.640
#> 2   y  ~   m     1     1    b1  0.211 0.201  1.050  0.294   -0.183    0.605
#> 3   m ~~   m     1     1        0.540 0.163  3.317  0.001    0.221    0.859
#> 4   y ~~   y     1     1        0.499 0.150  3.317  0.001    0.204    0.793
#> 5   x ~~   x     1     1        0.076 0.000     NA     NA    0.076    0.076
#> 6   m ~1         1     1       -0.004 0.157 -0.024  0.981   -0.311    0.303
#> 7   y ~1         1     1       -0.168 0.151 -1.113  0.266   -0.463    0.128
#> 8   x ~1         1     1       -0.007 0.000     NA     NA   -0.007   -0.007
filter_by(est_gp, group = "gp2", fit = fit_gp, op = "~")
#>   lhs op rhs block group label   est    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x     1     1    a1 0.526 0.569 0.925  0.355   -0.589    1.640
#> 2   y  ~   m     1     1    b1 0.211 0.201 1.050  0.294   -0.183    0.605

# Select user-defined parameters
filter_by(est_gp, op = ":=")
#>     lhs op   rhs block group label   est    se     z pvalue ci.lower ci.upper
#> 17 a1b1 := a1*b1     0     0  a1b1 0.111 0.160 0.694  0.488   -0.203    0.425
#> 18 a2b2 := a2*b2     0     0  a2b2 0.118 0.158 0.748  0.454   -0.191    0.427

# Can be used with some other functions in semhelpinghands
# Piping can also be used
est_gp |> filter_by(op = "~", group = "gp2", fit = fit_gp) |>
          add_sig()
#>   lhs op rhs block group label   est sig    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x     1     1    a1 0.526     0.569 0.925  0.355   -0.589    1.640
#> 2   y  ~   m     1     1    b1 0.211     0.201 1.050  0.294   -0.183    0.605

```
