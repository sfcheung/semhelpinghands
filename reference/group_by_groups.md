# Group Estimates By Groups

Groups parameter estimates or other information such as *p*-values into
a table with groups as columns and parameters as rows.

## Usage

``` r
group_by_groups(
  object,
  ...,
  col_names = "est",
  group_first = TRUE,
  group_labels = NULL,
  fit = NULL,
  use_standardizedSolution = FALSE
)
```

## Arguments

- object:

  A 'lavaan'-class object or the output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  or
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).

- ...:

  Optional arguments to be passed to
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  Ignored if object is an output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  or
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).

- col_names:

  A vector of the column names in the parameter estimate tables to be
  included. Default is `"est"`.

- group_first:

  If `TRUE`, the columns will be grouped by groups first and then by
  columns in the parameter estimates tables. Default is `TRUE`.

- group_labels:

  A character vector of group labels. Will be assigned to group id = 1,
  2, 3, etc. If not provided. will try to be retrieved from `object` if
  it is a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object.

- fit:

  Optional. A
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object. If `object` is a parameter estimates table and `group_labels`
  is `NULL`, it will try to retrieve the group labels from `fit` is
  supplied.

- use_standardizedSolution:

  If `TRUE` and `object` is not an estimates table, then
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
  will be used to generate the table. If `FALSE`, the default, then
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  will be used if necessary.

## Value

A data-frame-like object of the class `est_table`.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

library(lavaan)
set.seed(5478374)
n <- 100
x <- runif(n) - .5
m <- .40 * x + rnorm(n, 0, sqrt(1 - .40))
y <- .30 * m + rnorm(n, 0, sqrt(1 - .30))
city <- sample(c("City Alpha", "City Beta"), 100,
               replace = TRUE)
dat <- data.frame(x = x, y = y, m = m, city = city)
model <-
'
m ~ c(a1, a2)*x
y ~ c(b1, b2)*m
a1b1 := a1*b1
a2b2 := a2*b2
'
fit <- sem(model, data = dat, fixed.x = FALSE,
           group = "city")
(est <- parameterEstimates(fit))
#>     lhs op   rhs block group label    est    se      z pvalue ci.lower ci.upper
#> 1     m  ~     x     1     1    a1  0.636 0.454  1.401  0.161   -0.253    1.526
#> 2     y  ~     m     1     1    b1  0.469 0.153  3.055  0.002    0.168    0.769
#> 3     m ~~     m     1     1        0.530 0.107  4.950  0.000    0.320    0.740
#> 4     y ~~     y     1     1        0.636 0.128  4.950  0.000    0.384    0.887
#> 5     x ~~     x     1     1        0.052 0.011  4.950  0.000    0.032    0.073
#> 6     m ~1           1     1       -0.144 0.108 -1.335  0.182   -0.355    0.067
#> 7     y ~1           1     1       -0.092 0.117 -0.786  0.432   -0.322    0.138
#> 8     x ~1           1     1       -0.062 0.033 -1.882  0.060   -0.126    0.003
#> 9     m  ~     x     2     2    a2  0.418 0.271  1.540  0.124   -0.114    0.950
#> 10    y  ~     m     2     2    b2  0.277 0.147  1.880  0.060   -0.012    0.565
#> 11    m ~~     m     2     2        0.413 0.082  5.050  0.000    0.253    0.574
#> 12    y ~~     y     2     2        0.478 0.095  5.050  0.000    0.292    0.663
#> 13    x ~~     x     2     2        0.110 0.022  5.050  0.000    0.067    0.153
#> 14    m ~1           2     2       -0.179 0.091 -1.961  0.050   -0.357    0.000
#> 15    y ~1           2     2       -0.125 0.100 -1.253  0.210   -0.320    0.070
#> 16    x ~1           2     2        0.051 0.046  1.103  0.270   -0.040    0.142
#> 17 a1b1 := a1*b1     0     0  a1b1  0.298 0.234  1.274  0.203   -0.161    0.757
#> 18 a2b2 := a2*b2     0     0  a2b2  0.116 0.097  1.191  0.234   -0.075    0.306

# Group them by groups
group_by_groups(fit)
#>   lhs op rhs est_City.Beta est_City.Alpha
#> 1   m  ~   x         0.636          0.418
#> 2   y  ~   m         0.469          0.277
#> 3   m ~~   m         0.530          0.413
#> 4   x ~~   x         0.052          0.110
#> 5   y ~~   y         0.636          0.478
#> 6   m ~1            -0.144         -0.179
#> 7   x ~1            -0.062          0.051
#> 8   y ~1            -0.092         -0.125

# Can also work on a parameter estimates table
# To have group labels, need to supply the fit object
group_by_groups(est, fit = fit)
#>   lhs op rhs est_City.Beta est_City.Alpha
#> 1   m  ~   x         0.636          0.418
#> 2   y  ~   m         0.469          0.277
#> 3   m ~~   m         0.530          0.413
#> 4   x ~~   x         0.052          0.110
#> 5   y ~~   y         0.636          0.478
#> 6   m ~1            -0.144         -0.179
#> 7   x ~1            -0.062          0.051
#> 8   y ~1            -0.092         -0.125

# Can be used with some other functions in semhelpinghands
# when used on a parameter estimates table
group_by_groups(filter_by(est, op = "~"), fit = fit)
#>   lhs op rhs est_City.Beta est_City.Alpha
#> 1   m  ~   x         0.636          0.418
#> 2   y  ~   m         0.469          0.277

# Also support piping
est |> filter_by(op = "~") |>
       group_by_groups(fit = fit)
#>   lhs op rhs est_City.Beta est_City.Alpha
#> 1   m  ~   x         0.636          0.418
#> 2   y  ~   m         0.469          0.277
```
