# Sort a Parameter Estimates Table

Sort a parameter estimates table or a similar table in`lavaan` by common
fields such as `op` (operator) and `lhs` (left- hand side).

## Usage

``` r
sort_by(
  object,
  by = c("op", "lhs", "rhs"),
  op_priority = c("=~", "~", "~~", ":=", "~1", "|", "~*~"),
  number_rows = TRUE
)
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

- by:

  A character vector of the columns for filtering. Default is
  `c("op", "lhs", "rhs")`.

- op_priority:

  How rows are sorted by `op`. Default is
  `c("=~", "~", "~~", ":=", "~1", "|", "~*~")`. Can set only a few of
  the operators, e.g., `c("~", "~~")`. Other operators will be placed to
  the end with orders not changed.

- number_rows:

  Whether the row names will be set to row numbers after sorting *if*
  the row names of `object` is equal to row numbers. Default is `TRUE`.

## Value

The sorted version of the input object.

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
parameterEstimates(fit1)
#>   lhs op rhs label   est    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.569 0.343 1.660  0.097   -0.103    1.240
#> 2   y  ~   m     b 0.219 0.153 1.430  0.153   -0.081    0.519
#> 3   m ~~   m       0.460 0.092 5.000  0.000    0.280    0.641
#> 4   y ~~   y       0.570 0.114 5.000  0.000    0.347    0.794
#> 5   x ~~   x       0.078 0.016 5.000  0.000    0.048    0.109
#> 6  ab := a*b    ab 0.125 0.115 1.083  0.279   -0.101    0.350
parameterEstimates(fit2)
#>   lhs op rhs label   est    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.569 0.343 1.660  0.097   -0.103    1.240
#> 2   y  ~   m     b 0.214 0.157 1.360  0.174   -0.094    0.522
#> 3   y  ~   x       0.056 0.392 0.142  0.887   -0.712    0.823
#> 4   m ~~   m       0.460 0.092 5.000  0.000    0.280    0.641
#> 5   y ~~   y       0.570 0.114 5.000  0.000    0.347    0.793
#> 6   x ~~   x       0.078 0.016 5.000  0.000    0.048    0.109
#> 7  ab := a*b    ab 0.122 0.116 1.052  0.293   -0.105    0.348
out <- group_by_models(list(no_direct = fit1,
                            direct = fit2),
                        col_names = c("est", "pvalue"))
out
#>   lhs op rhs est_no_direct est_direct pvalue_no_direct pvalue_direct
#> 1   m  ~   x         0.569      0.569            0.097         0.097
#> 2   y  ~   m         0.219      0.214            0.153         0.174
#> 3   y  ~   x            NA      0.056               NA         0.887
#> 4   m ~~   m         0.460      0.460            0.000         0.000
#> 5   x ~~   x         0.078      0.078            0.000         0.000
#> 6   y ~~   y         0.570      0.570            0.000         0.000
#> 7  ab := a*b         0.125      0.122            0.279         0.293
sort_by(out)
#>   lhs op rhs est_no_direct est_direct pvalue_no_direct pvalue_direct
#> 1   m  ~   x         0.569      0.569            0.097         0.097
#> 2   y  ~   m         0.219      0.214            0.153         0.174
#> 3   y  ~   x            NA      0.056               NA         0.887
#> 4   m ~~   m         0.460      0.460            0.000         0.000
#> 5   x ~~   x         0.078      0.078            0.000         0.000
#> 6   y ~~   y         0.570      0.570            0.000         0.000
#> 7  ab := a*b         0.125      0.122            0.279         0.293
sort_by(out, op_priority = c("~", ":="))
#>   lhs op rhs est_no_direct est_direct pvalue_no_direct pvalue_direct
#> 1   m  ~   x         0.569      0.569            0.097         0.097
#> 2   y  ~   m         0.219      0.214            0.153         0.174
#> 3   y  ~   x            NA      0.056               NA         0.887
#> 4  ab := a*b         0.125      0.122            0.279         0.293
#> 5   m ~~   m         0.460      0.460            0.000         0.000
#> 6   x ~~   x         0.078      0.078            0.000         0.000
#> 7   y ~~   y         0.570      0.570            0.000         0.000
sort_by(out, by = c("op", "rhs"))
#>   lhs op rhs est_no_direct est_direct pvalue_no_direct pvalue_direct
#> 1   y  ~   m         0.219      0.214            0.153         0.174
#> 2   m  ~   x         0.569      0.569            0.097         0.097
#> 3   y  ~   x            NA      0.056               NA         0.887
#> 4   m ~~   m         0.460      0.460            0.000         0.000
#> 5   x ~~   x         0.078      0.078            0.000         0.000
#> 6   y ~~   y         0.570      0.570            0.000         0.000
#> 7  ab := a*b         0.125      0.122            0.279         0.293

```
