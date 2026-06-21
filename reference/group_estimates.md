# Group Estimates By Dependent or Independent Variables

Groups parameter estimates or other information such as p-values into a
table with dependent variables as columns and independent variables as
rows, or a transpose of this table.

## Usage

``` r
group_by_dvs(
  object,
  ...,
  col_name = "est",
  add_prefix = TRUE,
  group_first = FALSE,
  use_standardizedSolution = FALSE
)

group_by_ivs(
  object,
  ...,
  col_name = "est",
  add_prefix = TRUE,
  group_first = FALSE,
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
  Ignored if `object` is an output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  or
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).

- col_name:

  The column name of information to be grouped. Default is `"est"`. It
  accepts only one name.

- add_prefix:

  If `TRUE`, the default, `col_name` will be added as prefix to the
  column names of the output.

- group_first:

  If `TRUE`, the rows will be grouped by groups first and then by
  independent variables Ignored if the model has only one group. Default
  is `FALSE`.

- use_standardizedSolution:

  If `TRUE` and `object` is not an estimates table, then
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
  will be used to generate the table. If `FALSE`, the default, then
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  will be used if necessary.

## Value

A data-frame-like object of the class `est_table`.

## Details

It gets a 'lavaan'-class object or the output of
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
or
[`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
and group selected columns by "dependent" variables `group_by_dvs()` or
by "independent" variables `group_by_ivs()`.

"Dependent" variables are defined as variables on the left hand side of
the operator `~`.

"Independent" variables are defined as variables on the right hand side
of the operator `~`.

Note that a variable can both be a "dependent" variable and an
"independent" variable in a model.

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
parameterEstimates(fit)
#>   lhs op rhs label   est    se     z pvalue ci.lower ci.upper
#> 1   m  ~   x     a 0.569 0.343 1.660  0.097   -0.103    1.240
#> 2   y  ~   m     b 0.219 0.153 1.430  0.153   -0.081    0.519
#> 3   m ~~   m       0.460 0.092 5.000  0.000    0.280    0.641
#> 4   y ~~   y       0.570 0.114 5.000  0.000    0.347    0.794
#> 5   x ~~   x       0.078 0.016 5.000  0.000    0.048    0.109
#> 6  ab := a*b    ab 0.125 0.115 1.083  0.279   -0.101    0.350

# Group by DVs
group_by_dvs(fit)
#>   iv est_m est_y
#> m  m    -- 0.219
#> x  x 0.569    --

# Group by IVs
group_by_ivs(fit)
#>   dv est_m est_x
#> m  m    -- 0.569
#> y  y 0.219    --
```
