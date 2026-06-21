# Group Estimates By Models

Groups parameter estimates or other information such as *p*-values into
a table with models as columns.

## Usage

``` r
group_by_models(
  object_list,
  ...,
  col_names = "est",
  group_first = FALSE,
  model_first = TRUE,
  use_standardizedSolution = FALSE
)
```

## Arguments

- object_list:

  A named list of 'lavaan'-class objects, a named list of the output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html),
  or a named list of the output of
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).

- ...:

  Optional arguments to be passed to
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  Ignored if the elements in object_list are the results of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  or
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).

- col_names:

  A vector of the column names in the parameter estimate tables to be
  included. Default is `"est"`, or `"est.std"` if
  `use_standardizedSolution` is `TRUE`.

- group_first:

  If `TRUE`, the rows will be grouped by groups first and then by
  parameters. Ignored if the model has only one group. Default is
  `FALSE`.

- model_first:

  If `TRUE`, the columns will be grouped by models first and then by
  columns in the parameter estimates tables. Default is `TRUE`.

- use_standardizedSolution:

  If `TRUE` and `object_list` is not a list of estimates tables, then
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
  will be used to generate the table. If `FALSE`, the default, then
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  will be used if necessary.

## Value

A data-frame-like object of the class `est_table`.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448> Inspired by the
proposal Rönkkö posted in a GitHub
<https://github.com/simsem/semTools/issues/24#issue-235172313> for
`semTools`. I want something simple for a quick overview and so I wrote
this function.

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
group_by_models(list(no_direct = fit1,
                     direct = fit2),
                col_names = c("est", "pvalue"))
#>   lhs op rhs est_no_direct est_direct pvalue_no_direct pvalue_direct
#> 1   m  ~   x         0.569      0.569            0.097         0.097
#> 2   y  ~   m         0.219      0.214            0.153         0.174
#> 3   y  ~   x            NA      0.056               NA         0.887
#> 4   m ~~   m         0.460      0.460            0.000         0.000
#> 5   x ~~   x         0.078      0.078            0.000         0.000
#> 6   y ~~   y         0.570      0.570            0.000         0.000
#> 7  ab := a*b         0.125      0.122            0.279         0.293
# Can also be used with some other functions in
# semhelpinghands
group_by_models(list(no_direct = fit1,
                     direct = fit2),
                col_names = c("est", "pvalue")) |>
                filter_by(op = "~")
#>   lhs op rhs est_no_direct est_direct pvalue_no_direct pvalue_direct
#> 1   m  ~   x         0.569      0.569            0.097         0.097
#> 2   y  ~   m         0.219      0.214            0.153         0.174
#> 3   y  ~   x            NA      0.056               NA         0.887

```
