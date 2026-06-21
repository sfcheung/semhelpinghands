# Print a 'fit_by_models' Object

Print method for a 'fit_by_models' object

## Usage

``` r
# S3 method for class 'fit_by_models'
print(
  x,
  ...,
  nd = 3,
  type = c("compact"),
  remove_all_na = TRUE,
  measures_compact = c("npar", "chisq", "chisq.scaled", "df", "df.scaled", "pvalue",
    "pvalue.scaled", "chisq.scaling.factor", "cfi", "cfi.robust", "tli", "tli.robust",
    "aic", "bic", "bic2", "rmsea", "rmsea.ci.level", "rmsea.ci.lower", "rmsea.ci.upper",
    "rmsea.close.h0", "rmsea.pvalue", "rmsea.robust", "rmsea.ci.lower.robust",
    "rmsea.ci.upper.robust", "rmsea.pvalue.robust", "srmr", "srmr_nomean")
)
```

## Arguments

- x:

  Object of the class `fit_by_models`.

- ...:

  Optional arguments to be passed to
  [`print()`](https://rdrr.io/r/base/print.html) methods.

- nd:

  The number of digits to be printed. Default is 3. (Scientific notation
  will never be used.)

- type:

  String. The type of the output. Currently only supports one type,
  `"compact"`.

- remove_all_na:

  Logical. Whether rows with `NA` in all columns will be removed.
  Default is `TRUE`.

- measures_compact:

  If output `type` is `"compact"`, the character vector of fit measures
  to be printed. The names should be the names of the output of
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html),
  in vector form.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

This function is intended to print the fit measures of one or more
groups in a simple and compact table for quick preview. For a
well-organized layout, call
[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
and set `output` to `"text"`. For comparing the models with notations on
models with the best fit on each measures, use
[`semTools::compareFit()`](https://rdrr.io/pkg/semTools/man/compareFit.html).

## See also

[`fitMeasures_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/fitMeasures_by_models.md)

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

out <- fitMeasures_by_models(list(no_direct = fit1,
                                  direct = fit2))
out
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

print(out, nd = 4, measures_compact = c("chisq", "cfi", "rmsea"))
#>                no_direct direct
#> Test statistic    0.0202      0
#> CFI               1.0000      1
#> RMSEA             0.0000      0
```
