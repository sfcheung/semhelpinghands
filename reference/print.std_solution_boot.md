# Print an 'std_solution_boot' Object

Print method for an 'std_solution_boot' object, which is the output of
[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md).

## Usage

``` r
# S3 method for class 'std_solution_boot'
print(x, ..., nd = 3, output = c("table", "text"), standardized_only = TRUE)
```

## Arguments

- x:

  Object of the class `std_solution_boot`.

- ...:

  Optional arguments to be passed to
  [`print()`](https://rdrr.io/r/base/print.html) methods.

- nd:

  The number of digits after the decimal place. Default is 3.

- output:

  String. How the results are printed. Default is `"table"` and the
  results are printed in a table format similar to that of
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).
  If `"text"`, the results will be printed in a text format similar to
  the printout of the output of
  [`summary()`](https://rdrr.io/r/base/summary.html) of a 'lavaan'-class
  object.

- standardized_only:

  Logical. If `TRUE`, the default, only the results for the standardized
  solution will be printed. If `FALSE`, then the standardized solution
  is printed alongside the unstandardized solution, as in the printout
  of the output of [`summary()`](https://rdrr.io/r/base/summary.html) of
  a 'lavaan'-class object.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

The default format of the printout is that of
[`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html),
which is compact but not easy to read. Users can request a format
similar to that of the printout of the summary of a `lavaan` output by
setting `output` to `"text"`.

For the `"text"` format, users can also select whether only the
standardized solution is printed (the default) or whether the
standardized solution is appended to the right of the printout.

## See also

[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)

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

# Should set bootstrap to at least 2000 in real studies
fit <- sem(model, data = dat, fixed.x = FALSE,
           se = "boot",
           bootstrap = 50)
std_out <- standardizedSolution_boot_ci(fit)
std_out
#>   lhs op rhs label est.std    se      z pvalue ci.lower ci.upper boot.ci.lower
#> 1   m  ~   x     a   0.229 0.117  1.955  0.051   -0.001    0.458        -0.025
#> 2   y  ~   m     b   0.198 0.121  1.644  0.100   -0.038    0.434        -0.005
#> 3   m ~~   m         0.948 0.053 17.729  0.000    0.843    1.053         0.786
#> 4   y ~~   y         0.961 0.048 20.110  0.000    0.867    1.054         0.758
#> 5   x ~~   x         1.000 0.000     NA     NA    1.000    1.000            NA
#> 6  ab := a*b    ab   0.045 0.037  1.240  0.215   -0.026    0.117        -0.006
#>   boot.ci.upper boot.se
#> 1         0.462   0.121
#> 2         0.492   0.112
#> 3         1.000   0.055
#> 4         0.999   0.056
#> 5            NA      NA
#> 6         0.151   0.038
print(std_out, output = "text")
#> 
#> Standardized Estimates Only
#> 
#>   Standard errors                            Bootstrap
#>   Confidence interval                        Bootstrap
#>   Confidence Level                               95.0%
#>   Standardization Type                         std.all
#>   Number of requested bootstrap draws               50
#>   Number of successful bootstrap draws              50
#> 
#> Regressions:
#>                Standardized  Std.Err ci.lower ci.upper
#>   m ~                                                 
#>     x          (a)    0.229    0.121   -0.025    0.462
#>   y ~                                                 
#>     m          (b)    0.198    0.112   -0.005    0.492
#> 
#> Variances:
#>                Standardized  Std.Err ci.lower ci.upper
#>    .m                 0.948    0.055    0.786    1.000
#>    .y                 0.961    0.056    0.758    0.999
#>     x                 1.000       NA       NA       NA
#> 
#> Defined Parameters:
#>                Standardized  Std.Err ci.lower ci.upper
#>     ab                0.045    0.038   -0.006    0.151
#> 
print(std_out, output = "text", standardized_only = FALSE)
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                            Bootstrap
#>   Number of requested bootstrap draws               50
#>   Number of successful bootstrap draws              50
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>   m ~                                                                   
#>     x          (a)    0.569    0.293    1.942    0.052   -0.056    1.165
#>   y ~                                                                   
#>     m          (b)    0.219    0.147    1.490    0.136   -0.002    0.725
#>  Standardized ci.std.lower ci.std.upper Std.Err.std
#>                                                    
#>     0.229       -0.025        0.462        0.121   
#>                                                    
#>     0.198       -0.005        0.492        0.112   
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>    .m                 0.460    0.083    5.556    0.000    0.247    0.593
#>    .y                 0.570    0.109    5.229    0.000    0.341    0.792
#>     x                 0.078    0.013    5.924    0.000    0.052    0.102
#>  Standardized ci.std.lower ci.std.upper Std.Err.std
#>     0.948        0.786        1.000        0.055   
#>     0.961        0.758        0.999        0.056   
#>     1.000           NA           NA           NA   
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>     ab                0.125    0.106    1.171    0.241   -0.019    0.440
#>  Standardized ci.std.lower ci.std.upper Std.Err.std
#>     0.045       -0.006        0.151        0.038   
#> 
```
