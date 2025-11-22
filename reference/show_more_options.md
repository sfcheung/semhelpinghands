# Show More Major Options in an Output of 'lavaan'

Display the values of more major options in a model fitted by
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or its
wrappers (e.g., [lavaan::sem](https://rdrr.io/pkg/lavaan/man/sem.html)
or [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)).

## Usage

``` r
show_more_options(fit)

# S3 method for class 'show_more_options'
print(x, ...)
```

## Arguments

- fit:

  An output of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or
  its wrappers (e.g.,
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html))

- x:

  The output of `show_more_options()`.

- ...:

  Additional arguments. Ignored.

## Value

A `show_more_options`-class object with a print method that formats the
output.

## Details

It extracts the values of major options in the output of
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or its
wrappers (e.g., [lavaan::sem](https://rdrr.io/pkg/lavaan/man/sem.html)
or [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html). Most of
the values are also reported in the summary of a 'lavaan'-class object.
This function is used to show the values in one single table for a quick
overview.

It checks the actual values, not the call used. This is useful for
understanding how a prepackaged estimator such as `ML`, `MLM`, and `MLR`
set other options. It supports the following options:

- Estimator (`estimator`)

- Standard error (`se`)

- Model chi-square test(s) (`test`)

- Missing data method (`missing`)

- Information matrix used for computing standard errors (`information`)

- Information matrix used for computing model chi-square (`information`)

- Whether the mean structure is included.

It is named `show_more_options()` to differentiate it from
[`show_options()`](https://sfcheung.github.io/semhelpinghands/reference/show_options.md),
originally in the `semunpack` package, which is intended for new users
of `lavaan`. The code is adapted from `show_options` with more advanced
options added.

## Methods (by generic)

- `print(show_more_options)`: The print method of the output of
  `show_more_options()`.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
library(lavaan)

# From the help page of lavaan::cfa().

HS.model <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'
fit <- cfa(HS.model, data = HolzingerSwineford1939)

tmp <- show_more_options(fit)
tmp
#>  Options                             Call    Actual  
#>  Estimator(s)                        default ML      
#>  Standard Error (SE)                 default standard
#>  Model Test Statistic(s)             default standard
#>  How Missing Data is Handled         default listwise
#>  Information Matrix (for SE)         default expected
#>  Information Matrix (for Model Test) default expected
#>  Mean Structure                      default No      
#>  'x' Fixed                           default FALSE   

fit <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "MLR")
show_more_options(fit)
#>  Options                             Call    Actual            
#>  Estimator(s)                        MLR     ML                
#>  Standard Error (SE)                 default robust.huber.white
#>  Model Test Statistic(s)             default yuan.bentler.mplus
#>  How Missing Data is Handled         default listwise          
#>  Information Matrix (for SE)         default observed          
#>  Information Matrix (for Model Test) default observed          
#>  Mean Structure                      default No                
#>  'x' Fixed                           default FALSE             
fit <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "MLM")
show_more_options(fit)
#>  Options                             Call    Actual         
#>  Estimator(s)                        MLM     ML             
#>  Standard Error (SE)                 default robust.sem     
#>  Model Test Statistic(s)             default satorra.bentler
#>  How Missing Data is Handled         default listwise       
#>  Information Matrix (for SE)         default expected       
#>  Information Matrix (for Model Test) default expected       
#>  Mean Structure                      default No             
#>  'x' Fixed                           default FALSE          
```
