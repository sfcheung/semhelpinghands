# Show Major Options in an Output of 'lavaan'

Display the values of major options in a model fitted by
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or its
wrappers (e.g.,
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) or
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)).

## Usage

``` r
show_options(fit)

# S3 method for class 'show_options'
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

  The output of `show_options()`.

- ...:

  Additional arguments. Ignored.

## Value

A `show_options`-class object with a print method that formats the
output.

## Details

It extracts the values of major options in the output of
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or its
wrappers (e.g.,
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) or
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

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

## Methods (by generic)

- `print(show_options)`: The print method of the output of
  `show_options()`.

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

tmp <- show_options(fit)
tmp
#>  Options                             Call    Actual  
#>  Estimator(s)                        default ML      
#>  Standard Error                      default standard
#>  Model Test Statistic(s)             default standard
#>  Missing Data                        default listwise
#>  Information Matrix (for SE)         default expected
#>  Information Matrix (for Model Test) default expected
#>  Mean Structure                      default No      

fit <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "MLR")
show_options(fit)
#>  Options                             Call    Actual            
#>  Estimator(s)                        MLR     ML                
#>  Standard Error                      default robust.huber.white
#>  Model Test Statistic(s)             default yuan.bentler.mplus
#>  Missing Data                        default listwise          
#>  Information Matrix (for SE)         default observed          
#>  Information Matrix (for Model Test) default observed          
#>  Mean Structure                      default No                
fit <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "MLM")
show_options(fit)
#>  Options                             Call    Actual         
#>  Estimator(s)                        MLM     ML             
#>  Standard Error                      default robust.sem     
#>  Model Test Statistic(s)             default satorra.bentler
#>  Missing Data                        default listwise       
#>  Information Matrix (for SE)         default expected       
#>  Information Matrix (for Model Test) default expected       
#>  Mean Structure                      default No             
```
