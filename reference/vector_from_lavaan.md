# Wrapper Functions to Extract Information as a Vector

A set of wrapper functions to extract information from a `lavaan`-class
object and return a named vector.

## Usage

``` r
vec_rsquare(object)

vec_sample_vcov(object)

vec_sample_var(object)

vec_est_var(object)

vec_est_se(object)

vec_def_var(object)

vec_def_se(object)

vec_lavTestLRT(
  object,
  ...,
  method = "default",
  A.method = "delta",
  scaled.shifted = TRUE,
  H1 = TRUE,
  model.names = NULL
)

vec_lavTestScore(
  object,
  add = NULL,
  release = NULL,
  univariate = TRUE,
  information = "expected"
)

vec_lavTestWald(object, constraints = NULL, prefix = NULL)

vec_compRelSEM(object, ...)
```

## Arguments

- object:

  A `lavaan`-class object.

- ...:

  Additional arguments to be passed to the original function.

- method:

  An argument to be passed to
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).
  Please refer to the help page of
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).

- A.method:

  An argument to be passed
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).
  Please refer to the help page of
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).

- scaled.shifted:

  An argument to be passed to
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).
  Please refer to the help page of
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).

- H1:

  An argument to be passed to
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).
  Please refer to the help page of
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).

- model.names:

  An argument to be passed to
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).
  Please refer to the help page of
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).
  Unlike
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html),
  this argument is required, for the sake of naming the vector to be
  returned.

- add:

  An argument to be passed to
  [`lavaan::lavTestScore()`](https://rdrr.io/pkg/lavaan/man/lavTestScore.html).
  Please refer to the help page of
  [`lavaan::lavTestScore()`](https://rdrr.io/pkg/lavaan/man/lavTestScore.html).

- release:

  An argument to be passed to
  [`lavaan::lavTestScore()`](https://rdrr.io/pkg/lavaan/man/lavTestScore.html).
  Please refer to the help page of
  [`lavaan::lavTestScore()`](https://rdrr.io/pkg/lavaan/man/lavTestScore.html).

- univariate:

  An argument to be passed to
  [`lavaan::lavTestScore()`](https://rdrr.io/pkg/lavaan/man/lavTestScore.html).
  Please refer to the help page of
  [`lavaan::lavTestScore()`](https://rdrr.io/pkg/lavaan/man/lavTestScore.html).

- information:

  An argument to be passed to
  [`lavaan::lavTestScore()`](https://rdrr.io/pkg/lavaan/man/lavTestScore.html).
  Please refer to the help page of
  [`lavaan::lavTestScore()`](https://rdrr.io/pkg/lavaan/man/lavTestScore.html).

- constraints:

  An argument to be passed to
  [`lavaan::lavTestWald()`](https://rdrr.io/pkg/lavaan/man/lavTestWald.html).
  Please refer to the help page of
  [`lavaan::lavTestWald()`](https://rdrr.io/pkg/lavaan/man/lavTestWald.html).

- prefix:

  Optional. A character string to be added as a prefix to names of the
  output. Default is `NULL`.

## Value

All of them return a named numeric vector.

## Details

This set of wrapper functions are for functions like
[`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html)
that require users to supply a function that receives a `lavaan`-class
object and returns a vector of values.

All wrappers functions are designed to have the same form of output: a
named numeric vector.

Many of the tasks of this set of wrappers can be performed by writing
our own functions. The wrapper functions are developed just to save the
coding time for some commonly requested information.

The wrapper functions are designed to be as simple to use as possible,
with as few arguments as possible. If advanced control is needed, users
are recommended to write their own wrappers.

## Functions

- `vec_rsquare()`: Get R-squares in a model.

- `vec_sample_vcov()`: Get sample variances and covariances.

- `vec_sample_var()`: Get sample variances.

- `vec_est_var()`: Sampling variances of free parameters.

- `vec_est_se()`: Standard errors of free parameters.

- `vec_def_var()`: Sampling variances of user-defined parameters.

- `vec_def_se()`: Standard errors of user-defined parameters.

- `vec_lavTestLRT()`: Get sample variances.

- `vec_lavTestScore()`: Do score tests.

- `vec_lavTestWald()`: Do a Wald test.

- `vec_compRelSEM()`: Composite reliability.

## See also

[`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

# From the help page of lavaan::cfa().

library(lavaan)
HS.model <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'
fit <- cfa(HS.model, data = HolzingerSwineford1939)

vec_rsquare(fit)
#>    x1_rsq    x2_rsq    x3_rsq    x4_rsq    x5_rsq    x6_rsq    x7_rsq    x8_rsq 
#> 0.5957994 0.1794378 0.3377148 0.7251923 0.7311369 0.7022609 0.3243470 0.5227933 
#>    x9_rsq 
#> 0.4422373 
vec_sample_vcov(fit)
#>      x1~~x1      x1~~x2      x1~~x3      x1~~x4      x1~~x5      x1~~x6 
#>  1.35836985  0.40737133  0.57989932  0.50483500  0.44061554  0.45480813 
#>      x1~~x7      x1~~x8      x1~~x9      x2~~x2      x2~~x3      x2~~x4 
#>  0.08476543  0.26383768  0.45833855  1.38178387  0.45106394  0.20892337 
#>      x2~~x5      x2~~x6      x2~~x7      x2~~x8      x2~~x9      x3~~x3 
#>  0.21109108  0.24754457 -0.09675102  0.10965939  0.24400945  1.27486486 
#>      x3~~x4      x3~~x5      x3~~x6      x3~~x7      x3~~x8      x3~~x9 
#>  0.20816961  0.11229629  0.24410898  0.08834184  0.21232264  0.37385257 
#>      x4~~x4      x4~~x5      x4~~x6      x4~~x7      x4~~x8      x4~~x9 
#>  1.35066451  1.09775278  0.89551571  0.21974259  0.12559335  0.24336274 
#>      x5~~x5      x5~~x6      x5~~x7      x5~~x8      x5~~x9      x6~~x6 
#>  1.65978576  1.01452404  0.14299955  0.18060521  0.29524133  1.19635838 
#>      x6~~x7      x6~~x8      x6~~x9      x7~~x7      x7~~x8      x7~~x9 
#>  0.14407839  0.16543100  0.23599695  1.18313946  0.53524522  0.37329722 
#>      x8~~x8      x8~~x9      x9~~x9 
#>  1.02198281  0.45731656  1.01500387 
vec_sample_var(fit)
#>   x1_var   x2_var   x3_var   x4_var   x5_var   x6_var   x7_var   x8_var 
#> 1.358370 1.381784 1.274865 1.350665 1.659786 1.196358 1.183139 1.021983 
#>   x9_var 
#> 1.015004 
vec_est_var(fit)
#>       visual=~x2       visual=~x3      textual=~x5      textual=~x6 
#>      0.009933136      0.011904927      0.004279791      0.003074576 
#>        speed=~x8        speed=~x9           x1~~x1           x2~~x2 
#>      0.027220569      0.022851594      0.012905170      0.010347644 
#>           x3~~x3           x4~~x4           x5~~x5           x6~~x6 
#>      0.008212561      0.002276988      0.003409716      0.001852008 
#>           x7~~x7           x8~~x8           x9~~x9   visual~~visual 
#>      0.006622958      0.005504763      0.005003714      0.021159312 
#> textual~~textual     speed~~speed  visual~~textual    visual~~speed 
#>      0.012567721      0.007432025      0.005405761      0.003167033 
#>   textual~~speed 
#>      0.002431936 
vec_est_se(fit)
#>       visual=~x2       visual=~x3      textual=~x5      textual=~x6 
#>       0.09966512       0.10910970       0.06542011       0.05544886 
#>        speed=~x8        speed=~x9           x1~~x1           x2~~x2 
#>       0.16498657       0.15116744       0.11360092       0.10172337 
#>           x3~~x3           x4~~x4           x5~~x5           x6~~x6 
#>       0.09062318       0.04771779       0.05839278       0.04303497 
#>           x7~~x7           x8~~x8           x9~~x9   visual~~visual 
#>       0.08138156       0.07419409       0.07073694       0.14546241 
#> textual~~textual     speed~~speed  visual~~textual    visual~~speed 
#>       0.11210585       0.08620919       0.07352388       0.05627640 
#>   textual~~speed 
#>       0.04931466 

HS.model.sem1 <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
textual ~ a * visual
speed ~ b * textual
ab := a * b
'
fit_sem1 <- sem(HS.model.sem1, data = HolzingerSwineford1939)

HS.model.sem2 <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
textual ~ a * visual
speed ~ b * textual + cp * visual
ab := a * b
'
fit_sem2 <- sem(HS.model.sem2, data = HolzingerSwineford1939)

vec_def_var(fit_sem1)
#>           ab 
#> 0.0009906484 
vec_def_se(fit_sem1)
#>         ab 
#> 0.03147457 

vec_lavTestLRT(fit_sem1, fit_sem2,
               model.names = c("No Direct", "Direct"))
#> chisq_diff.Direct_vs_No Direct    df_diff.Direct_vs_No Direct 
#>                   1.810531e+01                   1.000000e+00 
#>    chisq_p.Direct_vs_No Direct 
#>                   2.090176e-05 

vec_lavTestScore(fit_sem1,
                 add = "speed ~ visual")
#>        score_chisq           score_df            score_p speed~visual.chisq 
#>       1.584567e+01       1.000000e+00       6.872365e-05       1.584567e+01 
#>    speed~visual.df     speed~visual.p 
#>       1.000000e+00       6.872365e-05 

vec_lavTestWald(fit_sem2,
                constraints = "cp == 0")
#>    wald_stat      wald_df       wald_p 
#> 1.444631e+01 1.000000e+00 1.442117e-04 

if (requireNamespace("semTools")) {
    vec_compRelSEM(fit)
  }
#>  visual_rel textual_rel   speed_rel 
#>       0.612       0.885       0.686 

```
