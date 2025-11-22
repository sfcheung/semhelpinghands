# Record the Minimization History

Record the minimization history when a model is fitted by
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or its
wrappers (e.g.,
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) or
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)).

## Usage

``` r
record_history(object)

# S3 method for class 'fit_history'
plot(x, params, last_n = -1, orientation = c("horizontal", "vertical"), ...)

# S3 method for class 'fit_history'
print(x, n_iterations = 10, digits = 3, ...)
```

## Arguments

- object:

  A 'lavaan'-class object.

- x:

  A `fit_history` class object, the output of `record_history()`.

- params:

  A character vector of the names of parameters to be plotted. Must be
  the names of one or more columns in `x`.

- last_n:

  The lass `n` iterations to be plotted. Default is -1, plotting all
  iterations.

- orientation:

  The orientation of the plot. Either `"horizontal"` (the default) or
  `"vertical"`.

- ...:

  Optional arguments. To be passed to the print method of data frame.

- n_iterations:

  The number of iterations to print. Default is 10, printing the first
  10 iterations (or all iterations, if the number of iterations is less
  than 10).

- digits:

  The number of digits to be displayed. Default is 3.

## Value

A `fit_history`-class object with a `plot` method
(`plot.fit_history()`).

## Details

It records the minimization history when a model is fitted by
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or its
wrappers (e.g.,
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) or
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)). The
recorded history can then be plotted or displayed, for visualizing how
the estimates of free parameters is found.

It will refit the model by the `update` method of
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html),
setting `se = "none"` and `test = "standard"` because they have no
impact on the minimization process.

This and related functions are adapted from the package `semunpack`. The
version in this package will be revised to be an advanced version
intended for diagnostic purpose in real studies.

## Functions

- `plot(fit_history)`: The plot method for the output of
  `record_history()`.

- `print(fit_history)`: The print method for the output of
  `record_history()`.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
# Adapted from the example for CFA in lavaan::cfa().
# Using only two of the factors
library(lavaan)
HS.model <-
'
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
'
fit <- cfa(HS.model, data = HolzingerSwineford1939)

# Refit the model with the history recorded
fit_h <- record_history(fit)
fit_h
#> Original call:
#> lavaan::lavaan(model = HS.model, data = HolzingerSwineford1939, 
#>     model.type = "cfa", int.ov.free = TRUE, int.lv.free = FALSE, 
#>     auto.fix.first = TRUE, auto.fix.single = TRUE, auto.var = TRUE, 
#>     auto.cov.lv.x = TRUE, auto.cov.y = TRUE, auto.th = TRUE, 
#>     auto.delta = TRUE, auto.efa = TRUE)
#> The number of iterations: 29 
#> The minimization history for the first 10 iterations:
#>    iteration     f visual=~x2 visual=~x3 textual=~x5 textual=~x6 x1~~x1 x2~~x2
#> 1          0 1.483      0.778      1.107       1.133       0.924  0.679  0.691
#> 2          1 0.349      0.791      1.126       1.161       0.950  0.746  0.767
#> 3          2 0.319      0.821      1.061       1.156       0.986  0.781  0.943
#> 4          3 0.184      0.791      0.943       1.115       0.940  0.799  0.995
#> 5          4 0.171      0.707      0.787       1.008       0.970  0.790  1.055
#> 6          5 0.061      0.668      0.768       1.115       0.876  0.767  1.062
#> 7          6 0.046      0.660      0.867       1.143       0.927  0.713  1.061
#> 8          7 0.045      0.653      0.853       1.125       0.943  0.668  1.075
#> 9          8 0.043      0.663      0.843       1.118       0.952  0.655  1.077
#> 10         9 0.042      0.660      0.830       1.128       0.930  0.651  1.081
#>    x3~~x3 x4~~x4 x5~~x5 x6~~x6 visual~~visual textual~~textual visual~~textual
#> 1   0.637  0.675  0.830  0.598          0.050            0.050           0.000
#> 2   0.702  0.727  0.871  0.658          0.481            0.781           0.505
#> 3   0.799  0.599  0.783  0.504          0.916            1.082          -0.285
#> 4   0.752  0.408  0.600  0.298          0.901            1.072          -0.162
#> 5   0.682  0.233  0.298  0.311          0.803            1.008           0.143
#> 6   0.674  0.415  0.439  0.410          0.717            0.944           0.307
#> 7   0.768  0.389  0.464  0.368          0.700            0.938           0.335
#> 8   0.788  0.387  0.451  0.348          0.662            0.931           0.393
#> 9   0.797  0.381  0.455  0.343          0.675            0.952           0.350
#> 10  0.801  0.372  0.447  0.352          0.680            0.951           0.351

# Plot the history for selected parameters
plot(fit_h, params = c("visual=~x2", "visual=~x3",
                       "visual~~textual"),
            last_n = 10)

plot(fit_h, params = c("visual=~x2", "visual=~x3",
                       "visual~~textual"),
            last_n = 10,
            orientation = "vertical")

```
