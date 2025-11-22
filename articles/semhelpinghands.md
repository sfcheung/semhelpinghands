# semhelpinghands

## Introduction

At the time of writing,
[`semhelpinghands`](https://sfcheung.github.io/semhelpinghands/) has
these groups of functions: manipulating parameter estimates tables,
comparing results from different methods, bootstrapping, and … others.

This article will only introduce very briefly the groups. Some of them
will be introduced in details in forthcoming article dedicated to them.

Let’s load the package first, and also load `lavaan`.

``` r
library(semhelpinghands)
#> 
#> Attaching package: 'semhelpinghands'
#> The following object is masked from 'package:base':
#> 
#>     sort_by
library(lavaan)
#> This is lavaan 0.6-20
#> lavaan is FREE software! Please report any bugs.
```

## Manipulate Parameter Estimates Tables

In using `lavaan`, I prefer reading the output of
[`parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html),
which is compact and clear to me. I sometimes would like to organize the
rows and columns in ways meaningful to a particular research questions.
This can certainly be done using base R or `dplyr`. However, I am lazy
and want to be able to do things using just one or two functions, with
just one or two arguments.

This is a sample dataset for illustration, `dvs_ivs`, with 3 predictors
(`x1`, `x2`, and `x3`), 3 outcome variables (`y1` `y2`, and `y3`), and a
group variable (`gp`).

First a single sample model:

``` r
data(dvs_ivs)
mod <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit <- sem(model = mod,
           data = dvs_ivs,
           fixed.x = FALSE)
```

The parameter estimates tables:

``` r
est <- parameterEstimates(fit)
est
#>    lhs op rhs    est    se      z pvalue ci.lower ci.upper
#> 1   y1  ~  x1  0.206 0.087  2.354  0.019    0.034    0.377
#> 2   y1  ~  x2  0.381 0.093  4.100  0.000    0.199    0.563
#> 3   y1  ~  x3  0.162 0.088  1.852  0.064   -0.009    0.334
#> 4   y2  ~  x1  0.149 0.105  1.425  0.154   -0.056    0.354
#> 5   y2  ~  x3  0.230 0.105  2.187  0.029    0.024    0.437
#> 6   y3  ~  y2  0.044 0.106  0.420  0.675   -0.163    0.251
#> 7   y3  ~  x2  0.295 0.121  2.445  0.014    0.059    0.532
#> 8   y1 ~~  y1  0.695 0.098  7.071  0.000    0.502    0.888
#> 9   y2 ~~  y2  1.013 0.143  7.071  0.000    0.732    1.294
#> 10  y3 ~~  y3  1.206 0.171  7.071  0.000    0.872    1.540
#> 11  y1 ~~  y3 -0.027 0.092 -0.292  0.770   -0.206    0.153
#> 12  x1 ~~  x1  0.926 0.131  7.071  0.000    0.669    1.183
#> 13  x1 ~~  x2  0.118 0.088  1.339  0.180   -0.055    0.291
#> 14  x1 ~~  x3  0.000 0.092 -0.001  0.999   -0.180    0.180
#> 15  x2 ~~  x2  0.828 0.117  7.071  0.000    0.598    1.057
#> 16  x2 ~~  x3  0.073 0.087  0.840  0.401   -0.098    0.244
#> 17  x3 ~~  x3  0.912 0.129  7.071  0.000    0.659    1.165
```

A two-sample model is also fitted to the dataset:

``` r
fit_gp <- sem(model = mod,
              data = dvs_ivs,
              group = "gp",
              fixed.x = FALSE)
```

This is the parameter estimates table:

``` r
est_gp <- parameterEstimates(fit_gp)
est_gp
#>    lhs op rhs block group    est    se      z pvalue ci.lower ci.upper
#> 1   y1  ~  x1     1     1  0.210 0.149  1.404  0.160   -0.083    0.502
#> 2   y1  ~  x2     1     1  0.243 0.138  1.761  0.078   -0.027    0.513
#> 3   y1  ~  x3     1     1  0.246 0.144  1.710  0.087   -0.036    0.527
#> 4   y2  ~  x1     1     1  0.036 0.179  0.199  0.842   -0.315    0.386
#> 5   y2  ~  x3     1     1  0.217 0.176  1.232  0.218   -0.128    0.561
#> 6   y3  ~  y2     1     1  0.096 0.120  0.802  0.423   -0.139    0.332
#> 7   y3  ~  x2     1     1  0.372 0.135  2.754  0.006    0.107    0.637
#> 8   y1 ~~  y1     1     1  0.754 0.157  4.796  0.000    0.446    1.062
#> 9   y2 ~~  y2     1     1  1.148 0.239  4.796  0.000    0.679    1.617
#> 10  y3 ~~  y3     1     1  0.787 0.164  4.796  0.000    0.466    1.109
#> 11  y1 ~~  y3     1     1  0.010 0.114  0.085  0.932   -0.213    0.232
#> 12  x1 ~~  x1     1     1  0.798 0.166  4.796  0.000    0.472    1.125
#> 13  x1 ~~  x2     1     1  0.223 0.132  1.694  0.090   -0.035    0.481
#> 14  x1 ~~  x3     1     1  0.121 0.121  1.001  0.317   -0.116    0.358
#> 15  x2 ~~  x2     1     1  0.938 0.196  4.796  0.000    0.555    1.321
#> 16  x2 ~~  x3     1     1  0.139 0.131  1.061  0.289   -0.118    0.397
#> 17  x3 ~~  x3     1     1  0.825 0.172  4.796  0.000    0.488    1.162
#> 18  y1 ~1         1     1 -0.295 0.137 -2.148  0.032   -0.564   -0.026
#> 19  y2 ~1         1     1  0.027 0.168  0.163  0.870   -0.302    0.357
#> 20  y3 ~1         1     1  0.233 0.131  1.775  0.076   -0.024    0.490
#> 21  x1 ~1         1     1  0.282 0.132  2.141  0.032    0.024    0.540
#> 22  x2 ~1         1     1 -0.067 0.143 -0.469  0.639   -0.347    0.213
#> 23  x3 ~1         1     1 -0.123 0.134 -0.915  0.360   -0.385    0.140
#> 24  y1  ~  x1     2     2  0.258 0.102  2.530  0.011    0.058    0.458
#> 25  y1  ~  x2     2     2  0.484 0.121  3.998  0.000    0.247    0.721
#> 26  y1  ~  x3     2     2  0.121 0.104  1.165  0.244   -0.082    0.324
#> 27  y2  ~  x1     2     2  0.226 0.127  1.774  0.076   -0.024    0.475
#> 28  y2  ~  x3     2     2  0.261 0.129  2.016  0.044    0.007    0.514
#> 29  y3  ~  y2     2     2 -0.003 0.162 -0.020  0.984   -0.322    0.315
#> 30  y3  ~  x2     2     2  0.291 0.190  1.527  0.127   -0.082    0.664
#> 31  y1 ~~  y1     2     2  0.568 0.109  5.196  0.000    0.354    0.783
#> 32  y2 ~~  y2     2     2  0.882 0.170  5.196  0.000    0.549    1.214
#> 33  y3 ~~  y3     2     2  1.412 0.272  5.196  0.000    0.880    1.945
#> 34  y1 ~~  y3     2     2  0.050 0.122  0.410  0.682   -0.189    0.289
#> 35  x1 ~~  x1     2     2  1.020 0.196  5.196  0.000    0.635    1.405
#> 36  x1 ~~  x2     2     2  0.042 0.117  0.361  0.718   -0.187    0.271
#> 37  x1 ~~  x3     2     2 -0.099 0.137 -0.719  0.472   -0.367    0.170
#> 38  x2 ~~  x2     2     2  0.722 0.139  5.196  0.000    0.450    0.994
#> 39  x2 ~~  x3     2     2  0.013 0.115  0.110  0.912   -0.212    0.238
#> 40  x3 ~~  x3     2     2  0.985 0.190  5.196  0.000    0.613    1.356
#> 41  y1 ~1         2     2  0.007 0.104  0.068  0.946   -0.196    0.211
#> 42  y2 ~1         2     2 -0.050 0.129 -0.392  0.695   -0.303    0.202
#> 43  y3 ~1         2     2 -0.333 0.163 -2.042  0.041   -0.652   -0.013
#> 44  x1 ~1         2     2  0.101 0.137  0.738  0.460   -0.168    0.371
#> 45  x2 ~1         2     2  0.092 0.116  0.797  0.425   -0.134    0.319
#> 46  x3 ~1         2     2 -0.065 0.135 -0.482  0.630   -0.330    0.200
```

So, these are what `semhelpinghands` has for now:

### Add Significance Test Results: `add_sig()`

Despite the controversy over null hypothesis significance testing, we
still need to report them, for now. They are there, but I have to decode
them mentally. Here comes
[`add_sig()`](https://sfcheung.github.io/semhelpinghands/reference/add_sig.md):

``` r
add_sig(est)
#>    lhs op rhs    est sig    se      z pvalue ci.lower ci.upper
#> 1   y1  ~  x1  0.206 *   0.087  2.354  0.019    0.034    0.377
#> 2   y1  ~  x2  0.381 *** 0.093  4.100  0.000    0.199    0.563
#> 3   y1  ~  x3  0.162     0.088  1.852  0.064   -0.009    0.334
#> 4   y2  ~  x1  0.149     0.105  1.425  0.154   -0.056    0.354
#> 5   y2  ~  x3  0.230 *   0.105  2.187  0.029    0.024    0.437
#> 6   y3  ~  y2  0.044     0.106  0.420  0.675   -0.163    0.251
#> 7   y3  ~  x2  0.295 *   0.121  2.445  0.014    0.059    0.532
#> 8   y1 ~~  y1  0.695 *** 0.098  7.071  0.000    0.502    0.888
#> 9   y2 ~~  y2  1.013 *** 0.143  7.071  0.000    0.732    1.294
#> 10  y3 ~~  y3  1.206 *** 0.171  7.071  0.000    0.872    1.540
#> 11  y1 ~~  y3 -0.027     0.092 -0.292  0.770   -0.206    0.153
#> 12  x1 ~~  x1  0.926 *** 0.131  7.071  0.000    0.669    1.183
#> 13  x1 ~~  x2  0.118     0.088  1.339  0.180   -0.055    0.291
#> 14  x1 ~~  x3  0.000     0.092 -0.001  0.999   -0.180    0.180
#> 15  x2 ~~  x2  0.828 *** 0.117  7.071  0.000    0.598    1.057
#> 16  x2 ~~  x3  0.073     0.087  0.840  0.401   -0.098    0.244
#> 17  x3 ~~  x3  0.912 *** 0.129  7.071  0.000    0.659    1.165
```

If bootstrapping was used, it also supports using the confidence
intervals, which may yield results different from the *p*-values when
bootstrapping confidence intervals are used (but same results in this
example):

``` r
add_sig(est,
        use = c("pvalue", "ci"))
#>    lhs op rhs    est sig  ci    se      z pvalue ci.lower ci.upper
#> 1   y1  ~  x1  0.206 *   Sig 0.087  2.354  0.019    0.034    0.377
#> 2   y1  ~  x2  0.381 *** Sig 0.093  4.100  0.000    0.199    0.563
#> 3   y1  ~  x3  0.162         0.088  1.852  0.064   -0.009    0.334
#> 4   y2  ~  x1  0.149         0.105  1.425  0.154   -0.056    0.354
#> 5   y2  ~  x3  0.230 *   Sig 0.105  2.187  0.029    0.024    0.437
#> 6   y3  ~  y2  0.044         0.106  0.420  0.675   -0.163    0.251
#> 7   y3  ~  x2  0.295 *   Sig 0.121  2.445  0.014    0.059    0.532
#> 8   y1 ~~  y1  0.695 *** Sig 0.098  7.071  0.000    0.502    0.888
#> 9   y2 ~~  y2  1.013 *** Sig 0.143  7.071  0.000    0.732    1.294
#> 10  y3 ~~  y3  1.206 *** Sig 0.171  7.071  0.000    0.872    1.540
#> 11  y1 ~~  y3 -0.027         0.092 -0.292  0.770   -0.206    0.153
#> 12  x1 ~~  x1  0.926 *** Sig 0.131  7.071  0.000    0.669    1.183
#> 13  x1 ~~  x2  0.118         0.088  1.339  0.180   -0.055    0.291
#> 14  x1 ~~  x3  0.000         0.092 -0.001  0.999   -0.180    0.180
#> 15  x2 ~~  x2  0.828 *** Sig 0.117  7.071  0.000    0.598    1.057
#> 16  x2 ~~  x3  0.073         0.087  0.840  0.401   -0.098    0.244
#> 17  x3 ~~  x3  0.912 *** Sig 0.129  7.071  0.000    0.659    1.165
```

It also works on the standardized solution:

``` r
std <- standardizedSolution(fit)
add_sig(std)
#>    lhs op rhs est.std sig    se      z pvalue ci.lower ci.upper
#> 1   y1  ~  x1   0.208 *   0.087  2.394  0.017    0.038    0.378
#> 2   y1  ~  x2   0.364 *** 0.083  4.364  0.000    0.200    0.527
#> 3   y1  ~  x3   0.163     0.087  1.871  0.061   -0.008    0.333
#> 4   y2  ~  x1   0.138     0.096  1.438  0.150   -0.050    0.326
#> 5   y2  ~  x3   0.212 *   0.095  2.236  0.025    0.026    0.397
#> 6   y3  ~  y2   0.041     0.097  0.420  0.674   -0.149    0.231
#> 7   y3  ~  x2   0.237 *   0.094  2.517  0.012    0.053    0.422
#> 8   y1 ~~  y1   0.767 *** 0.074 10.369  0.000    0.622    0.912
#> 9   y2 ~~  y2   0.936 *** 0.047 19.799  0.000    0.844    1.029
#> 10  y3 ~~  y3   0.941 *** 0.046 20.649  0.000    0.852    1.031
#> 11  y1 ~~  y3  -0.029     0.100 -0.292  0.770   -0.225    0.167
#> 12  x1 ~~  x1   1.000     0.000     NA     NA    1.000    1.000
#> 13  x1 ~~  x2   0.135     0.098  1.377  0.169   -0.057    0.328
#> 14  x1 ~~  x3   0.000     0.100 -0.001  0.999   -0.196    0.196
#> 15  x2 ~~  x2   1.000     0.000     NA     NA    1.000    1.000
#> 16  x2 ~~  x3   0.084     0.099  0.849  0.396   -0.110    0.279
#> 17  x3 ~~  x3   1.000     0.000     NA     NA    1.000    1.000
```

Note: But be careful about the interpretation of the *p*-values in the
standardized solution, which are based on the delta method. See
[`vignette("standardizedSolution_boot_ci")`](https://sfcheung.github.io/semhelpinghands/articles/standardizedSolution_boot_ci.md).

See the help page of
[`add_sig()`](https://sfcheung.github.io/semhelpinghands/reference/add_sig.md)
for other options.

### Filter by Operators and Some Other Columns: `filter_by()`

Its purpose is very simple, selecting rows by commonly used columns:
operators (`op`), “dependent variables” (`lhs`), and “independent
variables” (`rhs`).

``` r
filter_by(est,
          op = "~")
#>   lhs op rhs   est    se     z pvalue ci.lower ci.upper
#> 1  y1  ~  x1 0.206 0.087 2.354  0.019    0.034    0.377
#> 2  y1  ~  x2 0.381 0.093 4.100  0.000    0.199    0.563
#> 3  y1  ~  x3 0.162 0.088 1.852  0.064   -0.009    0.334
#> 4  y2  ~  x1 0.149 0.105 1.425  0.154   -0.056    0.354
#> 5  y2  ~  x3 0.230 0.105 2.187  0.029    0.024    0.437
#> 6  y3  ~  y2 0.044 0.106 0.420  0.675   -0.163    0.251
#> 7  y3  ~  x2 0.295 0.121 2.445  0.014    0.059    0.532
```

It also supports filtering by group using group labels:

``` r
filter_by(est_gp,
          op = "~",
          group = "gp1",
          fit = fit_gp)
#>   lhs op rhs block group   est    se     z pvalue ci.lower ci.upper
#> 1  y1  ~  x1     1     1 0.210 0.149 1.404  0.160   -0.083    0.502
#> 2  y1  ~  x2     1     1 0.243 0.138 1.761  0.078   -0.027    0.513
#> 3  y1  ~  x3     1     1 0.246 0.144 1.710  0.087   -0.036    0.527
#> 4  y2  ~  x1     1     1 0.036 0.179 0.199  0.842   -0.315    0.386
#> 5  y2  ~  x3     1     1 0.217 0.176 1.232  0.218   -0.128    0.561
#> 6  y3  ~  y2     1     1 0.096 0.120 0.802  0.423   -0.139    0.332
#> 7  y3  ~  x2     1     1 0.372 0.135 2.754  0.006    0.107    0.637
```

See the help page of
[`filter_by()`](https://sfcheung.github.io/semhelpinghands/reference/filter_by.md)
for other options.

### Group By DVs or Group By IVs: `group_by_dvs()` and `group_by_ivs()`

Sometimes I want the conventional iv-column-dv-row or dv-column-iv-row
format. That’s what
[`group_by_dvs()`](https://sfcheung.github.io/semhelpinghands/reference/group_estimates.md)
and
[`group_by_ivs()`](https://sfcheung.github.io/semhelpinghands/reference/group_estimates.md)
are for:

``` r
group_by_dvs(est)
#>    iv est_y1 est_y2 est_y3
#> x1 x1  0.206  0.149     --
#> x2 x2  0.381     --  0.295
#> x3 x3  0.162  0.230     --
#> y2 y2     --     --  0.044
group_by_ivs(est)
#>    dv est_x1 est_x2 est_x3 est_y2
#> y1 y1  0.206  0.381  0.162     --
#> y2 y2  0.149     --  0.230     --
#> y3 y3     --  0.295     --  0.044
```

They also supports extracting another column:

``` r
group_by_dvs(est,
             col_name = "pvalue")
#>    iv pvalue_y1 pvalue_y2 pvalue_y3
#> x1 x1     0.019     0.154        --
#> x2 x2     0.000        --     0.014
#> x3 x3     0.064     0.029        --
#> y2 y2        --        --     0.675
group_by_ivs(est,
             col_name = "pvalue")
#>    dv pvalue_x1 pvalue_x2 pvalue_x3 pvalue_y2
#> y1 y1     0.019     0.000     0.064        --
#> y2 y2     0.154        --     0.029        --
#> y3 y3        --     0.014        --     0.675
```

See the help page of
[`group_by_dvs()`](https://sfcheung.github.io/semhelpinghands/reference/group_estimates.md)
and
[`group_by_ivs()`](https://sfcheung.github.io/semhelpinghands/reference/group_estimates.md)
for other options.

### Group By Groups: `group_by_groups()`

In multiple sample models, one common task is comparing results across
groups. I wrote
[`group_by_groups()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_groups.md)
for this task, to compare results side-by-side:

``` r
group_by_groups(est_gp)
#>    lhs op rhs  est_1  est_2
#> 1   y1  ~  x1  0.210  0.258
#> 2   y1  ~  x2  0.243  0.484
#> 3   y1  ~  x3  0.246  0.121
#> 4   y2  ~  x1  0.036  0.226
#> 5   y2  ~  x3  0.217  0.261
#> 6   y3  ~  x2  0.372  0.291
#> 7   y3  ~  y2  0.096 -0.003
#> 8   x1 ~~  x1  0.798  1.020
#> 9   x1 ~~  x2  0.223  0.042
#> 10  x1 ~~  x3  0.121 -0.099
#> 11  x2 ~~  x2  0.938  0.722
#> 12  x2 ~~  x3  0.139  0.013
#> 13  x3 ~~  x3  0.825  0.985
#> 14  y1 ~~  y1  0.754  0.568
#> 15  y1 ~~  y3  0.010  0.050
#> 16  y2 ~~  y2  1.148  0.882
#> 17  y3 ~~  y3  0.787  1.412
#> 18  x1 ~1      0.282  0.101
#> 19  x2 ~1     -0.067  0.092
#> 20  x3 ~1     -0.123 -0.065
#> 21  y1 ~1     -0.295  0.007
#> 22  y2 ~1      0.027 -0.050
#> 23  y3 ~1      0.233 -0.333
```

It also supports extracting several columns:

``` r
group_by_groups(est_gp,
                col_names = c("est", "pvalue"))
#>    lhs op rhs  est_1  est_2 pvalue_1 pvalue_2
#> 1   y1  ~  x1  0.210  0.258    0.160    0.011
#> 2   y1  ~  x2  0.243  0.484    0.078    0.000
#> 3   y1  ~  x3  0.246  0.121    0.087    0.244
#> 4   y2  ~  x1  0.036  0.226    0.842    0.076
#> 5   y2  ~  x3  0.217  0.261    0.218    0.044
#> 6   y3  ~  x2  0.372  0.291    0.006    0.127
#> 7   y3  ~  y2  0.096 -0.003    0.423    0.984
#> 8   x1 ~~  x1  0.798  1.020    0.000    0.000
#> 9   x1 ~~  x2  0.223  0.042    0.090    0.718
#> 10  x1 ~~  x3  0.121 -0.099    0.317    0.472
#> 11  x2 ~~  x2  0.938  0.722    0.000    0.000
#> 12  x2 ~~  x3  0.139  0.013    0.289    0.912
#> 13  x3 ~~  x3  0.825  0.985    0.000    0.000
#> 14  y1 ~~  y1  0.754  0.568    0.000    0.000
#> 15  y1 ~~  y3  0.010  0.050    0.932    0.682
#> 16  y2 ~~  y2  1.148  0.882    0.000    0.000
#> 17  y3 ~~  y3  0.787  1.412    0.000    0.000
#> 18  x1 ~1      0.282  0.101    0.032    0.460
#> 19  x2 ~1     -0.067  0.092    0.639    0.425
#> 20  x3 ~1     -0.123 -0.065    0.360    0.630
#> 21  y1 ~1     -0.295  0.007    0.032    0.946
#> 22  y2 ~1      0.027 -0.050    0.870    0.695
#> 23  y3 ~1      0.233 -0.333    0.076    0.041
```

If the fit object is used, it can print group labels:

``` r
group_by_groups(fit_gp,
                col_names = c("est", "pvalue"))
#>    lhs op rhs est_gp1 est_gp2 pvalue_gp1 pvalue_gp2
#> 1   y1  ~  x1   0.210   0.258      0.160      0.011
#> 2   y1  ~  x2   0.243   0.484      0.078      0.000
#> 3   y1  ~  x3   0.246   0.121      0.087      0.244
#> 4   y2  ~  x1   0.036   0.226      0.842      0.076
#> 5   y2  ~  x3   0.217   0.261      0.218      0.044
#> 6   y3  ~  x2   0.372   0.291      0.006      0.127
#> 7   y3  ~  y2   0.096  -0.003      0.423      0.984
#> 8   x1 ~~  x1   0.798   1.020      0.000      0.000
#> 9   x1 ~~  x2   0.223   0.042      0.090      0.718
#> 10  x1 ~~  x3   0.121  -0.099      0.317      0.472
#> 11  x2 ~~  x2   0.938   0.722      0.000      0.000
#> 12  x2 ~~  x3   0.139   0.013      0.289      0.912
#> 13  x3 ~~  x3   0.825   0.985      0.000      0.000
#> 14  y1 ~~  y1   0.754   0.568      0.000      0.000
#> 15  y1 ~~  y3   0.010   0.050      0.932      0.682
#> 16  y2 ~~  y2   1.148   0.882      0.000      0.000
#> 17  y3 ~~  y3   0.787   1.412      0.000      0.000
#> 18  x1 ~1       0.282   0.101      0.032      0.460
#> 19  x2 ~1      -0.067   0.092      0.639      0.425
#> 20  x3 ~1      -0.123  -0.065      0.360      0.630
#> 21  y1 ~1      -0.295   0.007      0.032      0.946
#> 22  y2 ~1       0.027  -0.050      0.870      0.695
#> 23  y3 ~1       0.233  -0.333      0.076      0.041
```

See the help page of
[`group_by_groups()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_groups.md)
for other options.

### Compare Models: `group_by_models()`

This is inspired by the proposal Rönkkö proposed in a GitHub
[issue](https://github.com/simsem/semTools/issues/24#issue-235172313)
for `semTools`. I want something simple for a quick overview and so I
wrote
[`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md).

Suppose this is the other model fitted:

``` r
mod2 <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x2
y3 ~ y2 + x1
"
fit2 <- sem(model = mod2,
            data = dvs_ivs,
            fixed.x = FALSE)
est2 <- parameterEstimates(fit2)
est2
#>    lhs op rhs    est    se      z pvalue ci.lower ci.upper
#> 1   y1  ~  x1  0.209 0.087  2.394  0.017    0.038    0.381
#> 2   y1  ~  x2  0.387 0.093  4.171  0.000    0.205    0.569
#> 3   y1  ~  x3  0.162 0.088  1.853  0.064   -0.009    0.334
#> 4   y2  ~  x1  0.176 0.106  1.655  0.098   -0.032    0.384
#> 5   y2  ~  x2 -0.209 0.112 -1.864  0.062   -0.430    0.011
#> 6   y3  ~  y2  0.023 0.109  0.210  0.834   -0.190    0.236
#> 7   y3  ~  x1 -0.157 0.117 -1.339  0.181   -0.388    0.073
#> 8   y1 ~~  y1  0.695 0.098  7.071  0.000    0.502    0.888
#> 9   y2 ~~  y2  1.026 0.145  7.071  0.000    0.741    1.310
#> 10  y3 ~~  y3  1.254 0.177  7.071  0.000    0.906    1.601
#> 11  y1 ~~  y3 -0.028 0.093 -0.299  0.765   -0.211    0.155
#> 12  x1 ~~  x1  0.926 0.131  7.071  0.000    0.669    1.183
#> 13  x1 ~~  x2  0.118 0.088  1.339  0.180   -0.055    0.291
#> 14  x1 ~~  x3  0.000 0.092 -0.001  0.999   -0.180    0.180
#> 15  x2 ~~  x2  0.828 0.117  7.071  0.000    0.598    1.057
#> 16  x2 ~~  x3  0.073 0.087  0.840  0.401   -0.098    0.244
#> 17  x3 ~~  x3  0.912 0.129  7.071  0.000    0.659    1.165
```

These two models have no nested relationships. To compare the estimates,
[`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md)
can be used:

``` r
group_by_models(list(Model1 = est,
                     Model2 = est2))
#>    lhs op rhs est_Model1 est_Model2
#> 1   y1  ~  x1      0.206      0.209
#> 2   y1  ~  x2      0.381      0.387
#> 3   y1  ~  x3      0.162      0.162
#> 4   y2  ~  x1      0.149      0.176
#> 5   y2  ~  x2         NA     -0.209
#> 6   y2  ~  x3      0.230         NA
#> 7   y3  ~  x1         NA     -0.157
#> 8   y3  ~  x2      0.295         NA
#> 9   y3  ~  y2      0.044      0.023
#> 10  x1 ~~  x1      0.926      0.926
#> 11  x1 ~~  x2      0.118      0.118
#> 12  x1 ~~  x3      0.000      0.000
#> 13  x2 ~~  x2      0.828      0.828
#> 14  x2 ~~  x3      0.073      0.073
#> 15  x3 ~~  x3      0.912      0.912
#> 16  y1 ~~  y1      0.695      0.695
#> 17  y1 ~~  y3     -0.027     -0.028
#> 18  y2 ~~  y2      1.013      1.026
#> 19  y3 ~~  y3      1.206      1.254
```

It can also compare several columns:

``` r
group_by_models(list(Model1 = est,
                     Model2 = est2),
                col_names = c("est", "pvalue"))
#>    lhs op rhs est_Model1 est_Model2 pvalue_Model1 pvalue_Model2
#> 1   y1  ~  x1      0.206      0.209         0.019         0.017
#> 2   y1  ~  x2      0.381      0.387         0.000         0.000
#> 3   y1  ~  x3      0.162      0.162         0.064         0.064
#> 4   y2  ~  x1      0.149      0.176         0.154         0.098
#> 5   y2  ~  x2         NA     -0.209            NA         0.062
#> 6   y2  ~  x3      0.230         NA         0.029            NA
#> 7   y3  ~  x1         NA     -0.157            NA         0.181
#> 8   y3  ~  x2      0.295         NA         0.014            NA
#> 9   y3  ~  y2      0.044      0.023         0.675         0.834
#> 10  x1 ~~  x1      0.926      0.926         0.000         0.000
#> 11  x1 ~~  x2      0.118      0.118         0.180         0.180
#> 12  x1 ~~  x3      0.000      0.000         0.999         0.999
#> 13  x2 ~~  x2      0.828      0.828         0.000         0.000
#> 14  x2 ~~  x3      0.073      0.073         0.401         0.401
#> 15  x3 ~~  x3      0.912      0.912         0.000         0.000
#> 16  y1 ~~  y1      0.695      0.695         0.000         0.000
#> 17  y1 ~~  y3     -0.027     -0.028         0.770         0.765
#> 18  y2 ~~  y2      1.013      1.026         0.000         0.000
#> 19  y3 ~~  y3      1.206      1.254         0.000         0.000
```

See the help page of
[`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md)
for other options.

### Sort Rows: `sort_by()`

The function
[`sort_by()`](https://sfcheung.github.io/semhelpinghands/reference/sort_by.md)
can be used to sort rows using (a) common fields such as `"op"`,
`"lhs"`, and `"rhs"`, (b) operators such as `"~"` and `"~~"`. It can be
used on the output of some other functions that manipulate a parameter
estimates table.

``` r
out <- group_by_groups(est_gp,
                       col_names = c("est", "pvalue"))
out <- filter_by(out,
                 op = c("~", "~~"))
sort_by(out,
        by = c("op", "rhs"))
#>    lhs op rhs est_1  est_2 pvalue_1 pvalue_2
#> 1   y1  ~  x1 0.210  0.258    0.160    0.011
#> 2   y2  ~  x1 0.036  0.226    0.842    0.076
#> 3   y1  ~  x2 0.243  0.484    0.078    0.000
#> 4   y3  ~  x2 0.372  0.291    0.006    0.127
#> 5   y1  ~  x3 0.246  0.121    0.087    0.244
#> 6   y2  ~  x3 0.217  0.261    0.218    0.044
#> 7   y3  ~  y2 0.096 -0.003    0.423    0.984
#> 8   x1 ~~  x1 0.798  1.020    0.000    0.000
#> 9   x1 ~~  x2 0.223  0.042    0.090    0.718
#> 10  x2 ~~  x2 0.938  0.722    0.000    0.000
#> 11  x1 ~~  x3 0.121 -0.099    0.317    0.472
#> 12  x2 ~~  x3 0.139  0.013    0.289    0.912
#> 13  x3 ~~  x3 0.825  0.985    0.000    0.000
#> 14  y1 ~~  y1 0.754  0.568    0.000    0.000
#> 15  y2 ~~  y2 1.148  0.882    0.000    0.000
#> 16  y1 ~~  y3 0.010  0.050    0.932    0.682
#> 17  y3 ~~  y3 0.787  1.412    0.000    0.000
```

Some functions, such as
[`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md),
will automatically call
[`sort_by()`](https://sfcheung.github.io/semhelpinghands/reference/sort_by.md)
to sort the results.

The default order should be acceptable in most cases, but can also be
customized. See the help page of
[`sort_by()`](https://sfcheung.github.io/semhelpinghands/reference/sort_by.md)
on customizing the order.

### Piping

Though not officially supported, piping using `|>` can be used with most
of the functions that manipulate parameter estimates tables. For
example:

``` r
est_gp |>
  add_sig() |>
  group_by_groups(col_names = c("est", "pvalue", "sig"),
                  group_first = FALSE) |>
  filter_by(op = c("~"))
#>   lhs op rhs est_1 pvalue_1 sig_1  est_2 pvalue_2 sig_2
#> 1  y1  ~  x1 0.210    0.160        0.258    0.011   *  
#> 2  y1  ~  x2 0.243    0.078        0.484    0.000   ***
#> 3  y1  ~  x3 0.246    0.087        0.121    0.244      
#> 4  y2  ~  x1 0.036    0.842        0.226    0.076      
#> 5  y2  ~  x3 0.217    0.218        0.261    0.044   *  
#> 6  y3  ~  x2 0.372    0.006   **   0.291    0.127      
#> 7  y3  ~  y2 0.096    0.423       -0.003    0.984
```

## Compare Methods

Though I believe the choice of the estimation method should be
justified, suppose we want to assess the sensitivity of the parameter
estimate results to the methods used,
[`compare_estimators()`](https://sfcheung.github.io/semhelpinghands/reference/compare_estimators.md)
can be used as a quick way to compare results from different methods.

``` r
out <- compare_estimators(fit,
         estimator = c("ML", "GLS", "MLR"))
group_by_models(out,
                col_names = c("se", "pvalue"))
#>    lhs op rhs se_ML se_GLS se_MLR pvalue_ML pvalue_GLS pvalue_MLR
#> 1   y1  ~  x1 0.087  0.094  0.067     0.019      0.028      0.002
#> 2   y1  ~  x2 0.093  0.100  0.091     0.000      0.000      0.000
#> 3   y1  ~  x3 0.088  0.089  0.084     0.064      0.067      0.053
#> 4   y2  ~  x1 0.105  0.112  0.107     0.154      0.148      0.165
#> 5   y2  ~  x3 0.105  0.107  0.123     0.029      0.028      0.061
#> 6   y3  ~  x2 0.121  0.133  0.111     0.014      0.019      0.008
#> 7   y3  ~  y2 0.106  0.116  0.108     0.675      0.654      0.681
#> 8   x1 ~~  x1 0.131  0.129  0.136     0.000      0.000      0.000
#> 9   x1 ~~  x2 0.088  0.090  0.077     0.180      0.202      0.122
#> 10  x1 ~~  x3 0.092  0.092  0.102     0.999      0.840      0.999
#> 11  x2 ~~  x2 0.117  0.114  0.127     0.000      0.000      0.000
#> 12  x2 ~~  x3 0.087  0.089  0.091     0.401      0.414      0.423
#> 13  x3 ~~  x3 0.129  0.131  0.111     0.000      0.000      0.000
#> 14  y1 ~~  y1 0.098  0.100  0.119     0.000      0.000      0.000
#> 15  y1 ~~  y3 0.092  0.093  0.104     0.770      0.793      0.797
#> 16  y2 ~~  y2 0.143  0.139  0.141     0.000      0.000      0.000
#> 17  y3 ~~  y3 0.171  0.167  0.171     0.000      0.000      0.000
```

It simply refits the models for each estimator and returns the results.
They can then be treated as different “models” and processed by
[`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md).

[`se_ratios()`](https://sfcheung.github.io/semhelpinghands/reference/compare_estimators.md)
is a wrapper of
[`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md)
used to compare the standard errors by different estimators in the
output of
[`compare_estimators()`](https://sfcheung.github.io/semhelpinghands/reference/compare_estimators.md):

``` r
se_ratios(out,
          reference = "ML")
#>    lhs op rhs se_ML se_GLS se_MLR ratio_ML ratio_GLS ratio_MLR
#> 1   y1  ~  x1 0.087  0.094  0.067        1     1.072     0.765
#> 2   y1  ~  x2 0.093  0.100  0.091        1     1.073     0.977
#> 3   y1  ~  x3 0.088  0.089  0.084        1     1.013     0.958
#> 4   y2  ~  x1 0.105  0.112  0.107        1     1.068     1.027
#> 5   y2  ~  x3 0.105  0.107  0.123        1     1.011     1.165
#> 6   y3  ~  x2 0.121  0.133  0.111        1     1.101     0.918
#> 7   y3  ~  y2 0.106  0.116  0.108        1     1.099     1.020
#> 8   x1 ~~  x1 0.131  0.129  0.136        1     0.985     1.036
#> 9   x1 ~~  x2 0.088  0.090  0.077        1     1.015     0.867
#> 10  x1 ~~  x3 0.092  0.092  0.102        1     0.999     1.114
#> 11  x2 ~~  x2 0.117  0.114  0.127        1     0.974     1.084
#> 12  x2 ~~  x3 0.087  0.089  0.091        1     1.015     1.049
#> 13  x3 ~~  x3 0.129  0.131  0.111        1     1.012     0.859
#> 14  y1 ~~  y1 0.098  0.100  0.119        1     1.015     1.210
#> 15  y1 ~~  y3 0.092  0.093  0.104        1     1.015     1.134
#> 16  y2 ~~  y2 0.143  0.139  0.141        1     0.973     0.987
#> 17  y3 ~~  y3 0.171  0.167  0.171        1     0.979     1.005
```

See the help page of
[`compare_estimators()`](https://sfcheung.github.io/semhelpinghands/reference/compare_estimators.md)
for other options.

## Bootstrapping

2025-11-22: A new package,
[`semboottools`](https://yangzhen1999.github.io/semboottools/), can do
what
[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
in this package do, with more features, as well as some diagnostic
functions. The function
[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md),
as well as its helpers, such as
[`store_boot_def()`](https://sfcheung.github.io/semhelpinghands/reference/store_boot_def.md)
and
[`plot_boot()`](https://sfcheung.github.io/semhelpinghands/reference/plot_boot.md),
will stay in
[`semhelpinghands`](https://sfcheung.github.io/semhelpinghands/). , but
will not be further developed. Users are recommended to use
`semboottools::standardizedSolution_boot()` and other functions in
[`semboottools`](https://yangzhen1999.github.io/semboottools/) instead
of this function.

One issue with the standardized solution is the confidence intervals.
They are based on the delta method even if `se = "boot"` is used. For
indirect effects, for which bootstrap confidence intervals are commonly
used, the confidence intervals for them in the standardized solution are
not what usually reported other tools for mediation. There are some
other powerful tools on the Internet and the \[Google Group for
lavaan\], see [this
thread](https://groups.google.com/g/lavaan/c/0RSsh4M6zQg/m/I85YFAiLAAAJ),
to address this problem. I wrote
[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
not to replace them (it obviously can’t), but to address a very specific
case I usually encounter myself: Generating the bootstrap confidence
intervals for standardized estimates based on the bootstrap estimates
already generated by `se = "boot"`.

Please see
[`vignette("standardizedSolution_boot_ci")`](https://sfcheung.github.io/semhelpinghands/articles/standardizedSolution_boot_ci.md)
for an illustration on
[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md).

Another issue is examine bootstrap estimates, such as the distribution
of the estimates. The function
[`plot_boot()`](https://sfcheung.github.io/semhelpinghands/reference/plot_boot.md)
and related functions can be used to compute bootstrap estimates and
plot them. The bootstrap estimates of free parameters (stored by
`lavaan`), user-defined parameters (computed by
[`store_boot_def()`](https://sfcheung.github.io/semhelpinghands/reference/store_boot_def.md)),
and the standardized solution (computed by
[`store_boot_est_std()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md))
can be plotted.

Please see the
[article](https://sfcheung.github.io/semhelpinghands/articles/plot_boot.html)
on
[`plot_boot()`](https://sfcheung.github.io/semhelpinghands/reference/plot_boot.md)
on how to use this function.

## Others

### Showing the Options in a Model

[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) and its
wrappers suc has
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) allow users
to set several options using an `estimator`: `ML`, `GLS`, `WLSMV`, and
others. However, it is not easy to remember what the options set for an
estimator. Instead of finding them from the output of
[`summary()`](https://rdrr.io/r/base/summary.html), this function shows
some of them in one table for a quick overview. This is an example:

``` r
data(dvs_ivs)
mod <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit_default <- sem(model = mod,
                   data = dvs_ivs)
show_more_options(fit_default)
#>  Options                             Call    Actual  
#>  Estimator(s)                        default ML      
#>  Standard Error (SE)                 default standard
#>  Model Test Statistic(s)             default standard
#>  How Missing Data is Handled         default listwise
#>  Information Matrix (for SE)         default expected
#>  Information Matrix (for Model Test) default expected
#>  Mean Structure                      default No      
#>  'x' Fixed                           default TRUE
fit_MLR <- sem(model = mod,
               data = dvs_ivs,
               estimator = "MLR")
show_more_options(fit_MLR)
#>  Options                             Call    Actual            
#>  Estimator(s)                        MLR     ML                
#>  Standard Error (SE)                 default robust.huber.white
#>  Model Test Statistic(s)             default yuan.bentler.mplus
#>  How Missing Data is Handled         default listwise          
#>  Information Matrix (for SE)         default observed          
#>  Information Matrix (for Model Test) default observed          
#>  Mean Structure                      default No                
#>  'x' Fixed                           default TRUE
fit_MLR_fiml <- sem(model = mod,
                    data = dvs_ivs,
                    estimator = "MLR",
                    missing = "fiml")
show_more_options(fit_MLR_fiml)
#>  Options                             Call    Actual            
#>  Estimator(s)                        MLR     ML                
#>  Standard Error (SE)                 default robust.huber.white
#>  Model Test Statistic(s)             default yuan.bentler.mplus
#>  How Missing Data is Handled         fiml    ml                
#>  Information Matrix (for SE)         default observed          
#>  Information Matrix (for Model Test) default observed          
#>  Mean Structure                      default Yes               
#>  'x' Fixed                           default TRUE
```

### Recoding Minimization History

In structural equation modeling, closed-form solution is rare and
optimization (minimization) is used to find the/a solution. Out of
curiosity and teaching, I wrote a function to capture the minimization
history so I can examine it or even plot the process. The function,
[`record_history()`](https://sfcheung.github.io/semhelpinghands/reference/record_history.md),
is still in early development but should work for now for common simple
scenarios. Please refer the help page of
[`record_history()`](https://sfcheung.github.io/semhelpinghands/reference/record_history.md)
to learn more about it.

### Add Covariances Between “Exogenous” Variables

In `lavaan`, I rarely need to manually add covariances between exogenous
variables (defined in a loose sense: variables appear on the right-hand
side but not on the left-hand side of `~`). However, I came into a
situation in which `lavaan` will not do this (for good reasons). For
example, when a covariance between a residual term and an exogenous
variable is set to free. I wrote two simple functions,
[`add_exo_cov()`](https://sfcheung.github.io/semhelpinghands/reference/auto_cov.md)
and
[`auto_exo_cov()`](https://sfcheung.github.io/semhelpinghands/reference/auto_cov.md),
for this purpose. Please refer to their help pages for further
information.
