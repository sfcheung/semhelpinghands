# Diagnostic Plots of Bootstrap Estimates in 'lavaan'

Plots for examining the distribution of bootstrap estimates in a model
fitted by `lavaan`.

## Usage

``` r
plot_boot(
  object,
  param,
  standardized = NULL,
  nclass = NULL,
  hist_color = "lightgrey",
  hist_linewidth = 1,
  density_line_type = "solid",
  density_line_color = "blue",
  density_line_linewidth = 2,
  est_line_type = "dotted",
  est_line_color = "red",
  est_line_linewidth = 2,
  qq_dot_size = 2,
  qq_dot_color = "black",
  qq_dot_pch = 16,
  qq_line_linewidth = 2,
  qq_line_color = "black",
  qq_line_linetype = "solid"
)
```

## Arguments

- object:

  Either a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object with bootstrap estimates stored, or the output of
  [`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md).
  For standardized solution and user-defined parameters, if the object
  is a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object, the estimates need to be stored by
  [`store_boot_est_std()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
  or
  [`store_boot_def()`](https://sfcheung.github.io/semhelpinghands/reference/store_boot_def.md).

- param:

  String. The name of the parameter to be plotted, which should be the
  name as appeared in a call to
  [`coef()`](https://rdrr.io/r/stats/coef.html).

- standardized:

  Logical. Whether the estimates from the standardized solution are to
  be plotted. Default is `NULL`. If `object` is a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object, then this is a required parameter and users need to explicitly
  set it to `TRUE` or `FALSE`. If `object` is the output of
  [`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md),
  then this argument is ignored ( forced to be `TRUE` internally).

- nclass:

  The number of breaks. This argument will be passed to
  [`hist()`](https://rdrr.io/r/graphics/hist.html). Default is `NULL`.

- hist_color:

  String. The color of the bars in the histogram. It will be passed to
  [`hist()`](https://rdrr.io/r/graphics/hist.html) for the argument
  `col`. Default is `"lightgrey"`.

- hist_linewidth:

  The width of the borders of the bars in the histogram. Default is 1.

- density_line_type:

  String. The type of the line of the density curve in the histogram. It
  will be passed to [`lines()`](https://rdrr.io/r/graphics/lines.html)
  for the argument `lty`. Default is `"solid"`.

- density_line_color:

  String. The color of the density curve in the histogram. It will be
  passed to [`lines()`](https://rdrr.io/r/graphics/lines.html) for the
  argument `col`. Default is `"blue"`.

- density_line_linewidth:

  The width of the density curve in the histogram. It will be passed to
  [`lines()`](https://rdrr.io/r/graphics/lines.html) for the argument
  `lwd`. Default is 2.

- est_line_type:

  String. The type of the vertical line in the histogram showing the
  point estimate of the parameter. It will be passed to
  [`abline()`](https://rdrr.io/r/graphics/abline.html) for the argument
  `lty`. Default is `"dotted"`,

- est_line_color:

  String. The color of the vertical line showing the point estimate in
  the histogram. It will be passed to
  [`abline()`](https://rdrr.io/r/graphics/abline.html) for the argument
  `col`. Default is `"red"`.

- est_line_linewidth:

  The width of the vertical line showing the point estimate in the
  histogram. It will be passed to
  [`hist()`](https://rdrr.io/r/graphics/hist.html) for the argument
  `lwd`. Default is 2.

- qq_dot_size:

  The size of the points in the normal QQ-plot. It will be passed to
  [`qqnorm()`](https://rdrr.io/r/stats/qqnorm.html) for the argument
  `cex`. Default is 2.

- qq_dot_color:

  String. The color of the points in the normal QQ-plot. It will be
  passed to [`qqnorm()`](https://rdrr.io/r/stats/qqnorm.html) for the
  argument `col`. Default is `"black"`.

- qq_dot_pch:

  Numeric. The shape of the points in the normal QQ-plot. It will be
  passed to [`qqnorm()`](https://rdrr.io/r/stats/qqnorm.html) for the
  argument `pch`. Default is 16.

- qq_line_linewidth:

  The width of the diagonal line to be drawn in the normal QQ-plot. It
  will be passed to [`qqline()`](https://rdrr.io/r/stats/qqnorm.html)
  for the argument `lwd`. Default is 2.

- qq_line_color:

  String. The color of the diagonal line to be drawn in the normal
  QQ-plot. It will be passed to
  [`qqline()`](https://rdrr.io/r/stats/qqnorm.html) for the argument
  `col`. Default is `"black"`.

- qq_line_linetype:

  The type of the diagonal line to be drawn in the normal QQ-plot.
  Default is `"solid"`.

## Value

Return the original
[lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
object invisibly. Called for its side-effect (plotting the graphs).

## Details

**NOTE**: A new package,
[`semboottools`](https://yangzhen1999.github.io/semboottools/) (Yang &
Cheung, 2026), can do what
[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
does, with more features, as well as some diagnostic functions. The
function
[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md),
as well as its helpers, such as
[`store_boot_def()`](https://sfcheung.github.io/semhelpinghands/reference/store_boot_def.md)
and `plot_boot()`, will stay in this package, but will not be further
developed. Users are recommended to use
`semboottools::standardizedSolution_boot()` and other functions in
[`semboottools`](https://yangzhen1999.github.io/semboottools/) instead
of this function.

Rousselet, Pernet, and Wilcox (2021) argued that when using
bootstrapping, it is necessary to examine the distribution of bootstrap
estimates. This can be done when
[`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) is used because
it has a `plot` method for its output. This cannot be easily done in
model fitted by
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html).

The function `plot_boot()` is used for plotting the distribution of
bootstrap estimates for a model fitted by `lavaan` in a format similar
to that of the output of
[`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html), with a
histogram on the left and a normal QQ-plot on the right.

For free parameters in a model (unstandardized), it can be called
directly on the output of `lavaan` and retrieves the stored estimates.

For estimates of user-defined parameters, call
[`store_boot_def()`](https://sfcheung.github.io/semhelpinghands/reference/store_boot_def.md)
first to compute and store the bootstrap estimates first.

For estimates in standardized solution, for both free and user-defined
parameters, call
[`store_boot_est_std()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
first to compute and store the bootstrap estimates in the standardized
solution.

Since Version 0.1.11.2, it can also plot bootstrap estimates in the
output of
[`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md).

## References

Rousselet, G. A., Pernet, C. R., & Wilcox, R. R. (2021). The percentile
bootstrap: A primer with step-by-step instructions in R. *Advances in
Methods and Practices in Psychological Science*, *4*(1), 1–10.
[doi:10.1177/2515245920911881](https://doi.org/10.1177/2515245920911881)

Yang, W., & Cheung, S. F. (2026). Forming bootstrap confidence intervals
and examining bootstrap distributions of standardized coefficients in
structural equation modelling: A simplified workflow using the R package
*semboottools*. *Behavior Research Methods, 58*(2), 38.
[doi:10.3758/s13428-025-02911-z](https://doi.org/10.3758/s13428-025-02911-z)

## See also

[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html),
[`store_boot_est_std()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md),
and
[`store_boot_def()`](https://sfcheung.github.io/semhelpinghands/reference/store_boot_def.md).

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

library(lavaan)

data(simple_mediation)
mod <-
"
m ~ a * x
y ~ b * m + x
ab := a * b
"
fit <- sem(mod, simple_mediation,
           se = "bootstrap",
           bootstrap = 50,
           iseed = 985714)

# Can plot bootstrap estimates for
# free parameters directly
# Note that 'standardized' must be always be set to
# either TRUE or FALSE. No default value.
plot_boot(fit, "a", standardized = FALSE)


# For estimates of user-defined parameters,
# call store_boot_def() first.
fit <- store_boot_def(fit)
plot_boot(fit, "ab", standardized = FALSE)


# For estimates in standardized solution,
# call store_boot_est_std() first.
fit <- store_boot_est_std(fit)
plot_boot(fit, "a", standardized = TRUE)

plot_boot(fit, "ab", standardized = TRUE)


# It can also plot the estimates stored
# in the output of standardizedSolution_boot_ci().
std_boot <- standardizedSolution_boot_ci(fit)
plot_boot(std_boot, "ab")

plot_boot(fit, "ab", standardized = TRUE)


```
