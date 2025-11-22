# Add Covariances Between Exogenous Variables

It generates the 'lavaan' model syntax for exogenous variables in a
`lavaan` model.

## Usage

``` r
add_exo_cov(model, FUN = "sem", print = TRUE)

auto_exo_cov(model, FUN = "sem", print = TRUE)
```

## Arguments

- model:

  The model syntax to which the covariances are to be added.

- FUN:

  Name (as string) of the `lavaan` wrapper to be called. Normally should
  be `"sem"`, the default.

- print:

  Logical. Whether the generated syntax should also be printed by
  [`cat()`](https://rdrr.io/r/base/cat.html). Default is `TRUE`.

## Value

`add_exo_cov()` returns a one-element character vector of the syntax,
with lines separated by "\n". The generated syntax is appended to the
input model syntax.

`auto_exo_cov()` returns a one-element character vector of the generated
syntax, with lines separated by "\n".

## Details

The function [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)
usually will set covariances between "exogenous" variables free when
`fixed.x = FALSE` ("exogenous" is defined here as variables that appear
on the right hand side but not on the left hand side of the `~`
operator\`). However, if a covariance between the residual term of an
endogenous variable and an exogenous variable is manually set to free,
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) may not set
the aforementioned covariances free. Users will need to free them
manually, and there may be a lot of them in some models.

This function gets a model syntax and generates the syntax for these
covariances. Users can then inspect it, modify it if necessary, and then
copy and paste it to the model syntax.

## Functions

- `add_exo_cov()`: Add covariances between exogenous variables to the
  model syntax and than return the modified model syntax.

- `auto_exo_cov()`: Generate the model syntax for the covariances
  between exogenous variables.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
library(lavaan)
set.seed(8976223)
n <- 100
x <- rnorm(n)
m <- .5 * x + rnorm(n, 0, sqrt(.4))
z <- rnorm(n)
y <- .4 * m + .3 * z * m + rnorm(n, 0, .5)
dat <- data.frame(x, m, z, y)
dat$zm <- dat$z * dat$m
mod0 <-
"
m ~ x
y ~ m + z + zm
m ~~ z + zm
"
fit <- sem(mod0, dat, fixed.x = FALSE)

# Add covariances. Also printed by default.
mod0_cov <- add_exo_cov(mod0)
#> 
#> m ~ x
#> y ~ m + z + zm
#> m ~~ z + zm
#> 
#> # Added by auto_exo_cov
#> x ~~ z + zm
#> z ~~ zm

# Fit the model
fit_cov <- sem(mod0_cov, dat, fixed.x = FALSE)

# Manually adding the covariances
mod1 <-
"
m ~ x
y ~ m + z + zm
m ~~ z + zm
z  ~~ zm + x
zm ~~ x
"
fit1 <- sem(mod1, dat, meanstructure = TRUE, fixed.x = FALSE)

# Compare the results

# No manual covariances
fit
#> lavaan 0.6-20 ended normally after 15 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        11
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 5.863
#>   Degrees of freedom                                 4
#>   P-value (Chi-square)                           0.210

# Automatically generated covariances
fit_cov
#> lavaan 0.6-20 ended normally after 23 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        14
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.152
#>   Degrees of freedom                                 1
#>   P-value (Chi-square)                           0.696

# Manually added covariances
fit1
#> lavaan 0.6-20 ended normally after 23 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        19
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.152
#>   Degrees of freedom                                 1
#>   P-value (Chi-square)                           0.696

```
