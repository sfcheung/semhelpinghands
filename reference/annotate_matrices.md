# Annotate the Matrices of a 'lavaan' Model

Label the elements of the model matrices in a lavaan model.

## Usage

``` r
annotate_matrices(fit)

# S3 method for class 'annotate_matrices'
print(x, ...)
```

## Arguments

- fit:

  The output of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or
  its wrappers, such as
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- x:

  A 'annotate_matrices'-class object. The output of
  `annotate_matrices()`.

- ...:

  Optional arguments. To be passed to the default print method.

## Value

`annotate_matrices()` returns an `annotate_matrices`-class object, which
is a list of model matrices, with elements annotated:

- If a parameter is free, then it is represented by `"lhs-op-rhs"`
  according to the parameter estimate data frame.

- If a parameter is fixed but appears in the parameter table, it is
  represented by `"(lhs-op-rhs = x)"` , x the value it is fixed to.

- If a parameter is fixed to zero but not in the parameter table, then
  it is represented by 0.

The print-method return the input, `x`. It was called for its
side-effect.

## Details

This function annotates the model matrices in a 'lavaan'-class object.
This function is not to be used in analysis. It is a learning tool, for
learners to understand the relation between the model matrices and the
model parameters.

It currently supports a single-level single-group model only.

## Methods (by generic)

- `print(annotate_matrices)`: The print method of the output of
  `annotate_matrices()`

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
# Adapted from https://lavaan.ugent.be/tutorial/cfa.html

library(lavaan)
HS.model <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'
fit_cfa <- cfa(HS.model,
               data = HolzingerSwineford1939)
annotate_matrices(fit_cfa)
#> $lambda
#>              visual           textual           speed
#> x1 (visual=~x1 = 1)                 0               0
#> x2       visual=~x2                 0               0
#> x3       visual=~x3                 0               0
#> x4                0 (textual=~x4 = 1)               0
#> x5                0       textual=~x5               0
#> x6                0       textual=~x6               0
#> x7                0                 0 (speed=~x7 = 1)
#> x8                0                 0       speed=~x8
#> x9                0                 0       speed=~x9
#> 
#> $theta
#>        x1     x2     x3     x4     x5     x6     x7     x8     x9
#> x1 x1~~x1      0      0      0      0      0      0      0      0
#> x2      0 x2~~x2      0      0      0      0      0      0      0
#> x3      0      0 x3~~x3      0      0      0      0      0      0
#> x4      0      0      0 x4~~x4      0      0      0      0      0
#> x5      0      0      0      0 x5~~x5      0      0      0      0
#> x6      0      0      0      0      0 x6~~x6      0      0      0
#> x7      0      0      0      0      0      0 x7~~x7      0      0
#> x8      0      0      0      0      0      0      0 x8~~x8      0
#> x9      0      0      0      0      0      0      0      0 x9~~x9
#> 
#> $psi
#>                  visual          textual          speed
#> visual   visual~~visual  visual~~textual  visual~~speed
#> textual visual~~textual textual~~textual textual~~speed
#> speed     visual~~speed   textual~~speed   speed~~speed
#> 
```
