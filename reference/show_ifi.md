# Visualize How CFI and TLI Are Computed

Show how CFI and TLI are computed using a graph of model chi-square vs.
model degrees of freedom.

## Usage

``` r
show_cfi(fit, ...)

show_tli(fit, ...)

show_ifi(fit, fit_measures = c("cfi", "tli"), test = c("standard"))
```

## Arguments

- fit:

  An output of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or
  its wrappers (e.g.,
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)).

- ...:

  Arguments to be passed to `show_ifi()`.

- fit_measures:

  The fit measures to be plotted. Acceptable values are `"cfi"` and
  `"tli"`.

- test:

  The type of model chi-square test. It corresponds to the `test`
  argument of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or
  its wrappers. Only `"standard"` is supported for now.

## Value

An output of
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
that can be further modified.

## Details

This function receives an output of
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or its
wrappers (e.g.,
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)) and
illustrates how CFI is computed.

## Functions

- `show_cfi()`: A wrapper of `show_ifi()` with `fit_measures = "cfi"`.

- `show_tli()`: A wrapper of `show_ifi()` with `fit_measures = "tli"`.

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

# By default show how CFI is computed
show_ifi(fit)


# Show how TLI is computed
show_ifi(fit, fit_measures = "tli")


# Wrappers
show_cfi(fit)

show_tli(fit)

```
