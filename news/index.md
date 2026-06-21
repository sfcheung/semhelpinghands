# Changelog

## semhelpinghands 0.1.14.1

### Miscellaneous

- The argument `allow_incomplete` of
  [`ptable_to_syntax()`](https://sfcheung.github.io/semhelpinghands/reference/ptable_to_syntax.md)
  will no longer support `lavaan` 0.7-1 and later version. (0.1.14.1)

## semhelpinghands 0.1.14

CRAN release: 2026-02-10

### Miscellaneous

- Add notes to notify users that
  [`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
  and its helpers will no longer be further developed in this package.
  Users are recommended to use the package
  `semboottools`(<https://yangzhen1999.github.io/semboottools/>) to form
  bootstrap confidence intervals for the standardized solution.
  (0.1.12.1)

- Updated
  [`vec_compRelSEM()`](https://sfcheung.github.io/semhelpinghands/reference/vector_from_lavaan.md)
  for the upcoming `semTools` 0.5-8. Thank
  [@TDJorgensen](https://github.com/TDJorgensen) for updating this
  function. (0.1.12.2)

## semhelpinghands 0.1.12

CRAN release: 2024-11-02

### New Features

- [`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
  now support both percentile confidence intervals (`"perc"`, the
  default) and bias-corrected confidence intervals (`"bc"` or
  `"bca.simple"`), controlled by the argument `boot_ci_type`. (0.1.11.1)

- Updated
  [`plot_boot()`](https://sfcheung.github.io/semhelpinghands/reference/plot_boot.md)
  to support the output of
  [`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md).
  (0.1.11.2)

### Others

- Improve the error message when calling
  [`plot_boot()`](https://sfcheung.github.io/semhelpinghands/reference/plot_boot.md)
  with `standardize = TRUE` but
  [`store_boot_est_std()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
  has not been called. (0.1.11.3)

## semhelpinghands 0.1.11

CRAN release: 2024-04-26

### Minor Fixes

- Revised
  [`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
  to handle unusual cases in which the bootstrap estimates in the
  standardized solution cannot be computed. (0.1.10.1)
- Corrected some model specifications that are incompatible with the new
  parser of `lavaan`. (0.1.10.2)

## semhelpinghands 0.1.10

CRAN release: 2024-03-12

### New Features

- Added
  [`ptable_to_syntax()`](https://sfcheung.github.io/semhelpinghands/reference/ptable_to_syntax.md)
  for converting a `lavaan` parameter table to a `lavaan` model syntax.
  (0.1.9.1 - 0.1.9.6).
- Added
  [`fitMeasures_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/fitMeasures_by_models.md),
  with a print method
  ([`print.fit_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/print.fit_by_models.md)).
  Generate a compact table of the fit measures for two or more models,
  with models as the columns. For quick preview and comparison.
  (0.1.9.12)

### Others

- Fixed some typos in `NEWS.md`. (0.1.9.8)
- The default of `col_names` for
  [`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md)
  is now `"est.std"` if `use_standardizedSolution` is `TRUE`. (0.1.9.11)

## semhelpinghands 0.1.9

CRAN release: 2023-09-17

### New Features

- Added
  [`store_boot_est_std()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
  and
  [`store_boot_def()`](https://sfcheung.github.io/semhelpinghands/reference/store_boot_def.md)
  for computing and storing bootstrap estimates of the standardized
  solution and user parameters, respectively. Added
  [`get_boot_est_std()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
  and
  [`get_boot_def()`](https://sfcheung.github.io/semhelpinghands/reference/store_boot_def.md)
  to extract the computed estimates. (0.1.8.2, 0.1.8.3, 0.1.8.5).
- Set the class of the output of
  [`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
  to `std_solution_boot`, with a print method for printing the output as
  in the printout of the summary output of a `lavaan-class` object.
  (0.1.8.4)
- Added
  [`plot_boot()`](https://sfcheung.github.io/semhelpinghands/reference/plot_boot.md)
  for diagnostic plots for bootstrapping, similiar to those for the
  output of [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html).
  Can be used for `lavaan`’s bootstrap estimates for free parameters,
  user-defined parameters (stored by
  [`store_boot_def()`](https://sfcheung.github.io/semhelpinghands/reference/store_boot_def.md)),
  or standardized solution (stored by
  [`store_boot_est_std()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)).
  Also added an article to the `pkgdown` website (0.1.8.6, 0.1.8.10).

## semhelpinghands 0.1.8

CRAN release: 2023-07-06

### New Features

- Migrated functions from the `semunpack` package to this package:
  [`annotate_matrices()`](https://sfcheung.github.io/semhelpinghands/reference/annotate_matrices.md),
  [`plot_models_fm()`](https://sfcheung.github.io/semhelpinghands/reference/plot_models_fm.md),
  [`show_ifi()`](https://sfcheung.github.io/semhelpinghands/reference/show_ifi.md),
  [`show_cfi()`](https://sfcheung.github.io/semhelpinghands/reference/show_ifi.md),
  [`show_tli()`](https://sfcheung.github.io/semhelpinghands/reference/show_ifi.md),
  and
  [`show_options()`](https://sfcheung.github.io/semhelpinghands/reference/show_options.md).
  (0.1.6.4)
- Added a set of wrappers (e.g.,
  [`vec_rsquare()`](https://sfcheung.github.io/semhelpinghands/reference/vector_from_lavaan.md))
  to extract some commonly requested information and return output as a
  named numeric vector. (0.1.6.5)

### Others

- Updated a badge in README.md. (0.1.6.1)
- Updated the pkgdown site. (0.1.6.2)
- Added a test for noSuggests in R CMD check. (0.1.6.3)
- Shorten the y-axis label of the plot of
  [`show_ifi()`](https://sfcheung.github.io/semhelpinghands/reference/show_ifi.md).
  (0.1.8)

## semhelpinghands 0.1.6

CRAN release: 2023-01-06

- Fixed word wrap in DESCRIPTION. (0.1.4.5)
- Added R CMD Check action. (0.1.4.5)
- Fixed a typo error; updated doc. (0.1.4.6)
- Modified
  [`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
  to work with lavaan 0.6-13. Bootstrap replications with errors (e.g.,
  inadmissible solutions), if flagged, will be removed.
- Fixed an issue on README.md. (0.1.6)

## semhelpinghands 0.1.4.4 (CRAN)

CRAN release: 2022-08-27

- Fixed some typo errors. (0.1.4.2)
- Fixed a link. (0.1.4.3)
- Fixed CRAN related issues. (0.1.4.4)

## semhelpinghands 0.1.4.1

- Added
  [`sort_by()`](https://sfcheung.github.io/semhelpinghands/reference/sort_by.md).
  Update some functions to call
  [`sort_by()`](https://sfcheung.github.io/semhelpinghands/reference/sort_by.md).
  (0.1.4.1)

## semhelpinghands 0.1.4.0

- Improved the notes in examples. (0.1.2.1)
- Fixed some bugs. (0.1.2.1)
- Provided the background information on
  [`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md).
  (0.1.2.2, 0.1.2.3)
- Added
  [`se_ratios()`](https://sfcheung.github.io/semhelpinghands/reference/compare_estimators.md)
  for comparing standard errors by different estimators. (0.1.2.4)
- Added
  [`show_more_options()`](https://sfcheung.github.io/semhelpinghands/reference/show_more_options.md).
  (0.1.2.5)
- Fixed a bug in estimates table manipulation functions. (0.1.3.2)
- Fixed a bug in the test that used known results. (0.1.4.0)

## semhelpinghands 0.1.2.0

- Added
  [`standardizedSolution_boot_ci()`](https://sfcheung.github.io/semhelpinghands/reference/standardizedSolution_boot_ci.md)
  for forming bootstrap percentile confidence intervals for standardized
  solution in a `lavaan` output.
- Added
  [`add_sig()`](https://sfcheung.github.io/semhelpinghands/reference/add_sig.md)
  to add “stars” to a parameter estimates table.
- Added
  [`group_by_dvs()`](https://sfcheung.github.io/semhelpinghands/reference/group_estimates.md),
  [`group_by_ivs()`](https://sfcheung.github.io/semhelpinghands/reference/group_estimates.md),
  [`group_by_models()`](https://sfcheung.github.io/semhelpinghands/reference/group_by_models.md)
  to group parameter estimates.
- Added
  [`record_history()`](https://sfcheung.github.io/semhelpinghands/reference/record_history.md)
  to record optimization history.
- Added
  [`filter_by()`](https://sfcheung.github.io/semhelpinghands/reference/filter_by.md)
  for selecting rows in a parameter estimates table.
- Added `group_by_group()` to group parameter estimates.
- Added
  [`add_exo_cov()`](https://sfcheung.github.io/semhelpinghands/reference/auto_cov.md)
  and
  [`auto_exo_cov()`](https://sfcheung.github.io/semhelpinghands/reference/auto_cov.md)
  to generate covariances.
