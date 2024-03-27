# semhelpinghands 0.1.10.1

## Others

- Revised `standardizedSolution_boot_ci()`
  to handle unusual cases in which the
  bootstrap estimates in the
  standardized solution cannot be
  computed. (0.1.10.1)

# semhelpinghands 0.1.10

## New Features

- Added `ptable_to_syntax()` for
  converting a `lavaan` parameter
  table to a `lavaan` model syntax.
  (0.1.9.1 - 0.1.9.6).
- Added `fitMeasures_by_models()`,
  with a print method (`print.fit_by_models()`).
  Generate a compact table of the fit
  measures for two or more models, with
  models as the columns. For quick
  preview and comparison.
  (0.1.9.12)

## Others

- Fixed some typos in `NEWS.md`.
  (0.1.9.8)
- The default of `col_names` for
  `group_by_models()` is now
  `"est.std"` if `use_standardizedSolution`
  is `TRUE`. (0.1.9.11)

# semhelpinghands 0.1.9

## New Features

- Added `store_boot_est_std()` and
  `store_boot_def()` for computing
  and storing bootstrap estimates
  of the standardized solution and
  user parameters, respectively.
  Added `get_boot_est_std()`
  and `get_boot_def()` to extract
  the computed estimates. (0.1.8.2,
  0.1.8.3, 0.1.8.5).
- Set the class of the output of
  `standardizedSolution_boot_ci()`
  to `std_solution_boot`, with a
  print method for printing the output
  as in the printout of the summary
  output of a `lavaan-class` object. (0.1.8.4)
- Added `plot_boot()` for diagnostic
  plots for bootstrapping, similiar to
  those for the output of `boot::boot()`.
  Can be used for `lavaan`'s bootstrap
  estimates for free parameters,
  user-defined parameters (stored
  by `store_boot_def()`), or
  standardized solution (stored by
  `store_boot_est_std()`). Also
  added an article to the `pkgdown`
  website (0.1.8.6, 0.1.8.10).

# semhelpinghands 0.1.8

## New Features

- Migrated functions from the
  `semunpack` package to this package:
  `annotate_matrices()`,
  `plot_models_fm()`,
  `show_ifi()`, `show_cfi()`,
  `show_tli()`, and `show_options()`.
  (0.1.6.4)
- Added a set of wrappers (e.g., `vec_rsquare()`)
  to extract some commonly requested
  information and return output as a
  named numeric vector. (0.1.6.5)

## Others

- Updated a badge in README.md. (0.1.6.1)
- Updated the pkgdown site. (0.1.6.2)
- Added a test for noSuggests in R CMD check. (0.1.6.3)
- Shorten the y-axis label of the plot
  of `show_ifi()`. (0.1.8)

# semhelpinghands 0.1.6

- Fixed word wrap in DESCRIPTION. (0.1.4.5)
- Added R CMD Check action. (0.1.4.5)
- Fixed a typo error; updated doc. (0.1.4.6)
- Modified `standardizedSolution_boot_ci()` to work with lavaan 0.6-13.
  Bootstrap replications with errors (e.g., inadmissible solutions),
  if flagged, will be removed.
- Fixed an issue on README.md. (0.1.6)

# semhelpinghands 0.1.4.4 (CRAN)

- Fixed some typo errors. (0.1.4.2)
- Fixed a link. (0.1.4.3)
- Fixed CRAN related issues. (0.1.4.4)


# semhelpinghands 0.1.4.1

- Added `sort_by()`. Update some functions to call `sort_by()`. (0.1.4.1)

# semhelpinghands 0.1.4.0

- Improved the notes in examples. (0.1.2.1)
- Fixed some bugs. (0.1.2.1)
- Provided the background information on
  `standardizedSolution_boot_ci()`. (0.1.2.2, 0.1.2.3)
- Added `se_ratios()` for comparing standard errors by
  different estimators. (0.1.2.4)
- Added `show_more_options()`. (0.1.2.5)
- Fixed a bug in estimates table manipulation functions. (0.1.3.2)
- Fixed a bug in the test that used known results. (0.1.4.0)

# semhelpinghands 0.1.2.0

- Added `standardizedSolution_boot_ci()` for forming bootstrap percentile
  confidence intervals for standardized solution in a `lavaan` output.
- Added `add_sig()` to add "stars" to a parameter estimates table.
- Added `group_by_dvs()`, `group_by_ivs()`, `group_by_models()` to
  group parameter estimates.
- Added `record_history()` to record optimization history.
- Added `filter_by()` for selecting rows in a parameter estimates table.
- Added `group_by_group()` to group parameter estimates.
- Added `add_exo_cov()` and `auto_exo_cov()` to generate covariances.