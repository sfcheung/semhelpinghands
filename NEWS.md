# semhelpinghands 0.1.6.2

- Update a badge in README.md. (0.1.6.1)
- Update the pkgdown site. (0.1.6.2)

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