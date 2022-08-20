# semhelpinghands

An assortment of time-saving helper functions for doing structural
equation modeling
(SEM), mainly by `lavaan::lavaan()` and its wrappers, such as
`lavaan::sem()` and `lavaan::cfa()`. This package is not for implementing
advanced statistical procedures. This should be the job of excellent
packages like `semTools`. These helper functions are for tasks that come
in handy (at least to me) when doing SEM. They are
usually used *during* the data analysis phase, not for publishing
or presenting results.

Many of the tasks can be done by base R code or by `dplyr` functions.
However, these functions may help users who are not familiar with
R programming or use R only occasionally to do these tasks
without writing their own code, or for people like me who know
how to do this using base R but do not want to write the code every
time.

I intentionally use base R if possible because I want to keep
`semhelpinghands` a light weight package, depending on as few
other packages as possible, so that I do not need to worry about
deprecated functions or updates in other packages that break things,
defeating the "time saving" purpose of this package.

# Installation

The latest development version at GitHub can be installed by `remotes::install_github()`:

```
remotes::install_github("sfcheung/semhelpinghands")
```