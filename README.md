# semhelpinghands

An assortment of helper functions for doing structural equation modeling
(SEM), mainly by `lavaan::lavaan()` and its wrappers, such as
`lavaan::sem()` and `lavaan::cfa()`. This package is not for implementing
advanced statistical procedures. They are functions for tasks that come
in handy for doing SEM in general, including understanding and interpreting
the output.

Many of the tasks can be done by basic R code. However, these functions can
help users who are not familiar with R programming or use R only occasionally
to do these tasks without writing their own code.

# Installation

The latest development version at GitHub can be installed by `remotes::install_github()`:

```
remotes::install_github("sfcheung/semhelpinghands")
```