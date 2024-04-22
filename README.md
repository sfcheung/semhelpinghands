<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status](https://www.r-pkg.org/badges/version/semhelpinghands?color=blue)](https://CRAN.R-project.org/package=semhelpinghands)
[![CRAN: Release Date](https://www.r-pkg.org/badges/last-release/semhelpinghands?color=blue)](https://cran.r-project.org/package=semhelpinghands)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/semhelpinghands?color=blue)](https://r-pkg.org/pkg/semhelpinghands)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/semhelpinghands.svg)](https://github.com/sfcheung/semhelpinghands)
[![Last Commit at Main](https://img.shields.io/github/last-commit/sfcheung/semhelpinghands.svg)](https://github.com/sfcheung/semhelpinghands/commits/main)
[![R-CMD-check](https://github.com/sfcheung/semhelpinghands/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/semhelpinghands/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# semhelpinghands <img src="man/figures/logo.png" align="right" height="150" />

(Version 0.1.11 updated on 2024-04-22, [release history](https://sfcheung.github.io/semhelpinghands/news/index.html))

This package is an assortment of time-saving helper functions for doing structural
equation modeling
(SEM), mainly by `lavaan::lavaan()` and its wrappers, such as
`lavaan::sem()` and `lavaan::cfa()`. This package is not for implementing
advanced statistical procedures. This should be the job of excellent
packages such as `semTools`. These helper functions are for tasks that come
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

# Home Page

More about this package can be found from the GitHub page of this package:

[https://sfcheung.github.io/semhelpinghands/](https://sfcheung.github.io/semhelpinghands/)

# Installation

The latest version at CRAN can be installed by `install.packages()`:

```
install.packages("semhelpinghands")
```

The latest developmental version at GitHub can be installed by `remotes::install_github()`:

```
remotes::install_github("sfcheung/semhelpinghands")
```

# Motive

One major motive behind all these functions
is writing something for my own work
with others. So,
each function should be at least useful
to me myself and I myself will use them.
Nevertheless, maybe there are others
who, like me, just want some simple
functions for some simple tasks. I
also like using packages to pack functions
I used myself in my work, so I do not have to
source files again and again. Therefore,
I packaged them into `semhelpinghands`
also for myself.
I hope this package will also be useful
for some others.

I will only devote a limited amount of
time on this package. The package and
its functions are means for me to do
research. They are not the goals
themselves.

# Acknowledgement

I would like to thank Prof. Rosseel and
Prof. Jorgensen for developing `lavaan`
and `semTools`. Their advice in the
[lavaan Google Group](https://groups.google.com/g/lavaan?pli=1)
and GitHub pages (for [lavaan](https://github.com/yrosseel/lavaan)
and [semTools](https://github.com/simsem/semTools))
on various issues about `lavaan`
helped me in writing some
of the functions here, before they
were packed into `semhelpinghands`.
If there are people who inspired some
of the functions but I forgot to acknowledge,
please let me know.

# Suggestions and Bugs

This package is still under development.
There will be bugs, and there are
limitations. Comments and suggestions
are welcomed. Feel free to add an issue
at [GitHub](https://github.com/sfcheung/semhelpinghands/issues).
Although I may not be able to address
all of them because this package is
intended to be a collection of simple
functions for simple tasks, and
they are designed
to be easy to maintain, I will try to
do what I can do.
