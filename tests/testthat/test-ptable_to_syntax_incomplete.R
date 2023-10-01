skip("WIP")

library(testthat)

library(lavaan)

# SEM

mod <-
"
visual  =~ x1 + d2*x2 + 1.5*x3
textual =~ x4 + x6 + d5*x5
speed   =~ x7 + d8*x8 + d9*x9 + d9*x6
visual ~ a*textual
speed ~ start(0.1) * b*visual + start(NA) * textual + ageyr
ab: = a * b
d2 == d5
d8 == d2
x4 ~1
x7 ~ .5*1
x9 ~ i9*1
x5 ~ start(.2) * 1
"

pt_incomplete <- lavParseModelString(mod,
                                     as.data.frame. = TRUE)
fit_chk <- sem(mod, data = HolzingerSwineford1939)
mod_chk <- ptable_to_syntax(pt_incomplete, allow_incomplete = TRUE)
fit <- sem(mod_chk, data = HolzingerSwineford1939)

test_that("partable_to_syntax", {
    expect_identical(coef(fit),
                     coef(fit_chk))
  })

# Path Model

mod <-
"
x4 ~ x1 + .3 * x2 + x3
x5 ~ a * x1 + x2
x4 ~~ .3 * x5
"

pt_incomplete <- lavParseModelString(mod,
                                     as.data.frame. = TRUE)
fit_chk <- sem(mod, data = HolzingerSwineford1939)
mod_chk <- ptable_to_syntax(pt_incomplete, allow_incomplete = TRUE)
fit <- sem(mod_chk, data = HolzingerSwineford1939)

test_that("partable_to_syntax", {
    expect_identical(coef(fit),
                     coef(fit_chk))
  })

# Path Model

mod <-
"
x4 ~ x1 + .3 * x2 + start(4) * x3
x5 ~ a * x1 + x2
x4 ~~ .3 * x5
x1 ~~ a * x1
"

pt_incomplete <- lavParseModelString(mod,
                                     as.data.frame. = TRUE,
                                     warn = FALSE)
fit_chk <- sem(mod, data = HolzingerSwineford1939, warn = FALSE)
mod_chk <- ptable_to_syntax(pt_incomplete, allow_incomplete = TRUE)
fit <- sem(mod_chk, data = HolzingerSwineford1939, warn = FALSE)

test_that("partable_to_syntax", {
    expect_identical(coef(fit),
                     coef(fit_chk))
  })

# SEM
# With means

mod <-
"
visual  =~ x1 + d2*x2 + 1.5*x3
textual =~ x4 + x6 + d5*x5
speed   =~ x7 + d8*x8 + d9*x9 + d9*x6
visual ~ a*textual
speed ~ start(0.1) * b*visual + start(NA) * textual + ageyr
ab: = a * b
d2 == d5
d8 == d2
x1 ~ start(.5) * 1
x2 ~ start(.6) * b*1
x4 ~ b*1
"

pt_incomplete <- lavParseModelString(mod,
                                     as.data.frame. = TRUE,
                                     warn = FALSE)
fit_chk <- sem(mod, data = HolzingerSwineford1939, warn = FALSE)
mod_chk <- ptable_to_syntax(pt_incomplete, allow_incomplete = TRUE)
fit <- sem(mod_chk, data = HolzingerSwineford1939, warn = FALSE)

test_that("partable_to_syntax", {
    expect_identical(coef(fit),
                     coef(fit_chk))
  })

# TO PROCESS


# Multiple Group Models

mod <-
"
x4 ~ x1 + x2 + x3
x5 ~ x1 + x2
"

fit <- sem(mod, data = HolzingerSwineford1939, group = "school")
ptable <- parameterTable(fit)

test_that("partable_to_syntax", {
    expect_error(ptable_to_syntax(fit))
    expect_error(ptable_to_syntax_check_ptable(ptable))
  })

# Multilevel models

# Adapted from https://lavaan.ugent.be/tutorial/multilevel.html

model <- '
level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2 + x3
level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
'

fit <- sem(model = model, data = Demo.twolevel, cluster = "cluster", do.fit = FALSE)
ptable <- parameterTable(fit)

test_that("partable_to_syntax", {
    expect_error(ptable_to_syntax(fit))
    expect_error(ptable_to_syntax_check_ptable(ptable))
  })

# Categorical variables

HolzingerSwineford1939$x5c <- round(HolzingerSwineford1939$x5)

mod <-
"
x4 ~ x1 + x2 + x3
x5c ~ x1 + x2
"

fit <- sem(mod, data = HolzingerSwineford1939, ordered = "x5c")
ptable <- parameterTable(fit)

test_that("partable_to_syntax", {
    expect_error(ptable_to_syntax(fit))
    expect_error(ptable_to_syntax_check_ptable(ptable))
  })

# Formative indicators

HS.model <- ' visual  <~ x1 + x2 + x3
              textual <~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              speed ~ visual + textual'

fit <- sem(HS.model, data = HolzingerSwineford1939, do.fit = FALSE)
ptable <- parameterTable(fit)

test_that("partable_to_syntax", {
    expect_error(ptable_to_syntax(fit))
    expect_error(ptable_to_syntax_check_ptable(ptable))
  })

# Use equal()

mod <-
"
x4 ~ x1 + x2 + b*x3
x5 ~ equal('x4 ~ x1') * x1 + x2
"

fit <- sem(mod, data = HolzingerSwineford1939)
ptable <- parameterTable(fit)

test_that("partable_to_syntax", {
    expect_error(ptable_to_syntax(fit))
    expect_error(ptable_to_syntax_check_ptable(ptable))
  })
