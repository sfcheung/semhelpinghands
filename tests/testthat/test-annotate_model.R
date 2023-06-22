library(testthat)

library(lavaan)

HS.model <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'
fit_cfa <- cfa(HS.model,
               data = HolzingerSwineford1939)

out <- annotate_matrices(fit_cfa)
out_check <- structure(list(lambda = structure(c("(visual=~x1 = 1)", "visual=~x2",
"visual=~x3", "0", "0", "0", "0", "0", "0", "0", "0", "0", "(textual=~x4 = 1)",
"textual=~x5", "textual=~x6", "0", "0", "0", "0", "0", "0", "0",
"0", "0", "(speed=~x7 = 1)", "speed=~x8", "speed=~x9"), dim = c(9L,
3L), dimnames = list(c("x1", "x2", "x3", "x4", "x5", "x6", "x7",
"x8", "x9"), c("visual", "textual", "speed"))), theta = structure(c("x1~~x1",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "x2~~x2", "0", "0",
"0", "0", "0", "0", "0", "0", "0", "x3~~x3", "0", "0", "0", "0",
"0", "0", "0", "0", "0", "x4~~x4", "0", "0", "0", "0", "0", "0",
"0", "0", "0", "x5~~x5", "0", "0", "0", "0", "0", "0", "0", "0",
"0", "x6~~x6", "0", "0", "0", "0", "0", "0", "0", "0", "0", "x7~~x7",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "x8~~x8", "0", "0",
"0", "0", "0", "0", "0", "0", "0", "x9~~x9"), dim = c(9L, 9L), dimnames = list(
    c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"),
    c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"))),
    psi = structure(c("visual~~visual", "visual~~textual", "visual~~speed",
    "visual~~textual", "textual~~textual", "textual~~speed",
    "visual~~speed", "textual~~speed", "speed~~speed"), dim = c(3L,
    3L), dimnames = list(c("visual", "textual", "speed"), c("visual",
    "textual", "speed")))), class = c("annotate_matrices", "list"
))

test_that("Check output", {
    expect_identical(out, out_check)
  })