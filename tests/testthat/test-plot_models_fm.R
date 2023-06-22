skip("To be run in an interactive mode")

library(testthat)

library(lavaan)

# From the help page of modificationIndices

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)
modindices(fit, sort = TRUE, op = "=~")

fit2 <- update(fit, add = "visual =~ x9")
fit3 <- update(fit, add = "textual =~ x3\nvisual =~ x7")

fit_cfi <- sapply(list(fit, fit2, fit3), fitMeasures, fit.measures = "cfi")
fit_tli <- sapply(list(fit, fit2, fit3), fitMeasures, fit.measures = "tli")
fit_rmsea <- sapply(list(fit, fit2, fit3), fitMeasures, fit.measures = "rmsea")

plot_models_fm(fit, fit2, fit3)
plot_models_fm(fit, fit2, fit3, fit_values = c(.90, .925, .95, fit_cfi))
plot_models_fm(fit, fit2, fit3, include_model_values = TRUE)
plot_models_fm(list(A = fit, B = fit2, C = fit3), fit_values = c(.90, .925, .95))
plot_models_fm(fit, fit2, fit3, fit_measure = "tli")
plot_models_fm(fit, fit2, fit3, fit_measure = "tli", fit_values = c(.90, .925, .95, fit_tli))
plot_models_fm(fit, fit2, fit3, fit_measure = "tli", include_model_values = TRUE)
plot_models_fm(fit, fit2, fit3, fit_measure = "rmsea")
plot_models_fm(fit, fit2, fit3, fit_measure = "rmsea", include_model_values = TRUE)


