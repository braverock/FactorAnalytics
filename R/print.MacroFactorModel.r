print.MacroFactorModel <-
  function(fit.macro) {
    lapply(fit.macro[[1]], print)
  }