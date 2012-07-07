summary.MacroFactorModel <- 
  function(fit.macro){
     lapply(fit.macro[[1]], summary)
  }
    