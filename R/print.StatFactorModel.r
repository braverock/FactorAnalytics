print.StatFactorModel <-
function(fit.stat, digits = max(3, .Options$digits - 3), ...)
{
  if(!is.null(cl <- fit.stat$call)) {
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nFactor Model:\n")
  tmp <- c(dim(fit.stat$loadings), nrow(fit.stat$factors))
  names(tmp) <- c("Factors", "Variables", "Periods")
  print(tmp)
  cat("\nFactor Loadings:\n")
  print(fit.stat$loadings, digits = digits, ...)
  cat("\nRegression R-squared:\n")
  print(fit.stat$r2, digits = digits, ...)
}
