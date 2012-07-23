print.StatFactorModel <-
function(x, digits = max(3, .Options$digits - 3), ...)
{
  if(!is.null(cl <- x$call)) {
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nFactor Model:\n")
  tmp <- c(dim(x$loadings), nrow(x$factors))
  names(tmp) <- c("Factors", "Variables", "Periods")
  print(tmp)
  cat("\nFactor Loadings:\n")
  print(x$loadings, digits = digits, ...)
  cat("\nRegression R-squared:\n")
  print(x$r2, digits = digits, ...)
}
