#' print StatFactorModel object
#' 
#' Generic function of print method for fitStatFactorModel.
#' 
#' 
#' @param fit.stat fit object created by fitMacroeconomicFactorModel.
#' @param digits integer indicating the number of decimal places. Default is 3.
#' @param ...  Other arguments for print methods.
#' @author Eric Zivot and Yi-An Chen.
#' @examples
#' 
#' # load data for fitStatisticalFactorModel.r
#' # data from finmetric berndt.dat and folio.dat
#' 
#' data(stat.fm.data)
#' # pca
#' sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat,k=10)
#' print(sfm.pca.fit)
#' 
#' 
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
  cat("\nRegression alphas:\n")
  print(fit.stat$alpha , digits = digits, ...)
  cat("\nFactor Loadings:\n")
  print(fit.stat$loadings, digits = digits, ...)
  cat("\nRegression R-squared:\n")
  print(fit.stat$r2, digits = digits, ...)
  cat("\nResidual Variance:\n")
  print(fit.stat$resid.variance, digits = digits, ...)
}
