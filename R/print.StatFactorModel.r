#' print StatFactorModel object
#' 
#' Generic function of print method for fitStatFactorModel.
#' 
#' 
#' @param x fit object created by fitStatisticalFactorModel.
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
#' @method print StatFactorModel
#' @export
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
  cat("\nRegression alphas:\n")
  print(x$alpha , digits = digits, ...)
  cat("\nFactor Loadings:\n")
  print(x$loadings, digits = digits, ...)
  cat("\nRegression R-squared:\n")
  print(x$r2, digits = digits, ...)
  cat("\nResidual Variance:\n")
  print(x$resid.variance, digits = digits, ...)

}
