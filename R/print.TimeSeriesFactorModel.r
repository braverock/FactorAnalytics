#' print TimeSeriesfactorModel object
#' 
#' Generic function of print method for fitTimeSeriesFactorModel.
#' 
#' 
#' @param x fit object created by fitTimeSeriesFactorModel.
#' @param digits integer indicating the number of decimal places. Default is 3.
#' @param ... arguments to be passed to print method.   
#' @author Yi-An Chen.
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' fit.macro <- fitTimeseriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                       factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                       data=managers.df,fit.method="OLS")
#' print(fit.macro)
#' @method print TimeSeriesFactorModel
#' @export
print.TimeSeriesFactorModel <- function(x,digits=max(3, .Options$digits - 3),...){
  if(!is.null(cl <- x$call)) {
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nFactor Model:\n")
  tmp <- c(dim(t(x$beta)), nrow(x$data))
  names(tmp) <- c("Factors", "Variables", "Periods")
  print(tmp)
  cat("\nRegression alphas:\n")
  print(x$alpha , digits = digits, ...)
  cat("\nFactor Betas:\n")
  print(t(x$beta), digits = digits, ...)
  cat("\nRegression R-squared:\n")
  print(x$r2, digits = digits, ...)
  cat("\nResidual Variance:\n")
  print(x$resid.variance, digits = digits, ...)

}
