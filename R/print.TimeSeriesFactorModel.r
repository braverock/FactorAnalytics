#' print TimeSeriesfactorModel object
#' 
#' Generic function of print method for fitTimeSeriesFactorModel.
#' 
#' 
#' @param fit.macro fit object created by fitTimeSeriesFactorModel.
#' @param digits. integer indicating the number of decimal places. Default is 3.
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
#' 
#' @export
print.TimeSeriesFactorModel <- function(fit.macro,digits=max(3, .Options$digits - 3),...){
  if(!is.null(cl <- fit.macro$call)) {
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nFactor Model:\n")
  tmp <- c(dim(t(fit.macro$beta)), nrow(fit.macro$data))
  names(tmp) <- c("Factors", "Variables", "Periods")
  print(tmp)
  cat("\nRegression alphas:\n")
  print(fit.macro$alpha , digits = digits, ...)
  cat("\nFactor Betas:\n")
  print(t(fit.macro$beta), digits = digits, ...)
  cat("\nRegression R-squared:\n")
  print(fit.macro$r2, digits = digits, ...)
  cat("\nResidual Variance:\n")
  print(fit.macro$resid.variance, digits = digits, ...)

}
