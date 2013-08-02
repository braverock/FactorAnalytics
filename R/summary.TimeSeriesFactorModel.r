#' summary method for TimeSeriesModel object.
#' 
#' Generic function of summary method for fitTimeSeriesFactorModel. 
#' 
#' 
#' @param object An object created by fitTimeSeiresFactorModel.
#' @param digits Integer indicating the number of decimal places. Default is 3.
#' @param ... other option used in \code{summary.lm}
#' @author Yi-An Chen.
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' # fit the factor model with OLS
#' fit <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                data=managers.df,fit.method="OLS")
#' summary(fit)
#' @method summary TimeSeriesFactorModel
#' @export
#' 
summary.TimeSeriesFactorModel <- function(object,digits=3){
  if(!is.null(cl <- object$call)) {
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nFactor Betas\n")
  n <- length(object$assets.names)
  for (i in 1:n) {
  options(digits = digits)  
  cat("\n", object$assets.names[i], "\n")  
  table.macro <- t(summary(object$asset.fit[[i]])$coefficients)
  colnames(table.macro)[1] <- "alpha"
  print(table.macro,digits = digits)
  cat("\nR-square =", object$r2[i] ,",residual variance ="
      , object$resid.variance[i],"\n")
  }

}
    
