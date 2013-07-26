#' summary method for TimeSeriesModel object.
#' 
#' Generic function of summary method for fitTimeSeriesFactorModel. 
#' 
#' 
#' @param fit.macro fit.macro object created by fitTimeSeiresFactorModel.
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
#' 
#' @export
#' 
summary.TimeSeriesFactorModel <- function(fit.macro,digits=3){
  if(!is.null(cl <- fit.macro$call)) {
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nFactor Betas\n")
  n <- length(fit.macro$assets.names)
  for (i in 1:n) {
  options(digits = digits)  
  cat("\n", fit.macro$assets.names[i], "\n")  
  table.macro <- t(summary(fit.macro$asset.fit[[i]])$coefficients)
  colnames(table.macro)[1] <- "alpha"
  print(table.macro,digits = digits)
  cat("\nR-square =", fit.macro$r2[i] ,",residual variance ="
      , fit.macro$resid.variance[i],"\n")
  }
}
    
