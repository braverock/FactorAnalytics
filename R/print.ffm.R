#' @title Prints a fitted fundamental factor model
#' 
#' @description S3 \code{print} method for object of class \code{ffm}. Prints 
#' the call, factor model dimension and summary statistics for the estimated
#' factor returns, cross-sectional r-squared values and residual variances 
#' from the fitted object. 
#' 
#' Refer to \code{\link{summary.ffm}} for a more detailed summary of the fit at 
#' each time period.
#' 
#' @param x an object of class \code{ffm} produced by \code{fitFfm}.
#' @param digits an integer value, to indicate the required number of 
#' significant digits. Default is 3.
#' @param ... optional arguments passed to the \code{print} method.
#'    
#' @author Yi-An Chen and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitFfm}}, \code{\link{summary.ffm}}
#' 
#' @examples
#' data("factorDataSetDjia5Yrs")
#' # fit a fundamental factor model
#' exposure.vars <- c("P2B", "MKTCAP")
#' fit.style.sector <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", 
#'                            ret.var="RETURN", date.var="DATE", 
#'                            exposure.vars=exposure.vars)
#' print(fit)
#' 
#' @method print ffm
#' @export
#' 

print.ffm <- function(x, digits=max(3, .Options$digits - 3), ...){
  if(!is.null(cl <- x$call)){
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nModel dimensions:\n")
  tmp <- c(dim(t(x$beta)), length(x$time.periods))
  names(tmp) <- c("Factors", "Assets", "Periods")
  print(tmp)
  cat("\nFactor returns across periods:\n")
  print(summary(coredata(x$factor.returns)), digits=digits, ...)
  cat("\nR-squared values across periods:\n")
  print(summary(x$r2), digits=digits, ...)
  cat("\nResidual Variances across assets:\n")
  print(summary(x$resid.var), digits=digits, ...)
}
