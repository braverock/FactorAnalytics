#' @title Prints out a fitted time series factor model object
#' 
#' @description S3 \code{print} method for object of class \code{tsfm}. Prints 
#' the call, factor model dimension, regression coefficients, r-squared and 
#' residual volatilities from the fitted object. 
#' 
#' @param x an object of class \code{tsfm} produced by \code{fitTsfm}.
#' @param digits an integer value, to indicate the required number of 
#' significant digits. Default is 3.
#' @param ... optional arguments passed to the \code{print} method.
#'    
#' @author Yi-An Chen and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{summary.tsfm}}
#' 
#' @examples
#' data(managers)
#' fit <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                factor.names=colnames(managers[,7:9]), 
#'                mkt.name="SP500.TR", data=managers)
#' print(fit)
#' 
#' @method print tsfm
#' @export
#' 

print.tsfm <- function(x, digits=max(3, .Options$digits - 3), ...){
  if(!is.null(cl <- x$call)){
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nModel dimensions:\n")
  tmp <- c(dim(t(x$beta)), nrow(x$data))
  names(tmp) <- c("Factors", "Assets", "Periods")
  print(tmp)
  cat("\nRegression Alphas:\n")
  print(t(x$alpha), digits=digits, ...)
  cat("\nFactor Betas:\n")
  B <- as.matrix(t(x$beta))
  if (x$variable.selection=="lars") { B[B==0] <- NA }
  print(B, digits=digits, na.print=".", ...)
  cat("\nR-squared values:\n")
  print(x$r2, digits=digits, ...)
  cat("\nResidual Volatilities:\n")
  print(x$resid.sd, digits=digits, ...)
}
