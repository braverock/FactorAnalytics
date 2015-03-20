#' @title Prints out a fitted up and down market time series factor model object
#' 
#' @description S3 \code{print} method for object of class \code{tsfmUpDn}. Prints 
#' the call, factor model dimension, regression coefficients, r-squared and 
#' residual volatilities from the fitted object. 
#' 
#' @param x an object of class \code{tsfmUpDn} produced by \code{fitTsfmUpDn}.
#' @param digits an integer value, to indicate the required number of 
#' significant digits. Default is 3.
#' @param ... optional arguments passed to the \code{print} method.
#'    
#' @author Yi-An Chen and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitTsfmUpDn}}, \code{\link{summary.tsfmUpDn}}
#' 
#' @examples
#' data(managers)
#' # example: Up and down market factor model with LS fit
#' fitUpDn <- fitTsfmUpDn(asset.names=colnames(managers[,(1:6)]),mkt.name="SP500.TR",
#'                        data=managers, fit.method="LS",control=NULL)
#'  
#' print(fitUpDn)
#' 
#' @method print tsfmUpDn
#' @export
#' 



print.tsfmUpDn <- function(x, digits=max(3, .Options$digits - 3), ...){
  if(!is.null(cl <- x$Up$call)){
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nModel dimensions:\n")
  tmp <- c(dim(t(x$Up$beta)), nrow(x$Up$data))
  names(tmp) <- c("Factors", "Assets", "Periods")
  print(tmp)
  cat("\nRegression Alphas in up market:\n")
  print(t(x$Up$alpha), digits=digits,...)
  cat("\nRegression Alphas in down market:\n")
  print(t(x$Dn$alpha), digits=digits,...)
  
  cat("\nFactor Betas in up market:\n")
  B <- as.matrix(t(x$Up$beta))
  if (x$Up$variable.selection=="lars") { B[B==0] <- NA }
  print(B, digits=digits, na.print=".", ...)
  cat("\nFactor Betas in down market:\n")
  B <- as.matrix(t(x$Dn$beta))
  if (x$Dn$variable.selection=="lars") { B[B==0] <- NA }
  print(B, digits=digits, na.print=".", ...)
  
  cat("\nR-squared values in up market:\n")
  print(x$Up$r2, digits=digits, ...)
  cat("\nR-squared values in down market:\n")
  print(x$Dn$r2, digits=digits, ...)
  
  cat("\nResidual Volatilities in up market:\n")
  print(x$Up$resid.sd, digits=digits, ...)
  cat("\nResidual Volatilities in down market:\n")
  print(x$Dn$resid.sd, digits=digits, ...)
}