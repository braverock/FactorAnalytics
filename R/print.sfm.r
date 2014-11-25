#' @title Prints out a fitted statictical factor model object
#' 
#' @description S3 \code{print} method for object of class \code{sfm}. Prints 
#' the call, factor model dimension, factor loadings, r-squared and residual 
#' volatilities from the fitted object. 
#' 
#' @param x an object of class \code{sfm} produced by \code{fitSfm}.
#' @param digits an integer value, to indicate the required number of 
#' significant digits. Default is 3.
#' @param ... optional arguments passed to the \code{print} method.
#'    
#' @author Yi-An Chen and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitSfm}}, \code{\link{summary.sfm}}
#' 
#' @examples
#' data(stat.fm.data)
#' fit <- fitSfm(sfm.dat, k=2)
#' print(fit)
#' 
#' @method print sfm
#' @export
#' 

print.sfm <- function(x, digits=max(3, .Options$digits - 3), ...){
  if(!is.null(cl <- x$call)){
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nModel dimensions:\n")
  tmp <- c(dim(t(x$loadings)), nrow(x$data))
  names(tmp) <- c("Factors", "Assets", "Periods")
  print(tmp)
  cat("\nFactor Loadings:\n")
  print(summary(x$loadings), digits=digits, ...)
  cat("\nR-squared values:\n")
  print(summary(x$r2), digits=digits, ...)
  cat("\nResidual Volatilities:\n")
  print(summary(x$resid.sd), digits=digits, ...)
}
