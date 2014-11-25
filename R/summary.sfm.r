#' @title Summarizing a fitted time series factor model
#' 
#' @description \code{summary} method for object of class \code{sfm}. 
#' Returned object is of class {summary.sfm}.
#' 
#' @details The default \code{summary} method for a fitted \code{lm} object 
#' computes the standard errors and t-statistics under the assumption of 
#' homoskedasticty. Argument \code{se.type} gives the option to compute 
#' heteroskedasticity-consistent (HC) standard errors and t-statistics using 
#' \code{\link[lmtest]{coeftest}}.
#'  
#' @param object an object of class \code{sfm} returned by \code{fitSfm}.
#' @param se.type one of "Default" or "HC"; option for computing HC standard 
#' errors and t-statistics.
#' @param x an object of class \code{summary.sfm}.
#' @param digits number of significants digits to use when printing. 
#' Default is 3.
#' @param ... futher arguments passed to or from other methods.
#' 
#' @return Returns an object of class \code{summary.sfm}. 
#' The print method for class \code{summary.sfm} outputs the call, 
#' coefficients (with standard errors and t-statistics), r-squared and 
#' residual volatilty (under the homoskedasticity assumption) for all assets. 
#' 
#' Object of class \code{summary.sfm} is a list of length N+2 containing:
#' \item{call}{the function call to \code{fitSfm}}
#' \item{se.type}{standard error type as input} 
#' \item{}{summary of the fit object of class \code{mlm} for the factor model.}
#' 
#' @note For a more detailed printed summary for each asset, refer to 
#' \code{\link[stats]{summary.lm}}, which includes F-statistics, 
#' Multiple R-squared, Adjusted R-squared, further formats the coefficients, 
#' standard errors, etc. and additionally gives significance stars if 
#' \code{signif.stars} is TRUE. 
#' 
#' @author Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitSfm}}, \code{\link[stats]{summary.lm}}
#' 
#' @examples
#' data(stat.fm.data)
#' # fit the factor model with PCA
#' fit <- fitSfm(sfm.dat, k=2)
#' 
#' # summary of factor model fit for all assets
#' summary(fit, "HAC")
#' 
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovHC
#' 
#' @method summary sfm
#' @export

summary.sfm <- function(object, se.type="Default", ...){
  
  # check input object validity
  if (!inherits(object, "sfm")) {
    stop("Invalid 'sfm' object")
  }
  
  # extract list of mlm summary object for the entire model
  mlm.fit.summary <- summary(object$asset.fit)
  
  # get coefficients and convert to HC standard errors and t-stats if specified
  coefficients <-  coeftest(object$asset.fit, vcov.=vcovHC, data=sfm.data[,1])
  if (se.type=="HC") {
    coefficients <- coeftest(object$asset.fit, vcov.=vcovHC)
  }
  
  # include the call and se.type to fitSfm
  sum <- list(call=object$call, se.type=se.type, coefficients=coefficients, 
              mlm.fit.summary=mlm.fit.summary, r.squared=object$r2, 
              sigma=object$resid.sd)
  class(sum) <- "summary.sfm"
  return(sum)
}

#' @rdname summary.sfm
#' @method print summary.sfm
#' @export

print.summary.sfm <- function(x, digits=3, ...) {
  
  if(!is.null(cl <- x$call)) {
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nFactor Model Coefficients:", "\n(", x$se.type, 
      " Standard Errors & T-stats)\n\n", sep="")
  c <- x$coefficients
  print(c, digits=digits, ...)
  r2 <- x$r.squared
  print(r2, digits=digits, ...)
  sig <- x$sigma
  print(sig, digits=digits, ...)
}
