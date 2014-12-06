#' @title Summarizing a fitted time series factor model
#' 
#' @description \code{summary} method for object of class \code{sfm}. 
#' Returned object is of class {summary.sfm}.
#' 
#' @details The default \code{summary} method for a fitted \code{lm} object 
#' computes the standard errors and t-statistics under the assumption of 
#' homoskedasticty. Argument \code{se.type} gives the option to compute 
#' heteroskedasticity-consistent (HC) or 
#' heteroskedasticity-autocorrelation-consistent (HAC) standard errors and 
#' t-statistics using \code{\link[lmtest]{coeftest}}.
#'  
#' @param object an object of class \code{sfm} returned by \code{fitSfm}.
#' @param se.type one of "Default", "HC" or "HAC"; option for computing HC/HAC 
#' standard errors and t-statistics.
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
#' \item{sum.list}{list of summaries for the N fit objects of class \code{lm} 
#' for each asset in the factor model.}
#' \item{mimic}{N x K matrix of factor mimicking portfolio weights.}
#' 
#' @author Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitSfm}}, \code{\link[stats]{summary.lm}}
#' 
#' @examples
#' data(StockReturns)
#' # fit the factor model with PCA
#' fit <- fitSfm(r.M, k=2)
#' 
#' # summary of factor model fit for all assets
#' summary(fit, "HAC")
#' 
#' @importFrom lmtest coeftest.default
#' @importFrom sandwich vcovHC.default vcovHAC.default
#' 
#' @method summary sfm
#' @export

summary.sfm <- function(object, se.type="Default", ...){
  
  # check input object validity
  if (!inherits(object, "sfm")) {
    stop("Invalid 'sfm' object")
  }
  
  # extract list of summary.lm objects for all assets
  sum.list <- summary(object$asset.fit)
  names(sum.list) <- object$asset.names
  
  # convert to HC standard errors and t-stats if specified
  if (se.type=="HC" || se.type=="HAC") {
    for (i in object$asset.names) {
      # need to get lm objects because "mlm" method doesn't exist for vcovHAC
      f <- lm(as.numeric(object$data[,i]) ~ object$factors)
      if (se.type=="HC") {
        sum.list[[i]]$coefficients <- coeftest.default(f, vcov.=vcovHC.default)[,1:4]
      } else if (se.type=="HAC") {
        sum.list[[i]]$coefficients <- coeftest.default(f, vcov.=vcovHAC.default)[,1:4]
      }
    }
  }
  
  # include the call and se.type to fitSfm
  sum <- list(call=object$call, se.type=se.type, sum.list=sum.list, 
              mimic=object$mimic)
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
  cat("\nFactor Model Coefficients:\n", sep="")
  n <- length(x$sum.list)
  for (i in 1:n) {
    options(digits = digits)  
    table.coef <- (x$sum.list)[[i]]$coefficients
    if (dim(table.coef)[2] > 1) {
      cat("\nAsset", i, ": ", names(x$sum.list[i]), "\n(", x$se.type, 
          " Standard Errors & T-stats)\n\n", sep="")  
    } else {
      cat("\nAsset", i, ": ", names(x$sum.list[i]), "\n\n", sep="")  
    }
    r2 <- x$sum.list[[i]]$r.squared
    sigma <- x$sum.list[[i]]$sigma
    printCoefmat(table.coef, digits=digits, ...)
    cat("\nR-squared: ", r2,", Residual Volatility: ", sigma,"\n", sep="")
  }
}
