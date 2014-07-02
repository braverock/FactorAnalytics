#' @title Summarizing a fitted time series factor model
#' 
#' @description \code{summary} method for object of class \code{tsfm}. 
#' Returned object is of class {summary.tsfm}.
#'  
#' @param object an object of class \code{tsfm} returned by \code{fitTSFM}.
#' @param x an object of class \code{summary.tsfm}.
#' @param digits number of significants digits to use when printing. 
#' Default is 3.
#' @param ... futher arguments passed to or from other methods.
#' 
#' @return Returns an object of class \code{summary.tsfm}, which is a list
#' containing the function call to \code{fitTSFM} and the 
#' \code{summary.lm} objects fitted for each asset in the factor model. 
#' The print method for class \code{summary.tsfm} outputs the call, 
#' coefficients, r-squared and residual volatilty for all assets.
#' 
#' @note For a more detailed printed summary for each asset, refer to 
#' \code{print.summary.lm}, which further formats the coefficients, 
#' standard errors, etc. and additionally gives significance 
#' stars if \code{signif.stars} is TRUE. 
#' 
#' @author Yi-An Chen & Sangeetha Srinivasan.
#' 
#' @seealso \code{\link{fitTSFM}}, \code{\link[stats]{summary.lm}}
#' 
#' @examples
#' data(managers.df)
#' fit <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                factor.names=colnames(managers.df[,7:9]), 
#'                market.name="SP500.TR", data=managers.df, 
#'                fit.method="OLS", variable.selection="none", 
#'                add.up.market=TRUE, add.market.sqd=TRUE)
#' # summary of factor model fit for all assets
#' summary(fit)
#' 
#' # summary of lm fit for a single asset
#' summary(fit$asset.fit[[1]])
#' 
#' @method summary tsfm
#' @export

summary.tsfm <- function(object, ...){
  # check input object validity
  if (!inherits(object, "tsfm")) {
    stop("Invalid 'tsfm' object")
  }
  # extract summary.lm objects for each asset
  sum <- lapply(object$asset.fit, summary)
  # include the call to fitTSFM
  sum <- c(call=object$call, sum)
  class(sum) <- "summary.tsfm"
  return(sum)
}


#' @rdname summary.tsfm
#' @method print summary.tsfm
#' @export

print.summary.tsfm <- function(x, digits=3, ...) {
  if(!is.null(cl <- x$call)) {
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nFactor Model Coefficients:\n")
  n <- length(x)
  for (i in 2:n) {
    options(digits = digits)  
    cat("\nAsset", i-1, ": ", names(x[i]), "\n", sep = "")  
    table.coef <- t(x[[i]]$coefficients)
    print(table.coef, digits = digits, ...)
    cat("\nR-squared: ", x[[i]]$r.squared,", Residual Volatility: "
        , x[[i]]$sigma,"\n", sep = "")
  }
}
