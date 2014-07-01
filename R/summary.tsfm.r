#' @title Summarizing a fitted time series factor model
#' 
#' @description S3 \code{summary} method for object of class \code{tsfm}. 
#' Resulting object is of class {summary.tsfm}. There is a generic 
#' \code{print} method for this object.
#'  
#' @param object an object of class \code{tsfm} produced by \code{fitTSFM}.
#' @param ... futher arguments passed to or from other methods.
#' 
#' @return Returns an object of class \code{summary.tsfm}, which is a list
#' containing the function call to \code{fitTSFM} and the 
#' \code{summary.lm} objects fitted for each asset in the factor model. 
#' The print method for class \code{summary.tsfm} outputs the call, coefficients, 
#' r-squared and residual volatilty for all assets.
#' 
#' @note For a more detailed printed summary for each asset, refer to 
#' \code{print.summary.lm}, which tries to be smart about formatting the 
#' coefficients, standard errors, etc. and additionally gives significance 
#' stars if \code{signif.stars} is TRUE. 
#' 
#' @author Yi-An Chen & Sangeetha Srinivasan.
#' 
#' @seealso \code{\link{fitTSFM}}
#' 
#' @examples
#' data(managers.df)
#' fit <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                factor.names=colnames(managers.df[,7:9]), 
#'                market.name="SP500.TR", data=managers.df, 
#'                fit.method="OLS", variable.selection="none", 
#'                add.up.market=TRUE, add.market.sqd=TRUE)
#' summary(fit)
#' 
#' @method summary tsfm
#' @method print summary.tsfm
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

# summary.tsfm <- function(object, ...){
#   # check input object validity
#   if (!inherits(object, "tsfm")) {
#     stop("Invalid 'tsfm' object")
#   }
#   n <- length(object$asset.names)
#   asset.summary <- list()
#   
#   for (i in 1:n) {
#     coeff <- t(summary(object$asset.fit[[i]])$coefficients)
#     R2 <- object$r2[i]
#     SD <- object$resid.sd[i]
#     asset.summary[[i]] <- list(coeff, R2, SD)
#     names(asset.summary) <- ("Coefficients", "R.squared", "Residual.Volatility")
#   }
#   
#   out <- c(Call=object$call, 
#            Coefficients=coeff, 
#            R.squared=R2,
#            Residual.Volatility=SD)
#   class(out) <- "summary.tsfm"
#   return(out)
# }
