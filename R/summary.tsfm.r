#' @title Summarizing a fitted time series factor model
#' 
#' @description S3 \code{summary} method for object of class \code{tsfm}. 
#' Resulting object is of class {summary.tsfm}.
#'  
#' @param object an object of class \code{tsfm} produced by \code{fitTSFM}.
#' @param digits an integer value, to indicate the required number of 
#' significant digits. Default is 3.
#' @param ... optional arguments passed to the \code{print} method.
#' 
#' @return Returns an object of class {summary.tsfm}.
#' 
#' @author Yi-An Chen & Sangeetha Srinivasan.
#' 
#' @seealso \code{\link{fitTSFM}}
#' 
#' @examples
#' data(managers.df)
#' fit <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                factor.names=colnames(managers.df[,7:9]), 
#'                market.name="SP500.TR",
#'                data=data, fit.method="OLS", variable.selection="none", 
#'                add.up.market=TRUE, add.market.sqd=TRUE)
#' summary(fit)
#' 
#' @method summary tsfm
#' @export

summary.tsfm <- function(object, digits=3, ...){
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
    colnames(table.macro)[1] <- "Intercept"
    print(table.macro,digits = digits,...)
    cat("\nR-squared =", object$r2[i] ,",Residual Volatility ="
        , object$resid.sd[i],"\n")
  }
  
}

