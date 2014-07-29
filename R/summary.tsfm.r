#' @title Summarizing a fitted time series factor model
#' 
#' @description \code{summary} method for object of class \code{tsfm}. 
#' Returned object is of class {summary.tsfm}.
#' 
#' @details The default \code{summary} method for a fitted \code{lm} object 
#' computes the standard errors and t-statistics under the assumption of 
#' homoskedasticty. Argument \code{se.type} gives the option to compute 
#' heteroskedasticity-consistent (HC) or 
#' heteroskedasticity-autocorrelation-consistent (HAC) standard errors and 
#' t-statistics using \code{\link[lmtest]{coeftest}}. This option is meaningful 
#' only if \code{fit.method = "OLS" or "DLS"}. This option is currently not 
#' available for \code{variable.selection = "lar" or "lasso"}.
#'  
#' @param object an object of class \code{tsfm} returned by \code{fitTsfm}.
#' @param se.type one of "Default", "HC" or "HAC"; option for computing 
#' HC/HAC standard errors and t-statistics.
#' @param x an object of class \code{summary.tsfm}.
#' @param digits number of significants digits to use when printing. 
#' Default is 3.
#' @param ... futher arguments passed to or from other methods.
#' 
#' @return Returns an object of class \code{summary.tsfm}. 
#' The print method for class \code{summary.tsfm} outputs the call, 
#' coefficients (with standard errors and t-statistics), r-squared and 
#' residual volatilty (under the homoskedasticity assumption) for all assets. 
#' 
#' Object of class \code{summary.tsfm} is a list of length N + 2 containing:
#' \item{call}{the function call to \code{fitTsfm}}
#' \item{se.type}{standard error type as input} 
#' \item{}{summaries of the N fit objects (of class \code{lm}, \code{lmRob} 
#' or \code{lars}) for each asset in the factor model.}
#' 
#' @note For a more detailed printed summary for each asset, refer to 
#' \code{\link[stats]{summary.lm}} or \code{\link[robust]{lmRob}}, which 
#' include F-statistics, Multiple R-squared, Adjusted R-squared and further 
#' format the coefficients, standard errors, etc. and additionally give 
#' significance stars if \code{signif.stars} is TRUE. 
#' 
#' @author Yi-An Chen & Sangeetha Srinivasan.
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link[stats]{summary.lm}}
#' 
#' @examples
#' data(managers)
#' fit <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                factor.names=colnames(managers[,7:9]), 
#'                mkt.name="SP500 TR", mkt.timing="HM", data=managers)
#' 
#' # summary of factor model fit for all assets
#' summary(fit, "HAC")
#' 
#' # summary of lm fit for a single asset
#' summary(fit$asset.fit[[1]])
#' 
#' @method summary tsfm
#' @export

summary.tsfm <- function(object, se.type="Default", ...){
  # check input object validity
  if (!inherits(object, "tsfm")) {
    stop("Invalid 'tsfm' object")
  }
  if (!(object$fit.method %in% c("OLS","DLS")) && se.type!="Default") {
    stop("Invalid argument: HC/HAC standard errors are applicable only if 
         fit.method = 'OLS' or 'DLS'")
  }
  
  # extract summary.lm objects for each asset
  sum <- lapply(object$asset.fit, summary)
  
  # convert to HC/HAC standard errors and t-stats if specified
  # extract coefficients separately for "lars" variable.selection method
  for (i in object$asset.names) {
    if (se.type == "HC") {
      sum[[i]]$coefficients <- coeftest(object$asset.fit[[i]], vcovHC)[,1:4]
    } else if (se.type == "HAC") {
      sum[[i]]$coefficients <- coeftest(object$asset.fit[[i]], vcovHAC)[,1:4]
    }
  }
  
  if (object$variable.selection=="lars") {
    sum <- list()
    for (i in object$asset.names) {
      sum[[i]]$coefficients <- as.matrix(c(object$alpha[i], object$beta[i,]))
      rownames(sum[[i]]$coefficients)[1]="(Intercept)"
      colnames(sum[[i]]$coefficients)[1]="Estimate"
      sum[[i]]$r.squared <- as.numeric(object$r2[i])
      sum[[i]]$sigma <- as.numeric(object$resid.sd[i]) 
    }
  }
  
  # include the call and se.type to fitTsfm
  sum <- c(list(call=object$call, Type=se.type), sum)
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
  cat("\nFactor Model Coefficients:\n", 
      sep="")
  n <- length(x)
  for (i in 3:n) {
    options(digits = digits)  
    cat("\nAsset", i-2, ": ", names(x[i]), "\n(",x$Type,
        " Standard Errors & T-stats)\n\n", sep = "")  
    table.coef <- x[[i]]$coefficients
    print(table.coef, digits = digits, ...)
    cat("\nR-squared: ", x[[i]]$r.squared,", Residual Volatility: "
        , x[[i]]$sigma,"\n", sep = "")
  }
}
