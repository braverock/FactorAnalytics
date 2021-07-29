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
#' only if \code{fit.method = "LS" or "DLS"}.
#' 
#' Standard errors are currently not available for 
#' \code{variable.selection="lars"} as there seems to be no consensus on a 
#' statistically valid method of calculating standard errors for the lasso 
#' predictions.
#'  
#' @param object an object of class \code{tsfm} returned by \code{fitTsfm}.
#' @param se.type one of "Default", "HC" or "HAC"; option for computing 
#' HC/HAC standard errors and t-statistics. Default is "Default".
#' @param x an object of class \code{summary.tsfm}.
#' @param digits number of significants digits to use when printing. 
#' Default is 3.
#' @param labels option to print labels and legend in the summary. Default is 
#' \code{TRUE}. When \code{FALSE}, only the coefficient matrx with standard 
#' errors is printed. 
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
#' \item{sum.list}{list of summaries of the N fit objects (of class \code{lm}, 
#' \code{lmRob} or \code{lars}) for each asset in the factor model.}
#' 
#' @author Sangeetha Srinivasan & Yi-An Chen.
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link[stats]{summary.lm}}
#' 
#' @examples
#' data(managers, package = 'PerformanceAnalytics')
#' fit <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                factor.names=colnames(managers[,7:9]), 
#'                data=managers)
#' 
#' # summary of factor model fit for all assets
#' summary(fit, "HAC")
#' 
#' # summary of lm fit for a single asset
#' summary(fit$asset.fit[[1]])
#' 
#' @method summary tsfm
#' @export

summary.tsfm <- function(object, se.type=c("Default","HC","HAC"), ...){
  
  # check input object validity
  if (!inherits(object, "tsfm")) {
    stop("Invalid 'tsfm' object")
  }
  
  #set default for se.type
  se.type = se.type[1]
  
  # note: fit.method=NULL for "lars" objects
  if (object$fit.method=="Robust" && se.type!="Default") {
    stop("Invalid argument: HC/HAC standard errors are applicable only if 
         fit.method = 'LS' or 'DLS'")
  }
  
  # extract summary.lm objects for each asset
  sum.list <- lapply(object$asset.fit, summary)
  
  # convert to HC/HAC standard errors and t-stats if specified
  # extract coefficients separately for "lars" variable.selection method
  for (i in object$asset.names) {
    if (se.type=="HC") {
      sum.list[[i]]$coefficients <- coeftest.default(object$asset.fit[[i]],
                                                     vcov.=vcovHC.default)[,1:4]
    } else if (se.type=="HAC") {
      sum.list[[i]]$coefficients <- coeftest.default(object$asset.fit[[i]], 
                                                     vcov.=vcovHAC.default)[,1:4]
    }
  }
  
  if (object$variable.selection=="lars") {
    sum.list <- list()
    for (i in object$asset.names) {
      sum.list[[i]]$coefficients <- as.matrix(c(object$alpha[i], object$beta[i,]))
      rownames(sum.list[[i]]$coefficients)[1]="(Intercept)"
      colnames(sum.list[[i]]$coefficients)[1]="Estimate"
      sum.list[[i]]$r.squared <- as.numeric(object$r2[i])
      sum.list[[i]]$sigma <- as.numeric(object$resid.sd[i]) 
    }
  }
  
  # include the call and se.type to fitTsfm
  sum <- list(call=object$call, se.type=se.type, sum.list=sum.list)
  class(sum) <- "summary.tsfm"
  return(sum)
}


#' @rdname summary.tsfm
#' @method print summary.tsfm
#' @export

print.summary.tsfm <- function(x, digits=3, labels=TRUE, ...) {
  n <- length(x$sum.list)
  if (labels==TRUE) {
    if(!is.null(cl <- x$call)) {
      cat("\nCall:\n")
      dput(cl)
    }
    cat("\nFactor Model Coefficients:\n", sep="")
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
  } else {
    for (i in 1:n) {
      options(digits = digits) 
      table.coef <- (x$sum.list)[[i]]$coefficients
      cat(names(x$sum.list[i]), "\n")
      printCoefmat(table.coef, digits=digits, signif.legend=FALSE, ...)
      cat("\n")
    }
  }
}
