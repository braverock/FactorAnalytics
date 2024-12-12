#' @title Summarizing a fitted fundamental factor model
#'
#' @description \code{summary} method for object of class \code{ffm}.
#' Returned object is of class \code{summary.ffm}.
#'
#' @details The default \code{summary} method for a fitted \code{lm} object
#' computes the standard errors and t-statistics under the assumption of
#' homoskedasticty.
#'
#' Note: This gives a summary of the fited factor returns at each time period.
#' If \code{T} is large, you might prefer the more succint summary produced by
#' \code{\link{print.ffm}}.
#'
#' @param object an object of class \code{ffm} returned by \code{fitFfm}.
#' @param x an object of class \code{summary.ffm}.
#' @param digits number of significants digits to use when printing.
#' Default is 3.
#' @param labels option to print labels and legend in the summary. Default is
#' \code{TRUE}. When \code{FALSE}, only the coefficient matrx with standard
#' errors is printed.
#' @param ... futher arguments passed to or from other methods.
#'
#' @return Returns an object of class \code{summary.ffm}.
#' The print method for class \code{summary.ffm} outputs the call,
#' coefficients (with standard errors and t-statistics), r-squared and
#' residual volatilty (under the homoskedasticity assumption) for all assets.
#'
#' Object of class \code{summary.ffm} is a list of length N + 2 containing:
#' \item{call}{the function call to \code{fitFfm}}
#' \item{sum.list}{list of summaries of the T fit objects (of class \code{lm} or
#' \code{lmRob}) for each time period in the factor model.}
#'
#' @author Sangeetha Srinivasan & Yi-An Chen.
#'
#' @seealso \code{\link{fitFfm}}, \code{\link[stats]{summary.lm}}
#'
#' @examples
#' data("factorDataSetDjia5Yrs")
#'
#' # fit a fundamental factor model
#' fit <- fitFfm(data = factorDataSetDjia5Yrs,
#'               asset.var = "TICKER",
#'               ret.var = "RETURN",
#'               date.var = "DATE",
#'               exposure.vars = c("P2B", "MKTCAP"))
#'
#' names(fit)
#'
#' # summary of factor returns estimated in each time period
#' summary(fit)
#'
#' # summary of lm fit for a single period
#' summary(fit$factor.fit[[1]])
#'
#' @method summary ffm
#' @export

summary.ffm <- function(object, ...){

  # check input object validity
  if (!inherits(object, "ffm")) {
    stop("Invalid 'ffm' object")
  }

  # extract summary.lm objects for each factor
  sum.list <- lapply(object$factor.fit, summary)

  # include the call and se.type to fitFfm
  sum <- list(call=object$call, sum.list=sum.list)
  class(sum) <- "summary.ffm"
  return(sum)
}


#' @rdname summary.ffm
#' @method print summary.ffm
#' @export

print.summary.ffm <- function(x, digits=3, labels=TRUE, ...) {
  n <- length(x$sum.list)
  if (labels==TRUE) {
    if(!is.null(cl <- x$call)) {
      cat("\nCall:\n")
      dput(cl)
    }
    cat("\nFactor Returns:\n", sep="")
    for (i in 1:n) {
      options(digits = digits)
      table.coef <- (x$sum.list)[[i]]$coefficients
      cat("\nTime Period ", i, ": ", names(x$sum.list[i]), "\n\n", sep="")
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
