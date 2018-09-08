#' @title Summarizing a cusumActMgr object
#'
#' @description \code{summary} method for object of class \code{cusumActMgr}.
#' Returned object is of class {summary.cusumActMgr}. The resulting object is fed to
#' \code{print.summary.cusumActMgr} to print all the summarized objects.
#'
#' @param object an object of class \code{cusumActMgr} returned from \code{cusumActMgr}.
#' @param x an object of class \code{summary.cusumActMgr} returned from
#' \code{summary.cusumActMgr}.
#' @param digits number of significants digits to use when printing.
#' Default is 3.
#' @param ... futher arguments passed to or from other methods.
#'
#' @return Returns an object of class \code{summary.ffm}.
#'
#' Object of class \code{summary.cusumActMgr} is a list of length 10 containing:
#' \item{Logarithmic Excess Returns}{The logarithmic excess returns of the fund
#' relative to the benchmark}
#' \item{Annualized Moving Average}{The annualized moving average of the
#' logarithmic excess returns}
#' \item{Tracking Error}{The monthly tracking error of the logarithmic excess returns}
#' \item{Information Ratios}{The vector of monthly information ratios}
#' \item{Lindley's Recursion}{The vector  Lindley's recursion with a reset after the detection threshold (6.81) is passed.}
#' \item{Annualized Cusum IR}{The vector annualized CUSUM of the information ratios}
#' \item{Annualized Cusum Excess Return}{The vector annualized CUSUM of the excess returns}
#' \item{Excess Volatility}{Excess volatility of the fund, the benchmark and the excess return}
#' \item{Summary Annualized cusumIR}{The summary of annualized cusum IR}
#' \item{Summary Annualized Cusum Excess Returns}{The summary of annualized cusum excess returns}
#'
#' @author Chindhanai Uthaisaad.
#'
#' @examples
#' data(cusumData)
#' results = cusumActMgr(portfolioName = "Parvest", benchmarkName = "RUS2500", data = cusumData)
#' x = summary(results)
#' print(x)
#'
#' @method summary cusumActMgr
#' @export

summary.cusumActMgr <- function(object, digits = 3, ...){

  # check input object validity
  if (!inherits(object, "cusumActMgr")) {
    stop("Invalid 'cusumActMgr' object")
  }

  options(digits = digits)
  # extract summary.lm objects for each factor
  sum_name <- list("Logarithmic Excess Returns",
                   "Annualized Moving Average",
                   "Tracking Error",
                   "Information Ratios",
                   "Lindley's Recursion",
                   "Annualized Cusum IR",
                   "Annualized Cusum Excess Return",
                   "Excess Volatility",
                   "Summary Annualized cusumIR",
                   "Summary Annualized Cusum Excess Returns")

  x1 = summary(coredata(object$Logarithmic_Excess_Returns))
  x2 = summary(coredata(object$Annual_Moving_Average))
  x3 = summary(coredata(object$Tracking_Error))
  x4 = summary(coredata(object$Information_Ratios))
  x5 = summary(coredata(object$"Lindley's_Recursion"))
  x6 = summary(coredata(object$Annualized_Cusum_ER))
  x7 = summary(coredata(object$Annualized_Cusum_IR))
  dimnames(x1)[[2]] = dimnames(x2)[[2]] = dimnames(x3)[[2]] = ""
  dimnames(x4)[[2]] = dimnames(x5)[[2]] = dimnames(x6)[[2]] = dimnames(x7)[[2]] = ""

  sum_list <- list(x1, x2, x3, x4, x5, x6, x7,
                   summary(coredata(object$Excess_Volatility)),
                   object$AIR,
                   object$AER)

  sum = list(sum_name = sum_name, sum_list = sum_list)
  class(sum) <- "summary.cusumActMgr"
  return(sum)
}

#' @rdname summary.cusumActMgr
#' @method print summary.cusumActMgr
#' @export

print.summary.cusumActMgr <- function(x, digits=3, ...) {
  n <- length(x$sum_list)
  for (i in 1:n) {
    options(digits = digits)
    cat(x$sum_name[[i]], "\n")
    print(x$sum_list[[i]])
    cat("\n")
  }
}
