#' summary \code{"pafm"} object.
#' 
#' Generic function of summary method for \code{paFm}.
#' 
#' @param object \code{"pafm"} object created by
#' \code{paFm}.
#' @param digits integer indicating the number of decimal places. Default is 3.
#' @param ...  Other arguments for \code{print} methods.
#' 
#' @author Yi-An Chen.
#' 
#' @examples
#' # load data from the database
#' data(managers)
#' # fit the factor model with OLS
#' fit.ts <- fitTsfm(asset.names=colnames(managers[,(1:6)]), 
#'                   factor.names=c("EDHEC LS EQ","SP500 TR"),
#'                   mkt.name="SP500 TR", mkt.timing="both", data=managers)
#'   
#' fm.attr <- paFm(fit.ts)
#' summary(fm.attr)
#' 
#' @method summary pafm  
#' @export   
#' 
summary.pafm <- function(object ,digits = max(3, .Options$digits - 3),...) {
#   n <- dim(fm.attr[[1]])[1]
#   k <- dim(fm.attr[[1]])[2]+1 
# table.mat <- matrix(rep(NA,n*k*2),ncol=n)
  cat("\nMean of returns attributed to factors
      \n")
  print(sapply(object[[3]],function(x) apply(x,2,mean)),digits = digits,...)
  cat("\nStandard Deviation of returns attributed to factors
      \n")
  print(sapply(object[[3]],function(x) apply(x,2,sd)),digits = digits,...)  
}
