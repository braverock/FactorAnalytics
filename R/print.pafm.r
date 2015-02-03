#' Print object of class \code{"pafm"}.
#' 
#' Generic function of print method for \code{paFm}.
#' 
#' @param x object of class \code{"pafm"} created by
#' \code{paFm}.
#' @param ...  Other arguments for \code{print} methods.
#' 
#' @author Yi-An Chen.
#' 
#' @examples
#' # load data from the database
#'  data(managers)
#' # fit the factor model with OLS
#' fit <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                factor.names=c("EDHEC.LS.EQ","SP500.TR"), data=managers)
#' fm.attr <- paFm(fit)
#' print(fm.attr)
#'
#' @method print pafm  
#' @export   
#' 
print.pafm <- function(x, ...) {
  cat("\nMean of returns attributed to factors
      \n")
  print(sapply(x[[3]], function(x) apply(x,2,mean)))
 }
