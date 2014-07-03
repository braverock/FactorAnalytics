#' Print object of class \code{"pafm"}.
#' 
#' Generic function of print method for \code{paFM}.
#' 
#' 
#' @param x object of class \code{"pafm"} created by
#' \code{paFM}.
#' @param ...  Other arguments for \code{print} methods.
#' @author Yi-An Chen.
#' @examples
#' # load data from the database
#'  data(managers.df)
#' # fit the factor model with OLS
#' fit <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                factor.names=c("EDHEC.LS.EQ","SP500.TR"), data=managers.df, 
#'                add.up.market=FALSE, add.market.sqd=FALSE, 
#'                fit.method="OLS", variable.selection="none")
#' fm.attr <- paFM(fit)
#' print(fm.attr)
#'
#' @method print pafm  
#' @export   
#' 
print.pafm <- function(x,...) {
  cat("\nMean of returns attributed to factors
      \n")
  print(sapply(x[[3]],function(x) apply(x,2,mean)))
 }
