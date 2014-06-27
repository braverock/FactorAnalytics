#' @title Extract coefficients from a fitted time series factor model
#' 
#' @description Method or helper function for fit object of class \code{tsfm}. 
#' 
#' @param x an object of class \code{tsfm} which is returned by 
#' \code{\link{fitTSFM}} 
#' 
#' @return 
#' \item{coef.mat}{an N x (K+1) matrix of all coefficients}
#' where, N is the number of assets and K is the number of factors.
#' 
#' @author Eric Zivot and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitTSFM}}
#' 
#' @examples
#' \dontrun{
#' data(managers.df)
#' fit <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                factor.names=colnames(managers.df[,7:9]), 
#'                market.name="SP500.TR",
#'                data=data, fit.method="OLS", variable.selection="none", 
#'                add.up.market=TRUE, add.market.sqd=TRUE)
#' coef(fit)
#' }
#' 
#' @method coef tsfm
#' @export

coef.tsfm <- function(x){
  coef.mat <- t(sapply(x$asset.fit, coef))
  return(coef.mat)
}
