#' @title Extract coefficients from a fitted stochastic factor model
#' 
#' @description Method or helper function for fit object of class \code{sfm}. 
#' 
#' @param object a fit object of class \code{sfm} which is returned by 
#' \code{\link{fitSFM}} 
#' @param ... other arguments passed
#' 
#' @return
#' \item{coef.mat}{an N x (K+1) matrix of all coefficients}
#' where, N is the number of assets and K is the number of factors.
#' 
#' @author Eric Zivot and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitTSFM}}
#' 
#' @method coef sfm
#' @export

coef.sfm <- function(object,...){
  coef.mat <- t(sapply(object$asset.fit, coef))
  return(coef.mat)
}