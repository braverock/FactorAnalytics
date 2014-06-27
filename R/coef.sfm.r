#' @title Extract coefficients from a fitted stochastic factor model
#' 
#' @description Method or helper function for fit object of class \code{sfm}. 
#' 
#' @param x an object of class \code{sfm} which is returned by 
#' \code{\link{fitSFM}} 
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

coef.sfm <- function(x){
  coef.mat <- t(sapply(x$asset.fit, coef))
  return(coef.mat)
}