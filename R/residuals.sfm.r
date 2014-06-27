#' @title Get residuals from a fitted stochastic factor model
#' 
#' @description Method or helper function for fit object of class \code{sfm}. 
#' 
#' @param x an object of class \code{sfm} which is returned by 
#' \code{\link{fitSFM}} 
#' 
#' @return 
#' \item{residuals.xts}{an N x T data object of residuals}
#' where, N is the number of assets and T is the number of time periods.
#' 
#' @author Eric Zivot and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitSFM}}
#' 
#' @method residuals sfm
#' @export

residuals.sfm <- function(x) {
  # get residuals from each linear factor model fit 
  # and convert them into xts/zoo objects
  residuals.list = sapply(x$asset.fit, function(x) checkData(residuals(x)))
  # this is a list of xts objects, indexed by the asset name
  # merge the objects in the list into one xts object
  residuals.xts <- do.call(merge, residuals.list)
  return(residuals.xts)
}
