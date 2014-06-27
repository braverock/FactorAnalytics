#' @title Get fitted values from a stochastic factor model
#' 
#' @description Method or helper function for fit object of class \code{sfm}. 
#' 
#' @param x an object of class \code{sfm} which is returned by 
#' \code{\link{fitSFM}} 
#' 
#' @return 
#' \item{fitted.xts}{an N x T data object of fitted values}
#' where, N is the number of assets and T is the number of time periods.
#' 
#' @author Eric Zivot and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitSFM}}
#' 
#' @method fitted tsfm
#' @export

fitted.sfm <- function(x){
  # get fitted values from each linear factor model fit 
  # and convert them into xts/zoo objects
  fitted.list = sapply(x$asset.fit, function(x) checkData(fitted(x)))
  # this is a list of xts objects, indexed by the asset name
  # merge the objects in the list into one xts object
  fitted.xts <- do.call(merge, fitted.list)
  return(fitted.xts)
}
