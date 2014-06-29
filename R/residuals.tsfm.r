#' @title Get residuals from a fitted time series factor model
#' 
#' @description Method or helper function for fit object of class \code{tsfm}. 
#' 
#' @param object a fit object of class \code{tsfm} which is returned by 
#' \code{\link{fitTSFM}} 
#' @param ... other arguments passed
#' 
#' @return 
#' \item{residuals.xts}{an N x T data object of residuals}
#' where, N is the number of assets and T is the number of time periods.
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
#' residuals(fit)
#' }
#' 
#' @method residuals tsfm
#' @export

residuals.tsfm <- function(object ,...) {
  # get residuals from each linear factor model fit 
  # and convert them into xts/zoo objects
  residuals.list = sapply(object$asset.fit, function(x) checkData(residuals(x)))
  # this is a list of xts objects, indexed by the asset name
  # merge the objects in the list into one xts object
  residuals.xts <- do.call(merge, residuals.list)
  return(residuals.xts)
}
