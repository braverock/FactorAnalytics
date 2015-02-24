#' @title Predicts asset returns based on a fitted up and down market time series factor model
#' 
#' @description S3 \code{predict} method for object of class \code{tsfmUpDn}. It 
#' calls the \code{predict.tsfm} method for a list object of \code{Up} and \code{Dn}
#' 
#' @param object an object of class \code{tsfmUpDn} produced by \code{fitTsfmUpDn}.
#' @param ... optional arguments passed to \code{predict.lm} or
#' \code{\link[robust]{predict.lmRob}}, such as \code{se.fit}, or, to 
#' \code{\link[lars]{predict.lars}} such as \code{mode}.
#' 
#' @return 
#' \code{predict.tsfmUpDm} produces a list of \code{Up} and \code{Dn}. Both \code{Up} and \code{Dn} contain a 
#' vector or a matrix of predictions.
#' 
#' @author Yi-An Chen and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{predict.tsfm}},\code{\link{fitTsfmUpDn}}, \code{\link{summary.tsfmUpDn}}
#' 
#' @examples
#' # load data from the database
#' data(managers)
#' # fit the factor model with OLS
#  example: Up and down market factor model with OLS fit
#' fitUpDn <- fitTsfmUpDn(asset.names=colnames(managers[,(1:6)]),mkt.name="SP500.TR",
#'                        data=managers, fit.method="OLS",control=NULL)
#'  
#' predict(fitUpDn)
#' 
#' @importFrom PerformanceAnalytics checkData
#' 
#' @method predict tsfmUpDn
#' @export
#' 

predict.tsfmUpDn <- function(object,...) {
  uD.list <- lapply(object,predict,...)
  return(uD.list)
}