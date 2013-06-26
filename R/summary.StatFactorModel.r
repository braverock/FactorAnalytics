#' summary method for StatFactorModel object.
#' 
#' Generic function of summary method for fitStatisticalFactorModel. It utilizes
#' function \code{summary.lm}.
#' 
#' @param fit "StatFactorModel" object created by fitStatisticalFactorModel.
#' @param newdata An optional data frame in which to look for variables with which to predict. 
#' If omitted, the fitted values are used.
#' @param ... Any other arguments used in \code{summary.lm}
#' @author Yi-An Chen.
#' ' 
#' @examples
#' data(stat.fm.data)
#'.fit <- fitStatisticalFactorModel(sfm.dat,k=2,
#                                   ckeckData.method="data.frame")
#' 
#' summary(fit)
#' @export
#' 


summary.StatFactorModel <- function(fit,...){
  lapply(fit$asset.fit, summary,...)
}