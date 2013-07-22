#' predict method for StatFactorModel object.
#' 
#' Generic function of predict method for fitStatisticalFactorModel. It utilizes
#' function \code{predict.lm}.
#' 
#' @param fit "StatFactorModel" object created by fitStatisticalFactorModel.
#' @param ... Any other arguments used in \code{predict.lm}. For example like newdata and fit.se. 
#' @author Yi-An Chen.
#' ' 
#' @examples
#' data(stat.fm.data)
#'.fit <- fitStatisticalFactorModel(sfm.dat,k=2,
#                                   ckeckData.method="data.frame")
#' 
#' predict(fit)
#' @export
#' 


predict.StatFactorModel <- function(fit,...){
  lapply(fit$asset.fit, predict,...)
}