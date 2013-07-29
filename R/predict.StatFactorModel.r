#' predict method for StatFactorModel object.
#' 
#' Generic function of predict method for fitStatisticalFactorModel. It utilizes
#' function \code{predict.lm}.
#' 
#' @param fit.stat "StatFactorModel" object created by fitStatisticalFactorModel.
#' @param newdata a vector, matrix, data.frame, xts, timeSeries or zoo object to be coerced.
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


predict.StatFactorModel <- function(fit.stat,newdata = NULL,...){
  
  if (missing(newdata) || is.null(newdata)  ) {
    lapply(fit.stat$asset.fit, predict,...)
  } else  {
    newdata <- checkData(newdata,method = "data.frame")
    lapply(fit.stat$asset.fit, predict ,newdata,... )
  } 
}