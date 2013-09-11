#' predict method for StatFactorModel object.
#' 
#' Generic function of predict method for fitStatisticalFactorModel. It utilizes
#' function \code{predict.lm}.
#' 
#' @param object  A fit object created by fitStatisticalFactorModel.
#' @param newdata A vector, matrix, data.frame, xts, timeSeries or zoo object to be coerced.
#' @param ... Any other arguments used in \code{predict.lm}, such as \code{newdata} and 
#' \code{fit.se}. 
#' @author Yi-An Chen.
#' @method predict StatFactorModel
#' @export 
#' @examples
#' data(stat.fm.data)
#' fit <- fitStatisticalFactorModel(sfm.dat,k=2)
#' pred.stat <- predict(fit)
#' 


predict.StatFactorModel <- function(object,newdata = NULL,...){
  
  if (missing(newdata) || is.null(newdata)  ) {
    lapply(object$asset.fit, predict,...)
  } else  {
    newdata <- checkData(newdata,method = "data.frame")
    lapply(object$asset.fit, predict ,newdata,... )
  } 
}