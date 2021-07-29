#' @title Predicts asset returns based on a fitted time series factor model
#' 
#' @description S3 \code{predict} method for object of class \code{tsfm}. It 
#' calls the \code{predict} method for fitted objects of class \code{lm}, 
#' \code{lmRob} or \code{lars} as appropriate.
#' 
#' @param object an object of class \code{tsfm} produced by \code{fitTsfm}.
#' @param newdata a vector, matrix, data.frame, xts, timeSeries or zoo object 
#' containing the variables with which to predict.
#' @param ... optional arguments passed to \code{predict.lm} or
#' \code{\link[robust]{predict.lmRob}}, such as \code{se.fit}, or, to 
#' \code{\link[lars]{predict.lars}} such as \code{mode}.
#' 
#' @return 
#' \code{predict.tsfm} produces a matrix of return predictions, if all assets 
#' have equal history. If not, a list of predicted return vectors of unequal 
#' length is produced.
#' 
#' @author Yi-An Chen and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{summary.tsfm}}
#' 
#' @examples
#' # load data from the database
#' data(managers, package = 'PerformanceAnalytics')
#' # fit the factor model with LS
#' fit <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                factor.names=c("EDHEC.LS.EQ","SP500.TR"), data=managers)
#' 
#' pred.fit <- predict(fit)
#' newdata <- data.frame("EDHEC.LS.EQ"=rnorm(n=120), "SP500.TR"=rnorm(n=120))
#' rownames(newdata) <- rownames(fit$data)
#' pred.fit2 <- predict(fit, newdata, interval="confidence")
#' 
#' @method predict tsfm
#' @export
#' 

predict.tsfm <- function(object, newdata=NULL, ...){
  
  if (is.null(newdata)) {
    sapply(object$asset.fit, predict, ...)
  } else {
    newdata <- checkData(newdata, method="data.frame")
    sapply(object$asset.fit, predict, newdata, ...)
  }
}
