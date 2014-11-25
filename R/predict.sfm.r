#' @title Predicts asset returns based on a fitted statistical factor model
#' 
#' @description S3 \code{predict} method for object of class \code{sfm}. It 
#' calls the \code{predict} method for fitted objects of class \code{lm}.
#' 
#' @param object an object of class \code{sfm} produced by \code{fitSfm}.
#' @param newdata a vector, matrix, data.frame, xts, timeSeries or zoo object 
#' containing the variables with which to predict.
#' @param ... optional arguments passed to \code{predict.lm}.
#' 
#' @return 
#' \code{predict.sfm} produces a vector or a matrix of predictions.
#' 
#' @author Yi-An Chen and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitSfm}}, \code{\link{summary.sfm}}
#' 
#' @examples
#' # load data from the database
#' data(stat.fm.data)
#' # fit the factor model with PCA
#' fit <- fitSfm(sfm.dat, k=2)
#' 
#' pred.fit <- predict(fit)
#' newdata <- data.frame("EDHEC LS EQ"=rnorm(n=120), "SP500 TR"=rnorm(n=120))
#' rownames(newdata) <- rownames(fit$data)
#' pred.fit2 <- predict(fit, newdata, interval="confidence")
#' 
#' @importFrom PerformanceAnalytics checkData
#' 
#' @method predict sfm
#' @export
#' 

predict.sfm <- function(object, newdata = NULL, ...){
  
  if (missing(newdata) || is.null(newdata)) {
    predict(object$asset.fit, ...)
  } else {
    newdata <- checkData(newdata, method="data.frame")
    predict(object$asset.fit, newdata, ...)
  } 
}