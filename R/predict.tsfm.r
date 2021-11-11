#' @title Predicts asset returns based on a fitted time series factor model
#' 
#' @description S3 \code{predict} method for object of class \code{tsfm}. It 
#' calls the \code{predict} method for fitted objects of class \code{lm}, 
#' \code{lmRob} or \code{lars} as appropriate.
#' 
#' @importFrom PerformanceAnalytics checkData
#' 
#' @param object an object of class \code{tsfm} produced by \code{fitTsfm}.
#' @param newdata a vector, matrix, data.frame, xts, timeSeries or zoo object 
#' containing the variables with which to predict.
#' @param ... optional arguments passed to \code{predict.lm} or
#' \code{\link[robustbase]{predict.lmrob}}, such as \code{se.fit}, or, to 
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
#' 
#' # fit the factor model with LS
#' fit <- fitTsfm(asset.names = colnames(managers[,(1:6)]),
#'                factor.names = c("EDHEC LS EQ","SP500 TR"), 
#'                data = managers)
#' 
#' predict_fit <- predict(fit)
#' 
#' newdata <- data.frame(rnorm(n=NROW(fit$data)), rnorm(n=NROW(fit$data)))
#' colnames(newdata) <- c("EDHEC LS EQ", "SP500 TR")
#' rownames(newdata) <- zoo::index(fit$data)
#' 
#' predict_fit_2 <- predict(fit, newdata, interval = "confidence")
#' 
#' @method predict tsfm
#' @export
#' 

predict.tsfm <- function(object, newdata=NULL, ...){
  
  if (is.null(newdata)) {
    sapply(object$asset.fit, predict, ...)
  } else {
    newdata <- PerformanceAnalytics::checkData(newdata, method="data.frame")
    sapply(object$asset.fit, predict, newdata, ...)
  }
}
