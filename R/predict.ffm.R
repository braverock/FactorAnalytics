#' @title Predicts asset returns based on a fitted fundamental factor model
#' 
#' @description S3 \code{predict} method for object of class \code{ffm}.
#' 
#' @details The estimated factor returns and potentially new factor exposures 
#' are used to predict the asset returns during all dates from the fitted 
#' \code{ffm} object. For predictions based on estimated factor returns from a 
#' specific period use the \code{pred.date} argument.
#' 
#' @param object an object of class \code{ffm} produced by \code{fitFfm}.
#' @param newdata data.frame containing the variables \code{asset.var}, 
#' \code{date.var} and the same exact \code{exposure.vars} used in the fitted
#' \code{ffm} object. If omitted, the predictions are based on the data used 
#' for the fit.
#' @param pred.date character; unique date used to base the predictions. Should 
#' be coercible to class \code{Date} and match one of the dates in the data used
#' in the fiited \code{object}.
#' @param ... optional arguments passed to \code{predict.lm} or 
#' \code{predict.lmRob}.
#' 
#' @return 
#' \code{predict.ffm} produces a N x T matrix of predicted asset returns, where 
#' T is the number of time periods and N is the number of assets. T=1 if 
#' \code{pred.date} is specified.
#' 
#' @author Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitFfm}}, \code{\link{summary.ffm}}, 
#' \code{\link[stats]{predict.lm}}, \code{\link[robust]{predict.lmRob}}
#' 
#' @examples
#' 
#' # Load fundamental and return data
#' data(Stocks.df)
#' 
#' # fit a fundamental factor model
#' exposure.vars <- c("BOOK2MARKET", "LOG.MARKETCAP")
#' fit <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars=exposure.vars)
#' 
#' # generate random data
#' newdata <- as.data.frame(unique(stock$TICKER))
#' newdata$BOOK2MARKET <- rnorm(nrow(newdata))
#' newdata$LOG.MARKETCAP <- rnorm(nrow(newdata))
#' pred.fund <- predict(fit, newdata)
#' 
#' @method predict ffm
#' @export
#' 

predict.ffm <- function(object, newdata=NULL, pred.date=NULL, ...){
  
  if (!is.null(pred.date) && !(pred.date %in% names(object$factor.fit))) {
    stop("Invalid args: pred.date must be a character string that matches one 
         of the dates used in the fit")
  }
  
  if (is.null(newdata)) {
    sapply(object$factor.fit, predict, ...)
  } else {
    newdata <- checkData(newdata, method="data.frame")
    if (is.null(pred.date)) {
      sapply(object$factor.fit, predict, newdata, ...)
    } else {
      as.matrix(predict(object$factor.fit[[pred.date]], newdata, ...))
    }
  }
}
