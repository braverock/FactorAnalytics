#' @title Predicts asset returns based on a fitted time series factor model
#' 
#' @description S3 \code{predict} method for object of class \code{tsfm}. It 
#' calls the \code{predict} method for fitted objects of class \code{lm}, 
#' \code{lmRob} or \code{lars} as appropriate.
#' 
#' @param object an object of class \code{tsfm} produced by \code{fitTSFM}.
#' @param newdata a vector, matrix, data.frame, xts, timeSeries or zoo object 
#' containing the variables with which to predict.
#' @param ... optional arguments passed to \code{predict.lm} or
#' \code{\link[robust]{predict.lmRob}}, such as \code{se.fit}, or, to 
#' \code{\link[lars]{predict.lars}} such as \code{mode}.
#' 
#' @return 
#' \code{predict.tsfm} produces a vector or a matrix of predictions.
#' 
#' @author Yi-An Chen and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitTSFM}}, \code{\link{summary.tsfm}}
#' 
#' @examples
#' # load data from the database
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' # fit the factor model with OLS
#' fit <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                factor.names=c("EDHEC.LS.EQ","SP500.TR"), data=managers.df, 
#'                fit.method="OLS", variable.selection="none")
#' 
#' pred.fit <- predict(fit)
#' newdata <- data.frame(EDHEC.LS.EQ = rnorm(n=120), SP500.TR = rnorm(n=120) )
#' rownames(newdata) <- rownames(fit$data)
#' pred.fit2 <- predict(fit, newdata, interval="confidence")
#' 
#' @method predict tsfm
#' @export
#' 

predict.tsfm <- function(object, newdata = NULL, ...){
  
  if (missing(newdata) || is.null(newdata)) {
    lapply(object$asset.fit, predict, ...)
  } else {
    newdata <- checkData(newdata, method = "data.frame")
    lapply(object$asset.fit, predict, newdata, ...)
  } 
}

#   
#   if (  !(missing(newdata) && !is.null(newdata) )) {
#    numAssets <- length(names(fit.macro$asset.fit))
#    
#    data <- fit.macro$data
#   factors <-   data[,fit.macro$factors.names]
#    mu.factors <- apply(factors,2,mean)
#    cov.factors <- cov(factors)
#    
#    for (i in 1:numAssets) 
#    if (dim(newdata)[1] < length(residuals(fit$asset.fit[[1]])) ){
#      
#     
#      newdata <- data.frame(EDHEC.LS.EQ = rnorm(n=100), SP500.TR = rnorm(n=100) )
#      newdata.mat <- as.matrix(newdata)
#      factor.scenarios <- 0.001 
#      names(factor.scenarios) <- "SP500.TR"
#      
#      impliedFactorReturns(factor.scenarios, mu.factors, cov.factors)
#      
#    }
#     
#     
#     
#   }


