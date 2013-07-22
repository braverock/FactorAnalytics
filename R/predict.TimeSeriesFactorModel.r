#' predict method for TimeSeriesModel object.
#' 
#' Generic function of predict method for fitTimeSeriesFactorModel. It utilizes
#' function \code{predict.lm}.
#' 
#' @param fit "TimeSeriesFactorModel" object created by fitTimeSeiresFactorModel.
#' @param ... Any other arguments used in \code{predict.lm}. for example newdata and se.fit.
#' @author Yi-An Chen.
#' 
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' # fit the factor model with OLS
#' fit <- fitTimeseriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                data=managers.df,fit.method="OLS")
#' 
#' predict(fit)
#' predict(fit,newdata,interval="confidence")
#' 
#' @export
#' 

predict.TimeSeriesFactorModel <- function(fit.macro,...){
#   if (missing(newdata) || is.null(newdata)  ) {
  lapply(fit.macro$asset.fit, predict,...)
#   } 
  
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
  
  
}