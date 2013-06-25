#' summary method for TimeSeriesModel object.
#' 
#' Generic function of summary method for fitTimeSeriesFactorModel.
#' 
#' 
#' @param fit fit object created by fitTimeSeiresFactorModel.
#' @author Yi-An Chen.
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' factors    = managers.df[,(7:9)]
#' # fit the factor model with OLS
#' fit <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                data=managers.df,fit.method="OLS")
#' summary(fit)
#' 
#' @export
#' 
summary.TimeSeriesFactorModel <- 
  function(fit){
     lapply(fit[[1]], summary)
  }
    
