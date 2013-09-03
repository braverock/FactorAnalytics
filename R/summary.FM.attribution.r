#' summary FM.attribution object.
#' 
#' Generic function of summary method for factorModelPerformanceAttribution.
#' 
#' 
#' @param fm.attr FM.attribution object created by
#' factorModelPerformanceAttribution.
#' @author Yi-An Chen.
#' @examples
#' # load data from the database
#'  data(managers.df)
#'  # fit the factor model with OLS
#'  fit.ts <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                data=managers.df,fit.method="OLS")
#'   
#'   fm.attr <- factorModelPerformanceAttribution(fit.ts)
#'   summary(fm.attr)
#' @method summary FM.attribution  
#' @export   
#' 
summary.FM.attribution <- function(fm.attr) {
#   n <- dim(fm.attr[[1]])[1]
#   k <- dim(fm.attr[[1]])[2]+1 
# table.mat <- matrix(rep(NA,n*k*2),ncol=n)
  cat("\nMean of returns attributed to factors
      \n")
  print(sapply(fm.attr[[3]],function(x) apply(x,2,mean)))
  cat("\nStandard Deviation of returns attributed to factors
      \n")
  print(sapply(fm.attr[[3]],function(x) apply(x,2,sd)))  
}
