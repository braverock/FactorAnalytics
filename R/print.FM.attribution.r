#' Print FM.attribution object.
#' 
#' Generic function of print method for \code{factorModelPerformanceAttribution}.
#' 
#' 
#' @param fm.attr FM.attribution object created by
#' \code{factorModelPerformanceAttribution}.
#' @author Yi-An Chen.
#' @examples
#' \dontrun{
#' # load data from the database
#'  data(managers.df)
#'  # fit the factor model with OLS
#'  fit.ts <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                data=managers.df,fit.method="OLS")
#'   
#'   fm.attr <- factorModelPerformanceAttribution(fit.ts)
#'   print(fm.attr)
#'   }
#' @method print FM.attribution  
#' @export   
#' 
print.FM.attribution <- function(fm.attr) {
  cat("\nMean of returns attributed to factors
      \n")
  print(sapply(fm.attr[[3]],function(x) apply(x,2,mean)))
 }
