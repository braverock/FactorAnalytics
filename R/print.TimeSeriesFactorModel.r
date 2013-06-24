#' print TimeSeriesfactorModel object
#' 
#' Generic function of print method for fitTimeSeriesFactorModel.
#' 
#' 
#' @param fit.macro fit object created by fitTimeSeriesFactorModel.
#' @author Eric Zivot and Yi-An Chen.
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' factors    = managers.df[,(7:9)]
#' # fit the factor model with OLS
#' fit.macro <- fitTimeSeriesFactorModel(ret.assets,factors,fit.method="OLS",
#'                                  variable.selection="all subsets")
#' print(fit.macro)
#' 
#' 
print.TimeSeriesFactorModel <-
  function(fit.macro) {
    lapply(fit.macro[[1]], print)
  }
