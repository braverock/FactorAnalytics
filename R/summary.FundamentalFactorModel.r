#' summary method for FundamentalFactorModel
#' 
#' Generic function of summary method for fitTimeSeriesFactorModel.
#' 
#' @param fit it object created by fitFundamentalFactorModel.
#' 
#' @author Yi-An Chen
#' 
#' 
#' 
#' @export
#' 

summary.FundamentalFactorModel <- function(fit) {
dim(fit$factors)
print(fit$factors) 
  
}