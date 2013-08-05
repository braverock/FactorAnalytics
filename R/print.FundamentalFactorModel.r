#' print FundamentalFactorModel object
#' 
#' Generic function of print method for fitFundamentalFactorModel.
#' 
#' 
#' @param x fit object created by fitFundamentalFactorModel.
#' @param digits integer indicating the number of decimal places. Default is 3.
#' @param ...  Other arguments for print methods.
#' @author Yi-An Chen.
#' @method print FundamentalFactorModel
#' @export
#' @examples
#' 
#' data(Stock.df)
#' # there are 447 assets  
#' exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
#' test.fit <- fitFundamentalFactorModel(data=stock,exposure.names=exposure.names,
#'                                        datevar = "DATE", returnsvar = "RETURN",
#'                                        assetvar = "TICKER", wls = TRUE, 
#'                                        regression = "classic", 
#'                                        covariance = "classic", full.resid.cov = TRUE, 
#'                                        robust.scale = TRUE)
#' 
#' print(test.fit)
#'  
print.FundamentalFactorModel <-
  function(x, digits = max(3, .Options$digits - 3), ...)
  {
    if(!is.null(cl <- x$call)) {
      cat("\nCall:\n")
      dput(cl)
    }
    cat("\nFactor Model:\n")
    tmp <- c(dim(x$beta)[2]-1,length(x$asset.names), nrow(x$factor.returns))
    names(tmp) <- c("Exposures", "Variables", "Periods")
    print(tmp)
    cat("\nFactor Returns:\n")
    print(x$factor.returns, digits = digits, ...)
    cat("\nResidual Variance:\n")
    print(x$resid.variance, digits = digits, ...)
    
  }
