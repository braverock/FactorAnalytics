#' summary FundamentalFactorModel object
#' 
#' Generic function of summary method for fitFundamentalFactorModel.
#' 
#' 
#' @param fit.fund fit object created by fitFundamentalFactorModel.
#' @param digits integer indicating the number of decimal places. Default is 3.
#' @param ...  Other arguments for print methods.
#' @author Yi-An Chen.
#' @examples
#' 
#' data(stock)
#' # there are 447 assets  
#' exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
#' test.fit <- fitFundamentalFactorModel(data=data,exposure.names=exposure.names,
#'                                        datevar = "DATE", returnsvar = "RETURN",
#'                                        assetvar = "TICKER", wls = TRUE, 
#'                                        regression = "classic", 
#'                                        covariance = "classic", full.resid.cov = TRUE, 
#'                                        robust.scale = TRUE)
#' 
#' summary(test.fit)
#' 
#' @export 
summary.FundamentalFactorModel <-
  function(fit.fund, digits = max(3, .Options$digits - 3), ...)
  {
    if(!is.null(cl <- fit.fund$call)) {
      cat("\nCall:\n")
      dput(cl)
    }
  
    cat("\nFactor Returns:\n")
    n <- dim(fit.fund$factor.returns)[1]
    k <- dim(fit.fund$factor.returns)[2]
    se.beta <- fit.fund$factor.returns/fit.fund$tstat
    pvalue <- 1- pt(fit.fund$tstat,df=n-k)
    table.fund <- cbind(fit.fund$factor.returns[,1],se.beta[,1],fit.fund$tstat[,1])
    f.names <- colnames(fit.fund$factor.returns)
    for (i in 1:k) {
    cat("\n",f.names[i],"\n")
    table.fund <- cbind(fit.fund$factor.returns[,i],se.beta[,i],fit.fund$tstat[,i],pvalue[,i])
    colnames(table.fund)[1:4] <- c("Estimate","Std. Error","t value","Pr(>|t|)")
    print(table.fund, digits = digits,...)
    }
    cat("\nResidual Variance:\n")
    print(fit.fund$resid.variance, digits = digits, ...)
  }
