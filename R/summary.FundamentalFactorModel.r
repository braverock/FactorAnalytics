#' summary FundamentalFactorModel object
#' 
#' Generic function of summary method for fitFundamentalFactorModel.
#' 
#' 
#' @param object An object created by fitFundamentalFactorModel.
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
#' @method summary FundamentalFactorModel
#' @export 
summary.FundamentalFactorModel <-
  function(object, digits = max(3, .Options$digits - 3), ...)
  {
    if(!is.null(cl <- object$call)) {
      cat("\nCall:\n")
      dput(cl)
    }
  
    cat("\nFactor Returns:\n")
    n <- dim(object$factor.returns)[1]
    k <- dim(object$factor.returns)[2]
    se.beta <- object$factor.returns/object$tstat
    pvalue <- 1- pt(object$tstat,df=n-k)
    table.fund <- cbind(object$factor.returns[,1],se.beta[,1],object$tstat[,1])
    f.names <- colnames(object$factor.returns)
    for (i in 1:k) {
    cat("\n",f.names[i],"\n")
    table.fund <- cbind(object$factor.returns[,i],se.beta[,i],object$tstat[,i],pvalue[,i])
    colnames(table.fund)[1:4] <- c("Estimate","Std. Error","t value","Pr(>|t|)")
    print(table.fund, digits = digits,...)
    }
    cat("\nResidual Variance:\n")
    print(object$resid.variance, digits = digits, ...)
   
  }
