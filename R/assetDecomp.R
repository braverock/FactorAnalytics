#' @title Decompose portfolio risk into individual asset contributions and provide tabular report
#' 
#' @description Compute the asset contributions to Sd, VaR and ES of returns based on Euler's theorem
#' 
#' @importFrom stats cov resid qnorm
#' @importFrom xts xts  
#' 
#' 
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param rm one of "Sd" (Standard Deviation) or "VaR" (Value at Risk) or "ES" (Expected Shortfall)
#' @param weights vector of weights of the assets in the portfolio. Default is NULL, in which case an 
#' equal weights will be used.
#' @param p tail probability for calculation. Default is 0.05.
#' @param ... other optional arguments 
#'
#' @return Risk Decomposition report for every asset in the portfolio
#' 
#' @author Avinash 
#' 
#' 
#' @seealso \code{\link{riskDecomp}}
#' for the Risk Decomposition function based on factors in the fitted model.
#' 
#' 
#' @examples
#' # Fundamental Factor Model
#'data("factorDataSetDjia5Yrs")
#' # fit a fundamental factor model
#'exposure.vars <- c("P2B", "MKTCAP")
#'fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'                 date.var="DATE", exposure.vars=exposure.vars,z.score = TRUE )
#'sd.decomp = assetDecomp(fit, weights, rm = "Sd")
#'VaR.decomp = assetDecomp(fit, weights, rm = "VaR", p = 0.05)
#'ES.decomp = assetDecomp(fit, weights, rm = "ES", p = 0.05)                 
#'
#' @export   
 
assetDecomp = function(object,rm, weights, p)
{
  returns = matrix(object$data[,"RETURN"], 22,60)
  returns = xts(t(returns), order.by =unique(object$data[,"DATE"] ))
  n.assets = length(object$asset.names)
  weights = matrix(rep(1/n.assets, n.assets), ncol = 1)
  port.Sd = sqrt(t(weights) %*% cov(returns) %*% weights)
  switch(rm,
         Sd = 
         {
           RM = drop(port.Sd)
           MCR = lapply(seq(1:n.assets), FUN = function(x){((cov(returns)[,x] %*% weights)/ port.Sd)})
           CR = weights * unlist(MCR)
           rownames(CR) = object$asset.names
           PCR = CR/RM*100
         },
         VaR = 
         {
           RM = drop(t(weights) %*% (apply(returns, 2, mean)) + port.Sd*qnorm(p))
           MCR = lapply(seq(1:n.assets), FUN = function(x){mean((returns)[,x])+ as.numeric((cov(returns)[,x] %*% weights)*qnorm(p)/ port.Sd)})
           CR = weights * unlist(MCR)
           rownames(CR) = object$asset.names
           PCR = CR/RM*100
         },
         ES = 
         {
           RM = drop(t(weights) %*% (apply(returns, 2, mean)) + port.Sd*dnorm(qnorm(p))*(1/p))
           MCR = lapply(seq(1:n.assets), FUN = function(x){mean((returns)[,x])+ as.numeric((cov(returns)[,x] %*% weights)*dnorm(qnorm(p))*(1/p)/port.Sd)})
           CR = weights * unlist(MCR)
           rownames(CR) = object$asset.names
           PCR = CR/RM*100
         })
  out = list(RM, CR, PCR)
  names(out) = c(rm, paste(rm,".contribution", sep=""), paste(rm,".Percentage_Contrib", sep=""))
  return (out)
}