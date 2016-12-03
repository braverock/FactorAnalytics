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
#' @param type one of "np" (non-parametric) or "normal". Default is "np".
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
 
assetDecomp = function(object,rm, weights, p, type = "np")
{
  n.assets = length(object$asset.names)
  returns = matrix(object$data[,"RETURN"], n.assets,length(object$time.periods))
  returns = xts(t(returns), order.by =unique(object$data[,"DATE"] ))
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
           if(type == "normal")
           {
             RM = drop(t(weights) %*% (apply(returns, 2, mean)) + port.Sd*qnorm(p))
             MCR = lapply(seq(1:n.assets), FUN = function(x){mean((returns)[,x])+ as.numeric((cov(returns)[,x] %*% weights)*qnorm(p)/ port.Sd)})
             CR = weights * unlist(MCR)
             rownames(CR) = object$asset.names
             PCR = CR/RM*100
           }

           else if(type == "np")
           {
             R.xts = returns %*% weights
             R.xts = xts(R.xts, order.by = index(returns))
             Risk.fm <- quantile(R.xts, probs=p, na.rm=TRUE)
             # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
             # the constant 2.575 corresponds to a triangular kernel 
             eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
             
             # compute marginal VaR as expected value of factor returns, such that the
             # asset return was incident in the triangular kernel region peaked at the 
             # VaR value and bandwidth = epsilon.
             k.weight <- as.vector(1 - abs(R.xts - Risk.fm) / eps)
             k.weight[k.weight<0] <- 0
             mRisk <- colMeans(returns*k.weight, na.rm =TRUE)

           }
         },
         ES = 
         {
          if(type == "normal")
          {
            RM = drop(t(weights) %*% (apply(returns, 2, mean)) + port.Sd*dnorm(qnorm(p))*(1/p))
            MCR = lapply(seq(1:n.assets), FUN = function(x){mean((returns)[,x])+ as.numeric((cov(returns)[,x] %*% weights)*dnorm(qnorm(p))*(1/p)/port.Sd)})
            CR = weights * unlist(MCR)
            rownames(CR) = object$asset.names
            PCR = CR/RM*100
          }
           else if(type == "np")
           {
             
             VaR.fm <- rep(NA, 1)
             # get VaR for asset i
             VaR.fm <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
             # index of VaR exceedances
             idx.exceed <- which(R.xts <= VaR.fm)
             # compute ES as expected value of asset return, such that the given asset 
             # return is less than or equal to its value-at-risk (VaR)
             Risk.fm <- mean(R.xts[idx.exceed], na.rm =TRUE)
             
             # compute marginal ES as expected value of factor returns, when the asset's 
             # return is less than or equal to its value-at-risk (VaR)
             
             mRisk <- colMeans(R.xts[idx.exceed,], na.rm =TRUE)       
           }
         })
  if(type == "np")
  {
    # correction factor to ensure that sum(cRisk) = asset Risk
    cf <- as.numeric( Risk.fm / sum(mRisk*weights), na.rm=TRUE) 
    # compute corrected marginal, component and percentage contributions to Risk
    mRisk <- drop(cf * mRisk)
    RM<- Risk.fm
    CR <- drop(mRisk * weights)
    PCR <- drop(100* CR / Risk.fm)
  }
  out = list(RM, CR, PCR)
  names(out) = c(rm, paste(rm,".contribution", sep=""), paste(rm,".Percentage_Contrib", sep=""))
  return (out)
}




