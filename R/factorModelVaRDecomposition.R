factorModelVaRDecomposition <-
function(bootData, beta.vec, sig2.e, tail.prob = 0.01,
                                        VaR.method=c("HS", "CornishFisher")) {
## Compute factor model factor VaR decomposition based on Euler's theorem given historic 
## or simulated data and factor model parameters.
## The partial derivative of VaR wrt factor beta is computed
## as the expected factor return given fund return is equal to its VaR and approximated by kernel estimator.
## VaR is compute either as the sample quantile or as an estimated quantile
## using the Cornish-Fisher expansion.
## inputs:
## bootData   B x (k+2) matrix of bootstrap data. First column contains the fund returns,
##            second through k+1 columns contain factor returns, (k+2)nd column contain residuals
##            scaled to have variance 1.
## beta.vec   k x 1 vector of factor betas
## sig2.e  		scalar, residual variance from factor model
## tail.prob  scalar tail probability
## method     character, method for computing marginal ES. Valid choices are
##            "average" for approximating E[Fj | R=VaR]
## VaR.method character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package
## output:
## A list with the following components:
## VaR.fm              scalar, bootstrap VaR value for fund reported as a positive number
## n.exceed            scalar, number of observations beyond VaR
## idx.exceed          n.exceed x 1 vector giving index values of exceedences
## mcES.fm             k+1 x 1 vector of factor marginal contributions to ES
## cES.fm              k+1 x 1 vector of factor component contributions to ES
## pcES.fm             k+1 x 1 vector of factor percent contributions to ES
## Remarks:
## The factor model has the form
## R(t) = beta'F(t) + e(t) = beta.star'F.star(t)
## where beta.star = (beta, sig.e)' and F.star(t) = (F(t)', z(t))'
## By Euler's theorem
## ES.fm = sum(cES.fm) = sum(beta.star*mcES.fm)
## References:
## 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
##    The Journal of Risk 5/2.
## 2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and
##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
##    Bank of Japan.
## 3. Meucci (2007). "Risk Contributions from Generic User-Defined Factors," Risk.
  
  
require(PerformanceAnalytics)
  VaR.method = VaR.method[1]
  bootData = as.matrix(bootData)
  ncol.bootData = ncol(bootData)
  if(is.matrix(beta.vec)) {
    beta.names = c(rownames(beta.vec), "residual")
  } else if(is.vector(beta.vec)) {
    beta.names = c(names(beta.vec), "residual")
  } else {
   stop("beta.vec is not an n x 1 matrix or a vector")
  }  
  beta.names = c(names(beta.vec), "residual")
  beta.star.vec = c(beta.vec, sqrt(sig2.e))
	names(beta.star.vec) = beta.names

   ## epsilon is calculated in the sense of minimizing mean square error by Silverman 1986
   epi <- 2.575*sd(bootData[,1]) * (nrow(bootData)^(-1/5))
   if (VaR.method == "HS") {
    VaR.fm = quantile(bootData[, 1], prob=tail.prob)
    idx = which(bootData[, 1] <= VaR.fm + epi & bootData[,1] >= VaR.fm - epi)
    } else {
    VaR.fm = as.numeric(VaR(bootData[, 1], p=(1-tail.prob),method="modified"))
    idx = which(bootData[, 1] <= VaR.fm + epi & bootData[,1] >= VaR.fm - epi)
    }
  ##
  ## compute marginal contribution to VaR
  ##
  ## compute marginal VaR as expected value of factor return given 
  ## triangler kernel  
    mVaR.fm = -as.matrix(colMeans(bootData[idx, -1]))
    
## compute correction factor so that sum of weighted marginal VaR adds to portfolio VaR
cf = as.numeric( -VaR.fm / sum(mVaR.fm*beta.star.vec) )
mVaR.fm = cf*mVaR.fm
cVaR.fm = mVaR.fm*beta.star.vec
pcVaR.fm = cVaR.fm/-VaR.fm
colnames(mVaR.fm) = "MVaR"
colnames(cVaR.fm) = "CVaR"
colnames(pcVaR.fm) = "PCVaR"
ans = list(VaR.fm = -VaR.fm,
           n.exceed = length(idx),
           idx.exceed = idx,
           mVaR.fm = t(mVaR.fm), 
           cVaR.fm = t(cVaR.fm),
           pcVaR.fm = t(pcVaR.fm))
return(ans)
}

