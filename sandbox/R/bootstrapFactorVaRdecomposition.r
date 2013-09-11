bootstrapFactorVaRdecomposition <- function(bootData, beta.vec, sig2.e, h=NULL, tail.prob = 0.01,
                                            method=c("average"),
                                            VaR.method=c("HS", "CornishFisher")) {
## Compute factor model VaR decomposition based on Euler's theorem given bootstrap data
## and factor model parameters.
## The partial derivative of VaR wrt factor beta is computed 
## as the expected factor return given fund return is equal to portfolio VaR
## VaR is compute either as the sample quantile or as an estimated quantile
## using the Cornish-Fisher expansion
## inputs:
## bootData   B x (k+2) matrix of bootstrap data. First column contains the fund returns,
##            second through k+1 columns contain factor returns, k+2 column contain residuals
##            scaled to have variance 1.
## beta.vec   k x 1 vector of factor betas
## sig2.e			scalar, residual variance from factor model
## h          integer, number of obvs on each side of VaR. Default is h=round(sqrt(B)/2)
## tail.prob  scalar tail probability
## method     character, method for computing marginal VaR. Valid choices are
##            "average" for approximating E[Fj | R=VaR]
## VaR.method character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package
## output:
## Output:
## A list with the following components:
## VaR.fm             scalar, bootstrap VaR value for fund reported as a positive number
## mcVaR.fm             k+1 x 1 vector of factor marginal contributions to VaR
## cVaR.fm              k+1 x 1 vector of factor component contributions to VaR
## pcVaR.fm             k+1 x 1 vector of factor percent contributions to VaR
## Remarks:
## The factor model has the form
## R(t) = beta'F(t) + e(t) = beta.star'F.star(t)
## where beta.star = (beta, sig.e)' and F.star(t) = (F(t)', z(t))'
## By Euler's theorem
## VaR.fm = sum(cVaR.fm) = sum(beta.star*mcVaR.fm)
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
  beta.names = c(names(beta.vec), "residual")
  #beta.vec = as.vector(beta.vec)
	beta.star.vec = c(beta.vec, sqrt(sig2.e))
	names(beta.star.vec) = beta.names

  # determine number of obvs to average around VaR
  if (is.null(h)) {
    h = round(sqrt(nrow(bootData)))
  } else h = round(h)

  if (VaR.method == "HS") {
    VaR.fm = -quantile(bootData[,1], prob=tail.prob)
  } else {
    VaR.fm = VaR.CornishFisher(bootData[,1], p=(1-tail.prob))
  }
  ##
  ## compute marginal contribution to VaR
  ##
  if (method == "average") {
  ## compute marginal VaR as expected value of fund return given portfolio
  ## return is equal to portfolio VaR
    r.sort = sort(bootData[,1])
    idx.lower = which(r.sort <= -VaR.fm)
    idx.upper = which(r.sort > -VaR.fm)
    r.vals = c(r.sort[tail(idx.lower,n=h)], r.sort[head(idx.upper,n=h)])
    idx = which(bootData[,1] %in% r.vals)
    mcVaR.fm = -as.matrix(colMeans(bootData[idx,-1]))
  } else {
    stop("invalid method")
  }
  
## compute correction factor so that sum of weighted marginal VaR adds to portfolio VaR
cf = as.numeric( VaR.fm / sum(mcVaR.fm*beta.star.vec) )
mcVaR.fm = cf*mcVaR.fm
cVaR.fm = mcVaR.fm*beta.star.vec
pcVaR.fm = cVaR.fm/VaR.fm
ans = list(VaR.fm = VaR.fm, 
           mcVaR.fm = mcVaR.fm, 
           cVaR.fm = cVaR.fm,
           pcVaR.fm = pcVaR.fm)
return(ans)
}
