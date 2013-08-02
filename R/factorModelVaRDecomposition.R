#' Compute factor model factor VaR decomposition
#' 
#' Compute factor model factor VaR decomposition based on Euler's theorem given
#' historic or simulated data and factor model parameters. The partial
#' derivative of VaR wrt factor beta is computed as the expected factor return
#' given fund return is equal to its VaR and approximated by kernel estimator.
#' VaR is compute either as the sample quantile or as an estimated quantile
#' using the Cornish-Fisher expansion.
#' 
#' The factor model has the form R(t) = beta'F(t) + e(t) = beta.star'F.star(t)
#' where beta.star = (beta, sig.e)' and F.star(t) = (F(t)', z(t))' By Euler's
#' theorem VaR.fm = sum(cVaR.fm) = sum(beta.star*mVaR.fm)
#' 
#' @param Data B x (k+2) matrix of bootstrap data. First column contains
#' the fund returns, second through k+1 columns contain factor returns, (k+2)nd
#' column contain residuals scaled to have unit variance .
#' @param beta.vec k x 1 vector of factor betas.
#' @param sig2.e scalar, residual variance from factor model.
#' @param tail.prob scalar, tail probability
#' @param VaR.method character, method for computing VaR. Valid choices are
#' one of "modified","gaussian","historical", "kernel". computation is done with the \code{VaR}
#' in the PerformanceAnalytics package.
#' @return an S3 object containing
#' \itemize{
#' \item{VaR.fm} Scalar, bootstrap VaR value for fund reported as a
#' positive number.
#' \item{n.exceed} Scalar, number of observations beyond VaR.
#' \item{idx.exceed} n.exceed x 1 vector giving index values of
#' exceedences.
#' \item{mVaR.fm} (K+1) x 1 vector of factor marginal contributions to VaR.
#' \item{cVaR.fm} (K+1) x 1 vector of factor component contributions to VaR.
#' \item{pcVaR.fm} (K+1) x 1 vector of factor percent contributions to VaR.
#' }
#' @author Eric Zivot and Yi-An Chen
#' @references 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A
#' General Analysis", The Journal of Risk 5/2. 2. Yamai and Yoshiba (2002).
#' "Comparative Analyses of Expected Shortfall and Value-at-Risk: Their
#' Estimation Error, Decomposition, and Optimization Bank of Japan. 3. Meucci
#' (2007). "Risk Contributions from Generic User-Defined Factors," Risk. 4.
#' Epperlein and Smillie (2006) "Cracking VAR with Kernels," Risk.
#' @examples
#' 
#' data(managers.df)
#' fit.macro <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                      factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                      data=managers.df,fit.method="OLS")
#' # risk factor contribution to ETL
#' # combine fund returns, factor returns and residual returns for HAM1
#' tmpData = cbind(managers.df[,1],managers.df[,c("EDHEC.LS.EQ","SP500.TR")] ,
#' residuals(fit.macro$asset.fit$HAM1)/sqrt(fit.macro$resid.variance[1]))
#' colnames(tmpData)[c(1,4)] = c("HAM1", "residual")
#' factor.VaR.decomp.HAM1 = factorModelVaRDecomposition(tmpData, fit.macro$beta[1,],
#'                                                   fit.macro$resid.variance[1], tail.prob=0.05,
#'                                                   VaR.method="historical")
#' 
#' @export
factorModelVaRDecomposition <-
function(Data, beta.vec, sig2.e, tail.prob = 0.01,
         VaR.method=c("modified", "gaussian", "historical", "kernel")) {
  
require(PerformanceAnalytics)
  VaR.method = VaR.method[1]
  Data = as.matrix(Data)
  ncol.Data = ncol(Data)
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
   epi <- 2.575*sd(Data[,1]) * (nrow(Data)^(-1/5))
   VaR.fm = as.numeric(VaR(Data[, 1], p=(1-tail.prob),method=VaR.method))
    idx = which(Data[, 1] <= VaR.fm + epi & Data[,1] >= VaR.fm - epi)
   
  ##
  ## compute marginal contribution to VaR
  ##
  ## compute marginal VaR as expected value of factor return given 
  ## triangler kernel  
    mVaR.fm = -as.matrix(colMeans(Data[idx, -1]))
    
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

