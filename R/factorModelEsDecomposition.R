#' Compute Factor Model ES Decomposition
#' 
#' Compute the factor model factor expected shortfall (ES) decomposition for an
#' asset based on Euler's theorem given historic or simulated data and factor
#' model parameters. The partial derivative of ES with respect to factor beta
#' is computed as the expected factor return given fund return is less than or
#' equal to its value-at-risk (VaR). VaR is compute as the sample quantile of
#' the historic or simulated data.
#' 
#' The factor model has the form \cr \code{R(t) = beta'F(t) + e(t) = beta.star'F.star(t)}\cr
#' where beta.star = (beta, sig.e)' and F.star(t) = (F(t)', z(t))' By Euler's
#' theorem:\cr \code{ES.fm = sum(cES.fm) = sum(beta.star*mES.fm)} \cr
#' 
#' @param Data \code{B x (k+2)} matrix of historic or simulated data. The first
#' column contains the fund returns, the second through \code{k+1}st columns
#' contain the returns on the \code{k} factors, and the \code{(k+2)}nd column
#' contain residuals scaled to have unit variance.
#' @param beta.vec \code{k x 1} vector of factor betas.
#' @param sig2.e scalar, residual variance from factor model.
#' @param tail.prob scalar, tail probability for VaR quantile. Typically 0.01
#' or 0.05.
#' @param VaR.method character, method for computing VaR. Valid choices are
#' one of "modified","gaussian","historical", "kernel". computation is done with the \code{VaR}
#' in the PerformanceAnalytics package.
#' 
#' 
#' @return A list with the following components:
#' \itemize{
#' \item{VaR} {Scalar, nonparametric VaR value for fund reported as a
#' positive number.}
#' \item{n.exceed} Scalar, number of observations beyond VaR.
#' \item{idx.exceed} n.exceed x 1 vector giving index values of exceedences.
#' \item{ES.fm}  Scalar. nonparametric ES value for fund reported as a positive number.
#' \item{mES.fm} (K+1) x 1 vector of factor marginal contributions to ES.
#' \item{cES.fm} (K+1) x 1 vector of factor component contributions to ES.
#' \item{pcES.fm} (K+1) x 1 vector of factor percentage component contributions to ES.
#' }
#' @author Eric Zviot and Yi-An Chen.
#' @references \enumerate{ 
#' \item Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A
#' General Analysis", The Journal of Risk 5/2.
#' \item Yamai and Yoshiba (2002)."Comparative Analyses of Expected Shortfall and Value-at-Risk: Their
#' Estimation Error, Decomposition, and Optimization Bank of Japan.
#' \item Meucci (2007). "Risk Contributions from Generic User-Defined Factors," Risk. 
#' \item Epperlein and Smillie (2006) "Cracking VAR with Kernels," Risk.
#' }
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
#' factor.es.decomp.HAM1 = factorModelEsDecomposition(tmpData, fit.macro$beta[1,],
#'                                                   fit.macro$resid.variance[1], tail.prob=0.05,
#'                                                   VaR.method="historical" )
#' 
#' # fundamental factor model
#' # try to find factor contribution to ES for STI 
#' data(Stock.df)
#' fit.fund <- fitFundamentalFactorModel(exposure.names=c("BOOK2MARKET", "LOG.MARKETCAP")
#'                                       , data=stock,returnsvar = "RETURN",datevar = "DATE",  
#'                                      assetvar = "TICKER",
#'                                       wls = TRUE, regression = "classic", 
#'                                       covariance = "classic", full.resid.cov = FALSE)
#'  idx <- fit.fund$data[,fit.fund$assetvar]  == "STI"  
#' asset.ret <- fit.fund$data[idx,fit.fund$returnsvar]
#' tmpData = cbind(asset.ret, fit.fund$factor.returns,
#'                 fit.fund$residuals[,"STI"]/sqrt(fit.fund$resid.variance["STI"]) )
#' colnames(tmpData)[c(1,length(tmpData[1,]))] = c("STI", "residual")
#' factorModelEsDecomposition(tmpData, 
#'                           fit.fund$beta["STI",],
#'                           fit.fund$resid.variance["STI"], tail.prob=0.05,VaR.method="historical")
#' 
#' @export
#' 
factorModelEsDecomposition <-
function(Data, beta.vec, sig2.e, tail.prob = 0.05,
         VaR.method=c("modified", "gaussian", "historical", "kernel")) {

  require(PerformanceAnalytics)
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
  
  
 
  ES.fm = -mean(Data[idx, 1])
  
  ##
  ## compute marginal contribution to ES
  ##
  ## compute marginal ES as expected value of factor return given fund
  ## return is less than or equal to VaR
    mcES.fm = -as.matrix(colMeans(Data[idx, -1]))
  
## compute correction factor so that sum of weighted marginal ES adds to portfolio ES
cf = as.numeric( ES.fm / sum(mcES.fm*beta.star.vec) )
mcES.fm = cf*mcES.fm
cES.fm = mcES.fm*beta.star.vec
pcES.fm = cES.fm/ES.fm
colnames(mcES.fm) = "MCES"
colnames(cES.fm) = "CES"
colnames(pcES.fm) = "PCES"
ans = list(VaR.fm = -VaR.fm,
           n.exceed = length(idx),
           idx.exceed = idx,
           ES.fm = ES.fm, 
           mES.fm = t(mcES.fm), 
           cES.fm = t(cES.fm),
           pcES.fm = t(pcES.fm))
return(ans)
}

