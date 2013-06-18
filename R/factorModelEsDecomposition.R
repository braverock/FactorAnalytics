#' Compute Factor Model Factor ES Decomposition
#' 
#' Compute the factor model factor expected shortfall (ES) decomposition for an
#' asset based on Euler's theorem given historic or simulated data and factor
#' model parameters. The partial derivative of ES with respect to factor beta
#' is computed as the expected factor return given fund return is less than or
#' equal to its value-at-risk (VaR). VaR is compute as the sample quantile of
#' the historic or simulated data.
#' 
#' The factor model has the form \cr \code{R(t) = t(beta)*F(t) + e(t) =
#' t(beta.star)*F.star(t)} \cr where \code{beta.star = t(beta, sig.e)} and
#' \code{F.star(t) = (t(F(t)), t(z(t)))} By Euler's theorem \cr \code{ES.fm =
#' sum(cES.fm) = sum(beta.star*mcES.fm)} \cr
#' 
#' @param Data \code{B x (k+2)} matrix of historic or simulated data. The first
#' column contains the fund returns, the second through \code{k+1}st columns
#' contain the returns on the \code{k} factors, and the \code{(k+2)}nd column
#' contain residuals scaled to have unit variance.
#' @param beta.vec \code{k x 1} vector of factor betas.
#' @param sig2.e scalar, residual variance from factor model.
#' @param tail.prob scalar, tail probability for VaR quantile. Typically 0.01
#' or 0.05.
#' @return A list with the following components:
#' @returnItem VaR Scalar, nonparametric VaR value for fund reported as a
#' positive number.
#' @returnItem n.exceed Scalar, number of observations beyond VaR.
#' @returnItem idx.exceed \code{n.exceed x 1} vector giving index values of
#' exceedences.
#' @returnItem ES scalar, nonparametric ES value for fund reported as a
#' positive number.
#' @returnItem mcES \code{(K+1) x 1} vector of factor marginal contributions to
#' ES.
#' @returnItem cES \code{(K+1) x 1} vector of factor component contributions to
#' ES.
#' @returnItem pcES \code{(K+1) x 1} vector of factor percent contributions to
#' ES.
#' @author Eric Zviot and Yi-An Chen.
#' @references 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A
#' General Analysis", \emph{The Journal of Risk} 5/2. \cr 2. Yamai and Yoshiba
#' (2002). "Comparative Analyses of Expected Shortfall and Value-at-Risk: Their
#' Estimation Error, Decomposition, and Optimization", Bank of Japan. \cr 3.
#' Meucci (2007). "Risk Contributions from Generic User-Defined Factors,"
#' \emph{Risk}.
#' @examples
#' 
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' factors    = managers.df[,(7:9)]
#' # fit the factor model with OLS
#' fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
#'                                  variable.selection="all subsets",factor.set=3)
#' # risk factor contribution to ETL
#' # combine fund returns, factor returns and residual returns for HAM1
#' tmpData = cbind(ret.assets[,1], factors,
#'                residuals(fit$asset.fit$HAM1)/sqrt(fit$residVars.vec[1]))
#' colnames(tmpData)[c(1,5)] = c("HAM1", "residual")
#' factor.es.decomp.HAM1 = factorModelEsDecomposition(tmpData, fit$beta.mat[1,],
#'                                            fit$residVars.vec[1], tail.prob=0.05)
#' 
#' 
#' 
factorModelEsDecomposition <-
function(Data, beta.vec, sig2.e, tail.prob = 0.05) {
## Compute factor model factor ES decomposition based on Euler's theorem given historic 
## or simulated data and factor model parameters.
## The partial derivative of ES wrt factor beta is computed
## as the expected factor return given fund return is less than or equal to its VaR
## VaR is compute either as the sample quantile or as an estimated quantile
## using the Cornish-Fisher expansion
## inputs:
## Data       B x (k+2) matrix of data. First column contains the fund returns,
##            second through k+1 columns contain factor returns, (k+2)nd column contain residuals
##            scaled to have variance 1.
## beta.vec   k x 1 vector of factor betas
## sig2.e			scalar, residual variance from factor model
## tail.prob  scalar tail probability
## output:
## A list with the following components:
## VaR              scalar, nonparametric VaR value for fund reported as a positive number
## n.exceed         scalar, number of observations beyond VaR
## idx.exceed       n.exceed x 1 vector giving index values of exceedences
## ES               scalar, nonparametric ES value for fund reported as a positive number
## mcES             k+1 x 1 vector of factor marginal contributions to ES
## cES              k+1 x 1 vector of factor component contributions to ES
## pcES             k+1 x 1 vector of factor percent contributions to ES
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

  VaR.fm = quantile(Data[, 1], prob=tail.prob)
  idx = which(Data[, 1] <= VaR.fm)
  ES.fm = -mean(Data[idx, 1])
  
  ##
  ## compute marginal contribution to ES
  ##
  ## compute marginal ES as expected value of factor return given fund
  ## return is less than or equal to VaR
    mcES.fm = -as.matrix(colMeans(Data[idx, -1]))
  
## compute correction factor so that sum of weighted marginal ES adds to portfolio ES
#cf = as.numeric( ES.fm / sum(mcES.fm*beta.star.vec) )
#mcES.fm = cf*mcES.fm
cES.fm = mcES.fm*beta.star.vec
pcES.fm = cES.fm/ES.fm
colnames(mcES.fm) = "MCES"
colnames(cES.fm) = "CES"
colnames(pcES.fm) = "PCES"
ans = list(VaR = -VaR.fm,
           n.exceed = length(idx),
           idx.exceed = idx,
           ES = ES.fm, 
           mcES = t(mcES.fm), 
           cES = t(cES.fm),
           pcES = t(pcES.fm))
return(ans)
}

