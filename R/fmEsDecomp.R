#' @title Decompose ES into individual factor contributions
#' 
#' @description Compute the factor contributions to Expected Tail Loss or 
#' Expected Shortfall (ES) of assets' returns  based on Euler's theorem, given 
#' the fitted factor model. The partial derivative of ES with respect to factor 
#' beta is computed as the expected factor return given fund return is less 
#' than or equal to its value-at-risk (VaR). VaR is computed as the sample quantile of the historic or 
#' simulated data.
#' 
#' @details The factor model for an asset's return at time \code{t} has the 
#' form \cr \cr \code{R(t) = beta'f(t) + e(t) = beta.star'f.star(t)} \cr \cr 
#' where, \code{beta.star=(beta,sig.e)} and \code{f.star(t)=[f(t)',z(t)]'}. By 
#' Euler's theorem, the ES of the asset's return is given by:
#' \cr \cr \code{ES.fm = sum(cES_k) = sum(beta.star_k*mES_k)} \cr \cr
#' where, summation is across the \code{K} factors and the residual, 
#' \code{cES} and \code{mES} are the component and marginal 
#' contributions to \code{ES} respectively. The marginal contribution to ES is
#' defined as the expected value of \code{F.star}, conditional on the loss 
#' being less than or equal to \code{VaR.fm}. This is estimated as a sample 
#' average of the observations in that data window. 
#' 
#' Computation of the VaR measure is done using 
#' \code{\link[PerformanceAnalytics]{VaR}}. Arguments \code{p}, \code{method} 
#' and \code{invert} are passed to this function. Refer to their help file for 
#' details and other options. \code{invert} consistently affects the sign for 
#' all VaR and ES measures.
#' 
#' @param object fit object of class \code{tsfm}, \code{sfm} or \code{ffm}.
#' @param p confidence level for calculation. Default is 0.95.
#' @param method method for computing VaR, one of "modified","gaussian",
#' "historical", "kernel". Default is "modified". See details.
#' @param invert logical; whether to invert the VaR measure. Default is 
#' \code{FALSE}.
#' @param ... other optional arguments passed to 
#' \code{\link[PerformanceAnalytics]{VaR}}.
#' 
#' @return A list containing 
#' \item{ES.fm}{length-N vector of factor model ES of N-asset returns.}
#' \item{n.exceed}{length-N vector of number of observations beyond VaR for 
#' each asset.}
#' \item{idx.exceed}{list of numeric vector of index values of exceedances.}
#' \item{mES}{N x (K+1) matrix of marginal contributions to VaR.}
#' \item{cES}{N x (K+1) matrix of component contributions to VaR.}
#' \item{pcES}{N x (K+1) matrix of percentage component contributions to VaR.}
#' Where, \code{K} is the number of factors and N is the number of assets. 
#' 
#' @author Eric Zviot, Sangeetha Srinivasan and Yi-An Chen
#' 
#' @references 
#' Epperlein, E., & Smillie, A. (2006). Portfolio risk analysis Cracking VAR 
#' with kernels. RISK-LONDON-RISK MAGAZINE LIMITED-, 19(8), 70.
#' 
#' Hallerback (2003). Decomposing Portfolio Value-at-Risk: A General Analysis. 
#' The Journal of Risk, 5(2), 1-18.
#' 
#' Meucci, A. (2007). Risk contributions from generic user-defined factors. 
#' RISK-LONDON-RISK MAGAZINE LIMITED-, 20(6), 84. 
#' 
#' Yamai, Y., & Yoshiba, T. (2002). Comparative analyses of expected shortfall 
#' and value-at-risk: their estimation error, decomposition, and optimization. 
#' Monetary and economic studies, 20(1), 87-121.
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{fitSfm}}, \code{\link{fitFfm}}
#' for the different factor model fitting functions.
#' 
#' \code{\link[PerformanceAnalytics]{VaR}} for VaR computation.
#' \code{\link{fmSdDecomp}} for factor model SD decomposition.
#' \code{\link{fmVaRDecomp}} for factor model VaR decomposition.
#' 
#' @examples
#' # Time Series Factor Model
#' data(managers)
#' fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:8)]), data=managers)
#' ES.decomp <- fmEsDecomp(fit.macro)
#' # get the component contributions
#' ES.decomp$cES
#' 
#' # Statistical Factor Model
#' data(StockReturns)
#' sfm.pca.fit <- fitSfm(r.M, k=2)
#' ES.decomp <- fmEsDecomp(sfm.pca.fit)
#' ES.decomp$cES
#' 
#' @importFrom PerformanceAnalytics VaR
#' 
#' @export

fmEsDecomp <- function(object, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
  }
  UseMethod("fmEsDecomp")
}

#' @rdname fmEsDecomp
#' @method fmEsDecomp tsfm
#' @export

fmEsDecomp.tsfm <- function(object, p=0.95, 
                            method=c("modified","gaussian","historical",
                                     "kernel"), invert=FALSE, ...) {
  
  # set defaults and check input vailidity
  method = method[1]
  
  if (!(method %in% c("modified", "gaussian", "historical", "kernel"))) {
    stop("Invalid argument: method must be 'modified', 'gaussian',
         'historical' or 'kernel'")
  }
  
  # get beta.star
  beta <- object$beta
  beta[is.na(beta)] <- 0
  beta.star <- as.matrix(cbind(beta, object$resid.sd))
  colnames(beta.star)[dim(beta.star)[2]] <- "residual"
  
  # factor returns and residuals data
  factors.xts <- object$data[,object$factor.names]
  resid.xts <- as.xts(t(t(residuals(object))/object$resid.sd))
  time(resid.xts) <- as.Date(time(resid.xts))
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, N)
  ES.fm <- rep(NA, N)
  idx.exceed <- list()
  n.exceed <- rep(NA, N)
  names(VaR.fm) = names(ES.fm) = names(n.exceed) = object$asset.names
  mES <- matrix(NA, N, K+1)
  cES <- matrix(NA, N, K+1)
  pcES <- matrix(NA, N, K+1)
  rownames(mES)=rownames(cES)=rownames(pcES)=object$asset.names
  colnames(mES)=colnames(cES)=colnames(pcES)=c(object$factor.names,"residuals")
  
  for (i in object$asset.names) {
    # return data for asset i
    R.xts <- object$data[,i]
    # get VaR for asset i
    VaR.fm[i] <- VaR(R.xts, p=p, method=method, invert=invert, ...)
    # index of VaR exceedances
    idx.exceed[[i]] <- which(R.xts <= VaR.fm[i])
    # number of VaR exceedances
    n.exceed[i] <- length(idx.exceed[[i]])
    
    # get F.star data object
    factor.star <- merge(factors.xts, resid.xts[,i])
    colnames(factor.star)[dim(factor.star)[2]] <- "residual"
    
    if (!invert) {inv=-1} else {inv=1}
    
    # compute ES as expected value of asset return, such that the given asset 
    # return is less than or equal to its value-at-risk (VaR) and approximated
    # by a kernel estimator.
    idx <- which(R.xts <= inv*VaR.fm[i])
    ES.fm[i] <- inv * mean(R.xts[idx], na.rm =TRUE)
    
    # compute marginal ES as expected value of factor returns, such that the
    # given asset return is less than or equal to its value-at-risk (VaR) and 
    # approximated by a kernel estimator.
    mES[i,] <- inv * colMeans(factor.star[idx,], na.rm =TRUE)
    
    # correction factor to ensure that sum(cES) = portfolio ES
    cf <- as.numeric( ES.fm[i] / sum(mES[i,]*beta.star[i,], na.rm=TRUE) )
    
    # compute marginal, component and percentage contributions to ES
    # each of these have dimensions: N x (K+1)
    mES[i,] <- cf * mES[i,]
    cES[i,] <- mES[i,] * beta.star[i,]
    pcES[i,] <- 100* cES[i,] / ES.fm[i]
  }
  
  fm.ES.decomp <- list(ES.fm=ES.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                       mES=mES, cES=cES, pcES=pcES)
  
  return(fm.ES.decomp)
}

#' @rdname fmEsDecomp
#' @method fmEsDecomp sfm
#' @export

fmEsDecomp.sfm <- function(object, p=0.95, 
                            method=c("modified","gaussian","historical",
                                     "kernel"), invert=FALSE, ...) {
  
  # set defaults and check input vailidity
  method = method[1]
  
  if (!(method %in% c("modified", "gaussian", "historical", "kernel"))) {
    stop("Invalid argument: method must be 'modified', 'gaussian',
         'historical' or 'kernel'")
  }
  
  # get beta.star
  beta <- object$loadings
  beta[is.na(beta)] <- 0
  beta.star <- as.matrix(cbind(beta, object$resid.sd))
  colnames(beta.star)[dim(beta.star)[2]] <- "residual"
  
  # factor returns and residuals data
  factors.xts <- object$factors
  resid.xts <- as.xts(t(t(residuals(object))/object$resid.sd))
  time(resid.xts) <- as.Date(time(resid.xts))
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- object$k
  VaR.fm <- rep(NA, N)
  ES.fm <- rep(NA, N)
  idx.exceed <- list()
  n.exceed <- rep(NA, N)
  names(VaR.fm) = names(ES.fm) = names(n.exceed) = object$asset.names
  mES <- matrix(NA, N, K+1)
  cES <- matrix(NA, N, K+1)
  pcES <- matrix(NA, N, K+1)
  rownames(mES)=rownames(cES)=rownames(pcES)=object$asset.names
  colnames(mES)=colnames(cES)=colnames(pcES)=c(paste("F",1:K,sep="."),
                                               "residuals")
  
  for (i in object$asset.names) {
    # return data for asset i
    R.xts <- object$data[,i]
    # get VaR for asset i
    VaR.fm[i] <- VaR(R.xts, p=p, method=method, invert=invert, ...)
    # index of VaR exceedances
    idx.exceed[[i]] <- which(R.xts <= VaR.fm[i])
    # number of VaR exceedances
    n.exceed[i] <- length(idx.exceed[[i]])
    
    # get F.star data object
    factor.star <- merge(factors.xts, resid.xts[,i])
    colnames(factor.star)[dim(factor.star)[2]] <- "residual"
    
    if (!invert) {inv=-1} else {inv=1}
    
    # compute ES as expected value of asset return, such that the given asset 
    # return is less than or equal to its value-at-risk (VaR) and approximated
    # by a kernel estimator.
    idx <- which(R.xts <= inv*VaR.fm[i])
    ES.fm[i] <- inv * mean(R.xts[idx], na.rm =TRUE)
    
    # compute marginal ES as expected value of factor returns, such that the
    # given asset return is less than or equal to its value-at-risk (VaR) and 
    # approximated by a kernel estimator.
    mES[i,] <- inv * colMeans(factor.star[idx,], na.rm =TRUE)
    
    # correction factor to ensure that sum(cES) = portfolio ES
    cf <- as.numeric( ES.fm[i] / sum(mES[i,]*beta.star[i,], na.rm=TRUE) )
    
    # compute marginal, component and percentage contributions to ES
    # each of these have dimensions: N x (K+1)
    mES[i,] <- cf * mES[i,]
    cES[i,] <- mES[i,] * beta.star[i,]
    pcES[i,] <- 100* cES[i,] / ES.fm[i]
  }
  
  fm.ES.decomp <- list(ES.fm=ES.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                       mES=mES, cES=cES, pcES=pcES)
  
  return(fm.ES.decomp)
}

