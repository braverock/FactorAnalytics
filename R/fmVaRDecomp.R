#' @title Decompose VaR into individual factor contributions
#' 
#' @description Compute the factor contributions to Value-at-Risk (VaR) of 
#' assets' returns based on Euler's theorem, given the fitted factor model. 
#' The partial derivative of VaR w.r.t. factor beta is computed as the expected 
#' factor return given fund return is equal to its VaR and approximated by a
#' kernel estimator. Option to choose between non-parametric and Normal.
#' 
#' @details The factor model for an asset's return at time \code{t} has the 
#' form \cr \cr \code{R(t) = beta'f(t) + e(t) = beta.star'f.star(t)} \cr \cr 
#' where, \code{beta.star=(beta,sig.e)} and \code{f.star(t)=[f(t)',z(t)]'}. By 
#' Euler's theorem, the VaR of the asset's return is given by: 
#' \cr \cr \code{VaR.fm = sum(cVaR_k) = sum(beta.star_k*mVaR_k)} \cr \cr 
#' where, summation is across the \code{K} factors and the residual, 
#' \code{cVaR} and \code{mVaR} are the component and marginal 
#' contributions to \code{VaR} respectively. The marginal contribution to VaR 
#' is defined as the expectation of \code{F.star}, conditional on the loss 
#' being equal to \code{VaR.fm}. This is approximated as described in 
#' Epperlein & Smillie (2006); a triangular smoothing kernel is used here. 
#' 
#' 
#' @param object fit object of class \code{tsfm}, \code{sfm} or \code{ffm}.
#' @param factor.cov optional user specified factor covariance matrix with 
#' named columns; defaults to the sample covariance matrix.
#' @param p confidence level for calculation. Default is 0.95.
#' @param type one of "np" (non-parametric) or "normal" for calculating VaR. 
#' Default is "np".
#' @param use method for computing covariances in the presence of missing 
#' values; one of "everything", "all.obs", "complete.obs", "na.or.complete", or 
#' "pairwise.complete.obs". Default is "pairwise.complete.obs".
#' @param ... other optional arguments passed to \code{\link[stats]{quantile}}.
#' 
#' @return A list containing 
#' \item{VaR.fm}{length-N vector of factor model VaRs of N-asset returns.}
#' \item{n.exceed}{length-N vector of number of observations beyond VaR for 
#' each asset.}
#' \item{idx.exceed}{list of numeric vector of index values of exceedances.}
#' \item{mVaR}{N x (K+1) matrix of marginal contributions to VaR.}
#' \item{cVaR}{N x (K+1) matrix of component contributions to VaR.}
#' \item{pcVaR}{N x (K+1) matrix of percentage component contributions to VaR.}
#' Where, \code{K} is the number of factors and N is the number of assets.
#' 
#' @author Eric Zivot, Yi-An Chen and Sangeetha Srinivasan
#' 
#' @references 
#' Eric Zivot's slides from CFRM 546: Estimating risk measures: Portfolio of 
#' Assets, April 28, 2015.
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
#' \code{\link{fmSdDecomp}} for factor model SD decomposition.
#' \code{\link{fmEsDecomp}} for factor model ES decomposition.
#' 
#' @examples
#' # Time Series Factor Model
#' data(managers)
#' fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:8)]), data=managers)
#'                      
#' VaR.decomp <- fmVaRDecomp(fit.macro)
#' # get the component contributions
#' VaR.decomp$cVaR
#' 
#' # Statistical Factor Model
#' data(StockReturns)
#' sfm.pca.fit <- fitSfm(r.M, k=2)
#' 
#' VaR.decomp <- fmVaRDecomp(sfm.pca.fit, type="normal")
#' VaR.decomp$cVaR
#' 
#' # Fundamental Factor Model
#' data(Stock.df)
#' exposure.vars <- c("BOOK2MARKET", "LOG.MARKETCAP")
#' fit <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars=exposure.vars)
#' 
#' VaR.decomp <- fmVaRDecomp(fit, type="normal")
#' VaR.decomp$cVaR
#' 
#' @export

fmVaRDecomp <- function(object, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
  }
  UseMethod("fmVaRDecomp")
}

#' @rdname fmVaRDecomp
#' @method fmVaRDecomp tsfm
#' @export

fmVaRDecomp.tsfm <- function(object, factor.cov, p=0.95, type=c("np","normal"), 
                             use="pairwise.complete.obs", ...) {
  
  # set default for type
  type = type[1]
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
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
  
  if (type=="normal") {
    # get cov(F): K x K
    if (missing(factor.cov)) {
      factor.cov = cov(as.matrix(factors.xts), use=use, ...)
    } else {
      if (!identical(dim(factor.cov), as.integer(c(ncol(factor), ncol(factor))))) {
        stop("Dimensions of user specified factor covariance matrix are not
           compatible with the number of factors in the fitTsfm object")
      }
    }
    
    # get cov(F.star): (K+1) x (K+1)
    K <- ncol(object$beta)
    factor.star.cov <- diag(K+1)
    factor.star.cov[1:K, 1:K] <- factor.cov
    colnames(factor.star.cov) <- c(colnames(factor.cov),"residuals")
    rownames(factor.star.cov) <- c(colnames(factor.cov),"residuals")
    
    # factor expected returns
    MU <- c(colMeans(factors.xts, na.rm=TRUE), 0)
    names(MU) <- colnames(beta.star)
    
    # SIGMA*Beta to compute normal mVaR
    SIGB <-  beta.star %*% factor.star.cov
  }
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, N)
  idx.exceed <- list()
  n.exceed <- rep(NA, N)
  names(VaR.fm) = names(n.exceed) = object$asset.names
  mVaR <- matrix(NA, N, K+1)
  cVaR <- matrix(NA, N, K+1)
  pcVaR <- matrix(NA, N, K+1)
  rownames(mVaR)=rownames(cVaR)=rownames(pcVaR)=object$asset.names
  colnames(mVaR)=colnames(cVaR)=colnames(pcVaR)=c(object$factor.names, "residuals")
  
  for (i in object$asset.names) {
    # return data for asset i
    R.xts <- object$data[,i]
    # get VaR for asset i
    if (type=="np") {
      VaR.fm[i] <- quantile(R.xts, probs=1-p, na.rm=TRUE, ...)
    }
    else if (type=="normal") {
      VaR.fm[i] <- beta.star[i,] %*% MU + 
        sqrt(beta.star[i,,drop=F] %*% factor.star.cov %*% t(beta.star[i,,drop=F]))*qnorm(1-p)
    }
    # index of VaR exceedances
    idx.exceed[[i]] <- which(R.xts <= VaR.fm[i])
    # number of VaR exceedances
    n.exceed[i] <- length(idx.exceed[[i]])
    
    if (type=="np") {
      # get F.star data object
      factor.star <- merge(factors.xts, resid.xts[,i])
      colnames(factor.star)[dim(factor.star)[2]] <- "residual"
      
      # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
      # the constant 2.575 corresponds to a triangular kernel 
      eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
      
      # compute marginal VaR as expected value of factor returns, such that the
      # asset return was incident in the triangular kernel region peaked at the 
      # VaR value and bandwidth = epsilon.
      k.weight <- as.vector(1 - abs(R.xts - VaR.fm[i]) / eps)
      k.weight[k.weight<0] <- 0
      mVaR[i,] <- colMeans(factor.star*k.weight, na.rm =TRUE)
    } 
    else if (type=="normal")  {
      mVaR[i,] <- t(MU) + SIGB[i,] * qnorm(1-p)/sd(R.xts, na.rm=TRUE)
    }
    
    # correction factor to ensure that sum(cVaR) = asset VaR
    cf <- as.numeric( VaR.fm[i] / sum(mVaR[i,]*beta.star[i,], na.rm=TRUE) )
    
    # compute marginal, component and percentage contributions to VaR
    # each of these have dimensions: N x (K+1)
    mVaR[i,] <- cf * mVaR[i,]
    cVaR[i,] <- mVaR[i,] * beta.star[i,]
    pcVaR[i,] <- 100* cVaR[i,] / VaR.fm[i]
  }
  
  fm.VaR.decomp <- list(VaR.fm=VaR.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                        mVaR=mVaR, cVaR=cVaR, pcVaR=pcVaR)
  
  return(fm.VaR.decomp)
}

#' @rdname fmVaRDecomp
#' @method fmVaRDecomp sfm
#' @export

fmVaRDecomp.sfm <- function(object, factor.cov, p=0.95, type=c("np","normal"), 
                            use="pairwise.complete.obs", ...) {
  
  # set default for type
  type = type[1]
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
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
  
  if (type=="normal") {
    # get cov(F): K x K
    if (missing(factor.cov)) {
      factor.cov = cov(as.matrix(factors.xts), use=use, ...) 
    } else {
      if (!identical(dim(factor.cov), as.integer(c(object$k, object$k)))) {
        stop("Dimensions of user specified factor covariance matrix are not 
             compatible with the number of factors in the fitSfm object")
      }
    }
    
    # get cov(F.star): (K+1) x (K+1)
    K <- object$k
    factor.star.cov <- diag(K+1)
    factor.star.cov[1:K, 1:K] <- factor.cov
    colnames(factor.star.cov) <- c(colnames(factor.cov),"residuals")
    rownames(factor.star.cov) <- c(colnames(factor.cov),"residuals")
    
    # factor expected returns
    MU <- c(colMeans(factors.xts, na.rm=TRUE), 0)
    
    # SIGMA*Beta to compute normal mVaR
    SIGB <- beta.star %*% factor.star.cov
  }
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- object$k
  VaR.fm <- rep(NA, N)
  idx.exceed <- list()
  n.exceed <- rep(NA, N)
  names(VaR.fm) = names(n.exceed) = object$asset.names
  mVaR <- matrix(NA, N, K+1)
  cVaR <- matrix(NA, N, K+1)
  pcVaR <- matrix(NA, N, K+1)
  rownames(mVaR)=rownames(cVaR)=rownames(pcVaR)=object$asset.names
  colnames(mVaR)=colnames(cVaR)=colnames(pcVaR)=c(paste("F",1:K,sep="."),"residuals")
  
  for (i in object$asset.names) {
    # return data for asset i
    R.xts <- object$data[,i]
    # get VaR for asset i
    if (type=="np") {
      VaR.fm[i] <- quantile(R.xts, probs=1-p, na.rm=TRUE)
    }
    else if (type=="normal") {
      VaR.fm[i] <- beta.star[i,] %*% MU + 
        sqrt(beta.star[i,,drop=F] %*% factor.star.cov %*% t(beta.star[i,,drop=F]))*qnorm(1-p)
    }
    # index of VaR exceedances
    idx.exceed[[i]] <- which(R.xts <= VaR.fm[i])
    # number of VaR exceedances
    n.exceed[i] <- length(idx.exceed[[i]])
    
    if (type=="np") {
      # get F.star data object
      factor.star <- merge(factors.xts, resid.xts[,i])
      colnames(factor.star)[dim(factor.star)[2]] <- "residual"
      
      # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
      # the constant 2.575 corresponds to a triangular kernel 
      eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
      
      # compute marginal VaR as expected value of factor returns, such that the
      # asset return was incident in the triangular kernel region peaked at the 
      # VaR value and bandwidth = epsilon.
      k.weight <- as.vector(1 - abs(R.xts - VaR.fm[i]) / eps)
      k.weight[k.weight<0] <- 0
      mVaR[i,] <- colMeans(factor.star*k.weight, na.rm =TRUE)
    } 
    else if (type=="normal")  {
      mVaR[i,] <- t(MU) + SIGB[i,] * qnorm(1-p)/sd(R.xts, na.rm=TRUE)
    }
    
    # correction factor to ensure that sum(cVaR) = asset VaR
    cf <- as.numeric( VaR.fm[i] / sum(mVaR[i,]*beta.star[i,], na.rm=TRUE) )
    
    # compute marginal, component and percentage contributions to VaR
    # each of these have dimensions: N x (K+1)
    mVaR[i,] <- cf * mVaR[i,]
    cVaR[i,] <- mVaR[i,] * beta.star[i,]
    pcVaR[i,] <- 100* cVaR[i,] / VaR.fm[i]
  }
  
  fm.VaR.decomp <- list(VaR.fm=VaR.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                        mVaR=mVaR, cVaR=cVaR, pcVaR=pcVaR)
  
  return(fm.VaR.decomp)
}

#' @rdname fmVaRDecomp
#' @method fmVaRDecomp ffm
#' @export

fmVaRDecomp.ffm <- function(object, factor.cov, p=0.95, type=c("np","normal"), 
                            use="pairwise.complete.obs", ...) {
  
  # set default for type
  type = type[1]
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  which.numeric <- sapply(object$data[,object$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- object$exposure.vars[which.numeric]
  exposures.char <- object$exposure.vars[!which.numeric]
  
  # factor returns and residuals data
  factors.xts <- object$factor.returns
  resid.xts <- as.xts(t(t(residuals(object))/sqrt(object$resid.var)))
  time(resid.xts) <- as.Date(time(resid.xts))
  
  # get cov(F): K x K
  if (missing(factor.cov)) {
    factor.cov <- object$factor.cov
  } else {
    if (!identical(dim(factor.cov), dim(object$factor.cov))) {
      stop("Dimensions of user specified factor covariance matrix are not 
           compatible with the number of factors in the fitSfm object")
    }
  }
  
  # get beta.star
  beta <- object$beta
  beta[is.na(beta)] <- 0
  
  # re-order beta to match with factor.cov when both sector & style factors are used 
  if(length(exposures.char)>0 & length(exposures.num)>0){
    sectors.sec <- levels(object$data[,exposures.char])
    sectors.names <- paste(exposures.char,sectors.sec,sep="")
    
    for(i in 1:length(sectors.sec)){
      colnames(beta)[colnames(beta) == sectors.names[i]] = sectors.sec[i]
    }
    
    if(type == 'np'){
      beta = beta[,colnames(factors.xts)]
    }
    else if(type == 'normal'){
      beta = beta[,colnames(factor.cov)]
    }
  }
  
  beta.star <- as.matrix(cbind(beta, sqrt(object$resid.var)))
  colnames(beta.star)[dim(beta.star)[2]] <- "residual"
  
  
  if (type=="normal") {
    
    # get cov(F.star): (K+1) x (K+1)
    K <- ncol(object$beta)
    factor.star.cov <- diag(K+1)
    factor.star.cov[1:K, 1:K] <- factor.cov
    colnames(factor.star.cov) <- c(colnames(factor.cov),"residuals")
    rownames(factor.star.cov) <- c(colnames(factor.cov),"residuals")
    
    # factor expected returns
    MU <- c(colMeans(factors.xts, na.rm=TRUE), 0)
    
    # SIGMA*Beta to compute normal mVaR
    SIGB <-  beta.star %*% factor.star.cov
  }
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, N)
  idx.exceed <- list()
  n.exceed <- rep(NA, N)
  names(VaR.fm) = names(n.exceed) = object$asset.names
  mVaR <- matrix(NA, N, K+1)
  cVaR <- matrix(NA, N, K+1)
  pcVaR <- matrix(NA, N, K+1)
  rownames(mVaR)=rownames(cVaR)=rownames(pcVaR)=object$asset.names
  colnames(mVaR)=colnames(cVaR)=colnames(pcVaR)=c(object$factor.names, "residuals")
  
  for (i in object$asset.names) {
    # return data for asset i
    subrows <- which(object$data[[object$asset.var]]==i)
    R.xts <- as.xts(object$data[subrows,object$ret.var], 
                    as.Date(object$data[subrows,object$date.var]))
    # get VaR for asset i
    if (type=="np") {
      VaR.fm[i] <- quantile(R.xts, probs=1-p, na.rm=TRUE, ...)
    }
    else if (type=="normal") {
      VaR.fm[i] <- beta.star[i,] %*% MU + 
        sqrt(beta.star[i,,drop=F] %*% factor.star.cov %*% t(beta.star[i,,drop=F]))*qnorm(1-p)
    }
    # index of VaR exceedances
    idx.exceed[[i]] <- which(R.xts <= VaR.fm[i])
    # number of VaR exceedances
    n.exceed[i] <- length(idx.exceed[[i]])
    
    if (type=="np") {
      # get F.star data object
      factor.star <- merge(factors.xts, resid.xts[,i])
      colnames(factor.star)[dim(factor.star)[2]] <- "residual"
      
      # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
      # the constant 2.575 corresponds to a triangular kernel 
      eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
      
      # compute marginal VaR as expected value of factor returns, such that the
      # asset return was incident in the triangular kernel region peaked at the 
      # VaR value and bandwidth = epsilon.
      k.weight <- as.vector(1 - abs(R.xts - VaR.fm[i]) / eps)
      k.weight[k.weight<0] <- 0
      mVaR[i,] <- colMeans(factor.star*k.weight, na.rm =TRUE)
    } 
    else if (type=="normal")  {
      mVaR[i,] <- t(MU) + SIGB[i,] * qnorm(1-p)/sd(R.xts, na.rm=TRUE)
    }
    
    # correction factor to ensure that sum(cVaR) = asset VaR
    cf <- as.numeric( VaR.fm[i] / sum(mVaR[i,]*beta.star[i,], na.rm=TRUE) )
    
    # compute marginal, component and percentage contributions to VaR
    # each of these have dimensions: N x (K+1)
    mVaR[i,] <- cf * mVaR[i,]
    cVaR[i,] <- mVaR[i,] * beta.star[i,]
    pcVaR[i,] <- 100* cVaR[i,] / VaR.fm[i]
  }
  
  fm.VaR.decomp <- list(VaR.fm=VaR.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                        mVaR=mVaR, cVaR=cVaR, pcVaR=pcVaR)
  
  return(fm.VaR.decomp)
}