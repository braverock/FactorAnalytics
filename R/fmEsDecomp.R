#' @title Decompose ES into individual factor contributions
#' 
#' @description Compute the factor contributions to Expected Tail Loss or 
#' Expected Shortfall (ES) of assets' returns  based on Euler's theorem, given 
#' the fitted factor model. The partial derivative of ES with respect to factor 
#' beta is computed as the expected factor return given fund return is less 
#' than or equal to its value-at-risk (VaR). Option to choose between 
#' non-parametric and Normal.
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
#' Refer to Eric Zivot's slides (referenced) for formulas pertaining to the 
#' calculation of Normal ES (adapted from a portfolio context to factor models).
#' 
#' @param object fit object of class \code{tsfm}, \code{sfm} or \code{ffm}.
#' @param factor.cov optional user specified factor covariance matrix with 
#' named columns; defaults to the sample covariance matrix.
#' @param p tail probability for calculation. Default is 0.05.
#' @param type one of "np" (non-parametric) or "normal" for calculating VaR. 
#' Default is "np".
#' @param use method for computing covariances in the presence of missing 
#' values; one of "everything", "all.obs", "complete.obs", "na.or.complete", or 
#' "pairwise.complete.obs". Default is "pairwise.complete.obs".
#' @param ... other optional arguments passed to \code{\link[stats]{quantile}}.
#' 
#' @return A list containing 
#' \item{ES.fm}{length-N vector of factor model ES of N-asset returns.}
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
#' \code{\link{fmSdDecomp}} for factor model SD decomposition.
#' \code{\link{fmVaRDecomp}} for factor model VaR decomposition.
#' 
#' @examples
#'  # Time Series Factor Model
#'  # load data
#' data(managers, package = 'PerformanceAnalytics')
#' colnames(managers)
#'  # Make syntactically valid column names
#' colnames(managers) <- make.names( colnames(managers))
#' colnames(managers)
#' 
#' fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:8)]), data=managers)
#' ES.decomp <- fmEsDecomp(fit.macro)
#' # get the component contributions
#' ES.decomp$cES
#' 
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

fmEsDecomp.tsfm <- function(object, factor.cov, p=0.05, type=c("np","normal"),  
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
  colnames(beta.star)[dim(beta.star)[2]] <- "Residuals"
  
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
    colnames(factor.star.cov) <- c(colnames(factor.cov),"Residuals")
    rownames(factor.star.cov) <- c(colnames(factor.cov),"Residuals")
    # factor expected returns
    MU <- c(colMeans(factors.xts, na.rm=TRUE), 0)
    names(MU) <- colnames(beta.star)
    # SIGMA*Beta to compute normal mES
    SIGB <-  beta.star %*% factor.star.cov
  }
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, N)
  ES.fm <- rep(NA, N)
  idx.exceed <- list()
  names(VaR.fm) = names(ES.fm) = object$asset.names
  mES <- matrix(NA, N, K+1)
  cES <- matrix(NA, N, K+1)
  pcES <- matrix(NA, N, K+1)
  rownames(mES)=rownames(cES)=rownames(pcES)=object$asset.names
  colnames(mES)=colnames(cES)=colnames(pcES)=c(object$factor.names,"Residuals")
  
  for (i in object$asset.names) {
    # return data for asset i
    R.xts <- object$data[,i]
    
    if (type=="np") { 
      # get VaR for asset i
      VaR.fm[i] <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
      # index of VaR exceedances
      idx.exceed[[i]] <- which(R.xts <= VaR.fm[i])
      # compute ES as expected value of asset return, such that the given asset 
      # return is less than or equal to its value-at-risk (VaR)
      ES.fm[i] <- mean(R.xts[idx.exceed[[i]]], na.rm =TRUE)
      # get F.star data object
      factor.star <- merge(factors.xts, resid.xts[,i])
      colnames(factor.star)[dim(factor.star)[2]] <- "Residuals"
      # compute marginal ES as expected value of factor returns, when the asset's 
      # return is less than or equal to its value-at-risk (VaR)
      mES[i,] <- colMeans(factor.star[idx.exceed[[i]],], na.rm =TRUE)
      
    } else if (type=="normal") {
      # extract vector of factor model loadings for asset i
      beta.i <- beta.star[i,,drop=F]
      # compute ES
      ES.fm[i] <- -(beta.star[i,] %*% MU + sqrt(beta.i %*% factor.star.cov %*% t(beta.i))*dnorm(qnorm(p))/(p)) 
      # compute marginal ES
      mES[i,] <- -(t(MU) + SIGB[i,]/sd(R.xts, na.rm=TRUE) * dnorm(qnorm(p))/(p))
    }
    
    # correction factor to ensure that sum(cES) = asset ES
    cf <- as.numeric( ES.fm[i] / sum(mES[i,]*beta.star[i,], na.rm=TRUE) )
    
    # compute marginal, component and percentage contributions to ES
    # each of these have dimensions: N x (K+1)
    mES[i,] <- cf * mES[i,]
    cES[i,] <- mES[i,] * beta.star[i,]
    pcES[i,] <- 100* cES[i,] / ES.fm[i]
  }
  
  fm.ES.decomp <- list(ES.fm=ES.fm, mES=mES, cES=cES, pcES=pcES)
  
  return(fm.ES.decomp)
}

#' @rdname fmEsDecomp
#' @method fmEsDecomp sfm
#' @export

fmEsDecomp.sfm <- function(object, factor.cov, p=0.05, type=c("np","normal"),  
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
  colnames(beta.star)[dim(beta.star)[2]] <- "Residuals"
  
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
    colnames(factor.star.cov) <- c(colnames(factor.cov),"Residuals")
    rownames(factor.star.cov) <- c(colnames(factor.cov),"Residuals")
    # factor expected returns
    MU <- c(colMeans(factors.xts, na.rm=TRUE), 0)
    # SIGMA*Beta to compute normal mVaR
    SIGB <- beta.star %*% factor.star.cov
  }
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- object$k
  VaR.fm <- rep(NA, N)
  ES.fm <- rep(NA, N)
  idx.exceed <- list()
  names(VaR.fm) = names(ES.fm) = object$asset.names
  mES <- matrix(NA, N, K+1)
  cES <- matrix(NA, N, K+1)
  pcES <- matrix(NA, N, K+1)
  rownames(mES)=rownames(cES)=rownames(pcES)=object$asset.names
  colnames(mES)=colnames(cES)=colnames(pcES)=c(paste("F",1:K,sep="."),"Residuals")
  
  for (i in object$asset.names) {
    # return data for asset i
    R.xts <- object$data[,i]
    
    if (type=="np") {
      # get VaR for asset i
      VaR.fm[i] <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
      # index of VaR exceedances
      idx.exceed[[i]] <- which(R.xts <= VaR.fm[i])
      # compute ES as expected value of asset return, such that the given asset 
      # return is less than or equal to its value-at-risk (VaR)
      ES.fm[i] <- mean(R.xts[idx.exceed[[i]]], na.rm =TRUE)
      # get F.star data object
      time(factors.xts) <- time(resid.xts[,i])
      factor.star <- merge(factors.xts, resid.xts[,i])
      colnames(factor.star)[dim(factor.star)[2]] <- "Residuals"
      # compute marginal ES as expected value of factor returns, when the asset's 
      # return is less than or equal to its value-at-risk (VaR)
      mES[i,] <- colMeans(factor.star[idx.exceed[[i]],], na.rm =TRUE)
      
    } else if (type=="normal") {
      # extract vector of factor model loadings for asset i
      beta.i <- beta.star[i,,drop=F]
      # compute ES
      ES.fm[i] <- -(beta.star[i,] %*% MU + sqrt(beta.i %*% factor.star.cov %*% t(beta.i))*dnorm(qnorm(p))/(p)) 
      # compute marginal ES
      mES[i,] <- -(t(MU) + SIGB[i,]/sd(R.xts, na.rm=TRUE) * dnorm(qnorm(p))/(p))
    }
    
    # correction factor to ensure that sum(cES) = asset ES
    cf <- as.numeric( ES.fm[i] / sum(mES[i,]*beta.star[i,], na.rm=TRUE) )
    
    # compute marginal, component and percentage contributions to ES
    # each of these have dimensions: N x (K+1)
    mES[i,] <- cf * mES[i,]
    cES[i,] <- mES[i,] * beta.star[i,]
    pcES[i,] <- 100* cES[i,] / ES.fm[i]
  }
  
  fm.ES.decomp <- list(ES.fm=ES.fm, mES=mES, cES=cES, pcES=pcES)
  
  return(fm.ES.decomp)
}

#' @rdname fmEsDecomp
#' @method fmEsDecomp ffm
#' @export

fmEsDecomp.ffm <- function(object, factor.cov, p=0.05, type=c("np","normal"),  
                           use="pairwise.complete.obs", ...) {
  # set default for type
  type = type[1]
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  # get beta.star
  beta <- object$beta
  beta[is.na(beta)] <- 0
  beta.star <- as.matrix(cbind(beta, sqrt(object$resid.var)))
  colnames(beta.star)[dim(beta.star)[2]] <- "Residuals"
  
  # factor returns and residuals data
  factors.xts <- object$factor.returns
  resid.xts <- as.xts(t(t(residuals(object))/sqrt(object$resid.var)))
  time(resid.xts) <- as.Date(time(resid.xts))
  
  if (type=="normal") {
    # get cov(F): K x K
    if (missing(factor.cov)) {
      factor.cov <- object$factor.cov
    } else {
      if (!identical(dim(factor.cov), dim(object$factor.cov))) {
        stop("Dimensions of user specified factor covariance matrix are not 
             compatible with the number of factors in the fitSfm object")
      }
    }
    # get cov(F.star): (K+1) x (K+1)
    K <- ncol(object$beta)
    factor.star.cov <- diag(K+1)
    factor.star.cov[1:K, 1:K] <- factor.cov
    colnames(factor.star.cov) <- c(colnames(factor.cov),"Residuals")
    rownames(factor.star.cov) <- c(colnames(factor.cov),"Residuals")
    # factor expected returns
    MU <- c(colMeans(factors.xts, na.rm=TRUE), 0)
    # SIGMA*Beta to compute normal mVaR
    SIGB <-  beta.star %*% factor.star.cov
  }
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, N)
  ES.fm <- rep(NA, N)
  idx.exceed <- list()
  names(VaR.fm) = names(ES.fm) = object$asset.names
  mES <- matrix(NA, N, K+1)
  cES <- matrix(NA, N, K+1)
  pcES <- matrix(NA, N, K+1)
  rownames(mES)=rownames(cES)=rownames(pcES)=object$asset.names
  colnames(mES)=colnames(cES)=colnames(pcES)=c(object$factor.names, "Residuals")
  
  for (i in object$asset.names) {
    # return data for asset i
    subrows <- which(object$data[[object$asset.var]]==i)
    R.xts <- as.xts(object$data[subrows,object$ret.var], 
                    as.Date(object$data[subrows,object$date.var]))
    
    if (type=="np") {
      # get VaR for asset i
      VaR.fm[i] <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
      # index of VaR exceedances
      idx.exceed[[i]] <- which(R.xts <= VaR.fm[i])
      # compute ES as expected value of asset return, such that the given asset 
      # return is less than or equal to its value-at-risk (VaR)
      ES.fm[i] <- mean(R.xts[idx.exceed[[i]]], na.rm =TRUE)
      # get F.star data object
      time(factors.xts) <- time(resid.xts[,i])
      factor.star <- merge(factors.xts, resid.xts[,i])
      colnames(factor.star)[dim(factor.star)[2]] <- "Residuals"
      # compute marginal ES as expected value of factor returns, when the asset's 
      # return is less than or equal to its value-at-risk (VaR)
      mES[i,] <- colMeans(factor.star[idx.exceed[[i]],], na.rm =TRUE)
      
    } else if (type=="normal")  {
      # extract vector of factor model loadings for asset i
      beta.i <- beta.star[i,,drop=F]
      # compute ES
      ES.fm[i] <- -(beta.star[i,] %*% MU + sqrt(beta.i %*% factor.star.cov %*% t(beta.i))*dnorm(qnorm(p))/(p)) 
      # compute marginal ES
      mES[i,] <- -(t(MU) + SIGB[i,]/sd(R.xts, na.rm=TRUE) * dnorm(qnorm(p))/(p))
    }
    
    # correction factor to ensure that sum(cES) = asset ES
    cf <- as.numeric( ES.fm[i] / sum(mES[i,]*beta.star[i,], na.rm=TRUE) )
    
    # compute marginal, component and percentage contributions to ES
    # each of these have dimensions: N x (K+1)
    mES[i,] <- cf * mES[i,]
    cES[i,] <- mES[i,] * beta.star[i,]
    pcES[i,] <- 100* cES[i,] / ES.fm[i]
  }
  
  fm.ES.decomp <- list(ES.fm=ES.fm, mES=mES, cES=cES, pcES=pcES)
  
  return(fm.ES.decomp)
}
