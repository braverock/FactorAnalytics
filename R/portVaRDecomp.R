#' @title Decompose portfolio VaR into individual factor contributions
#' 
#' @description Compute the factor contributions to Value-at-Risk (VaR) of 
#' portfolio return based on Euler's theorem, given the fitted factor model. 
#' The partial derivative of VaR w.r.t. factor beta is computed as the expected 
#' factor return given fund return is equal to its VaR and approximated by a
#' kernel estimator. Option to choose between non-parametric and Normal.
#' 
#' @importFrom stats quantile residuals cov resid time qnorm
#' @importFrom xts as.xts 
#' @importFrom zoo as.Date  
#' 
#' @details The factor model for an portfolio's return at time \code{t} has the 
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
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL, 
#' in which case an equal weights will be used.
#' @param p confidence level for calculation. Default is 0.95.
#' @param type one of "np" (non-parametric) or "normal" for calculating VaR. 
#' Default is "np".
#' @param use an optional character string giving a method for computing factor
#' covariances in the presence of missing values. This must be (an 
#' abbreviation of) one of the strings "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is 
#' "pairwise.complete.obs".
#' @param ... other optional arguments passed to \code{\link[stats]{quantile}} and 
#' optional arguments passed to \code{\link[stats]{cov}}
#' 
#' @return A list containing 
#' \item{VaR.fm}{length-1 vector of factor model VaRs of portfolio returns.}
#' \item{n.exceed}{length-1 vector of number of observations beyond VaR.}
#' \item{idx.exceed}{list of numeric vector of index values of exceedances.}
#' \item{mVaR}{length-(N+K) vector of marginal contributions to VaR.}
#' \item{cVaR}{length-(N+K) vector of component contributions to VaR.}
#' \item{pcVaR}{length-(N+K) vector of percentage component contributions to VaR.}
#' Where, K is the number of factors and N is the number of assets.
#' 
#' @author Douglas Martin, Lingjie Yi
#' 
#' 
#' @examples
#' # Time Series Factor Model
#' require(factorAnalytics)
#' data(managers)
#' fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:9)]),
#'                      rf.name="US.3m.TR", data=managers)
#' decomp <- portVaRDecomp(fit.macro)
#' # get the factor contributions of risk
#' decomp$cVaR
#' # random weights
#' wts = runif(6)
#' wts = wts/sum(wts)
#' portVaRDecomp(fit.macro, wts) 
#' 
#' # Fundamental Factor Model
#' data("stocks145scores6")
#' dat = stocks145scores6
#' dat$DATE = as.yearmon(dat$DATE)
#' dat = dat[dat$DATE >=as.yearmon("2008-01-01") & dat$DATE <= as.yearmon("2012-12-31"),]
#'
#' #Load long-only GMV weights for the return data
#' data("wtsStocks145GmvLo")
#' wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  
#'                                                      
#' #fit a fundamental factor model
#' fit.cross <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"),
#'               date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
#'               fit.method="WLS", z.score = TRUE)
#' decomp = portVaRDecomp(fit.cross) 
#' # get the factor contributions of risk 
#' decomp$cVaR
#' portVaRDecomp(fit.cross, weights = wtsStocks145GmvLo)               
#'  
#' @export   

portVaRDecomp <- function(object,  ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', or 'ffm'.")
  }
  UseMethod("portVaRDecomp")
}

#' @rdname portVaRDecomp
#' @method portVaRDecomp tsfm
#' @export

portVaRDecomp.tsfm <- function(object, weights = NULL, p=0.95, type=c("np","normal"),  
                             use="pairwise.complete.obs", ...) {
  
  # set default for type
  type = type[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  # get beta.star
  beta <- object$beta
  beta[is.na(beta)] <- 0
  n.assets = nrow(beta)
  asset.names <- object$asset.names
  
  # check if there is weight input
  if(is.null(weights)){
    weights = rep(1/n.assets, n.assets)
  }else{
    # check if number of weight parameter matches 
    if(n.assets != length(weights)){
      stop("Invalid argument: incorrect number of weights")
    }
    if(!is.null(names(wts))){
      weights = weights[asset.names]
    }
  } 
  
  # get portfolio beta.star: 1 x (K+N)
  beta.star <- as.matrix(cbind(weights %*% as.matrix(beta), t(weights * object$resid.sd)))  


  # factor returns and residuals data
  factors.xts <- object$data[,object$factor.names]
  resid.xts <- as.xts(t(t(residuals(object))/object$resid.sd))
  time(resid.xts) <- as.Date(time(resid.xts))

  
  if (type=="normal") {
    # get cov(F): K x K
    factor <- as.matrix(object$data[, object$factor.names])
    factor.cov = cov(factor, use=use, ...)
    
    # get cov(F.star): (K+N) x (K+N)
    K <- ncol(object$beta)
    factor.star.cov <- diag(K+n.assets)
    factor.star.cov[1:K, 1:K] <- factor.cov
    colnames(factor.star.cov) <- c(colnames(factor.cov),asset.names)
    rownames(factor.star.cov) <- c(colnames(factor.cov),asset.names)
    
    # factor expected returns
    MU <- c(colMeans(factors.xts, na.rm=TRUE), rep(0,n.assets))
    names(MU) <- c(colnames(factor.cov),asset.names)
    
    # SIGMA*Beta to compute normal mVaR
    SIGB <-  beta.star %*% factor.star.cov
  }
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, 1)
  idx.exceed <- list()
  n.exceed <- rep(NA, 1)
  
  mVaR <- rep(NA, N+K)
  cVaR <- rep(NA, N+K)
  pcVaR <- rep(NA, N+K)
  names(mVaR)=names(cVaR)=names(pcVaR)=c(colnames(beta),asset.names)

  # return data for portfolio
  match = colnames(object$data) %in% asset.names
  R.xts <- object$data[,match]
  R.xts <- R.xts * weights
  R.xts = as.xts(rowSums(R.xts), order.by = index(R.xts))
  names(R.xts) = 'RETURN'
  
  # get VaR for porfolio
  if (type=="np") {
    VaR.fm <- quantile(R.xts, probs=1-p, na.rm=TRUE, ...)
  } 
  else if (type=="normal") {
    VaR.fm <- mean(R.xts, na.rm=TRUE) + sd(R.xts, na.rm=TRUE)*qnorm(1-p)
  }
  # index of VaR exceedances
  idx.exceed <- which(R.xts <= VaR.fm)
  # number of VaR exceedances
  n.exceed <- length(idx.exceed)
  
  #     # plot exceedances for asset i
  #     plot(R.xts, type="b", main="Asset Returns and 5% VaR Violations",
  #          ylab="Returns")
  #     abline(h=0)
  #     abline(h=VaR.fm, lwd=2, col="red")
  #     points(R.xts[idx.exceed], type="p", pch=16, col="red")
  
  # get F.star data object
  factor.star <- merge(factors.xts, resid.xts)
  
  if (type=="np") {
    # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
    # the constant 2.575 corresponds to a triangular kernel 
    eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
    # compute marginal VaR as expected value of factor returns, such that the
    # asset return was incident in the triangular kernel region peaked at the 
    # VaR value and bandwidth = epsilon.
    k.weight <- as.vector(1 - abs(R.xts - VaR.fm) / eps)
    k.weight[k.weight<0] <- 0
    mVaR <- colMeans(factor.star*k.weight, na.rm =TRUE)
  } 
  else if (type=="normal")  {
    mVaR <- MU + drop(SIGB) * qnorm(1-p)/sd(R.xts, na.rm=TRUE)
  }
  
  # correction factor to ensure that sum(cVaR) = asset VaR
  cf <- as.numeric( VaR.fm / sum(mVaR*beta.star, na.rm=TRUE) )
  
  # compute marginal, component and percentage contributions to VaR
  # each of these have dimensions: N x (K+1)
  mVaR <- cf * mVaR
  cVaR <- mVaR * beta.star
  pcVaR <- 100* cVaR / VaR.fm
  
  fm.VaR.decomp <- list(VaR.fm=VaR.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                        mVaR=mVaR, cVaR=cVaR, pcVaR=pcVaR)
  
  return(fm.VaR.decomp)
}


#' @rdname portVaRDecomp
#' @method portVaRDecomp ffm
#' @export

portVaRDecomp.ffm <- function(object, weights = NULL, p=0.95, type=c("np","normal"), ...) {
  
  # set default for type
  type = type[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  # get beta.star
  beta <- object$beta
  beta[is.na(beta)] <- 0
  n.assets = nrow(beta)
  asset.names <- object$asset.names
  
  # check if there is weight input
  if(is.null(weights)){
    weights = rep(1/n.assets, n.assets)
  }else{
    # check if number of weight parameter matches 
    if(n.assets != length(weights)){
      stop("Invalid argument: incorrect number of weights")
    }
    weights = weights[asset.names]
  }   
  
  # get portfolio beta.star: 1 x (K+N)
  beta.star <- as.matrix(cbind(weights %*% as.matrix(beta), t(weights * sqrt(object$resid.var))))   

  
  # factor returns and residuals data
  factors.xts <- object$factor.returns
  resid.xts <- as.xts(t(t(residuals(object))/sqrt(object$resid.var)))
  time(resid.xts) <- as.Date(time(resid.xts))
  
  if (type=="normal") {
    # get cov(F): K x K
    factor.cov = object$factor.cov
    
    # get cov(F.star): (K+N) x (K+N)
    K <- ncol(object$beta)
    factor.star.cov <- diag(K+n.assets)
    factor.star.cov[1:K, 1:K] <- factor.cov
    colnames(factor.star.cov) <- c(colnames(factor.cov),asset.names)
    rownames(factor.star.cov) <- c(colnames(factor.cov),asset.names)
    
    # factor expected returns
    MU <- c(colMeans(factors.xts, na.rm=TRUE), rep(0,n.assets))
    names(MU) <- c(colnames(factor.cov),asset.names)
    
    # SIGMA*Beta to compute normal mVaR
    SIGB <-  beta.star %*% factor.star.cov
  }
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, 1)
  idx.exceed <- list()
  n.exceed <- rep(NA, 1)
  
  mVaR <- rep(NA, N+K)
  cVaR <- rep(NA, N+K)
  pcVaR <- rep(NA, N+K)
  names(mVaR)=names(cVaR)=names(pcVaR)=c(colnames(beta),asset.names)
  
  dat = object$data
  # return data for portfolio
  R.xts = tapply(dat[,object$ret.var], list(dat[,object$date.var], dat[,object$asset.var]), FUN = I)
  R.xts <- R.xts * weights
  R.xts = as.xts(rowSums(R.xts), order.by = object$time.periods)
  names(R.xts) = 'RETURN'
  
  # get VaR for portfolio
  if (type=="np") {
    VaR.fm <- quantile(R.xts, probs=1-p, na.rm=TRUE, ...)
  } 
  else if (type=="normal") {
    VaR.fm <- mean(R.xts, na.rm=TRUE) + sd(R.xts, na.rm=TRUE)*qnorm(1-p)
  }
  # index of VaR exceedances
  idx.exceed <- which(R.xts <= VaR.fm)
  # number of VaR exceedances
  n.exceed <- length(idx.exceed)
  
  # get F.star data object
  time(factors.xts) <- time(resid.xts)
  factor.star <- merge(factors.xts, resid.xts)
  
  
  if (type=="np") {
    # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
    # the constant 2.575 corresponds to a triangular kernel 
    eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
    # compute marginal VaR as expected value of factor returns, such that the
    # asset return was incident in the triangular kernel region peaked at the 
    # VaR value and bandwidth = epsilon.
    k.weight <- as.vector(1 - abs(R.xts - VaR.fm) / eps)
    k.weight[k.weight<0] <- 0
    mVaR <- colMeans(factor.star*k.weight, na.rm =TRUE)
  } 
  else if (type=="normal")  {
    mVaR <- MU + drop(SIGB) * qnorm(1-p)/sd(R.xts, na.rm=TRUE)
  }
  
  # correction factor to ensure that sum(cVaR) = asset VaR
  cf <- as.numeric( VaR.fm / sum(mVaR*beta.star, na.rm=TRUE) )
  
  # compute marginal, component and percentage contributions to VaR
  # each of these have dimensions: N x (K+1)
  mVaR <- drop(cf * mVaR)
  cVaR <- drop(mVaR * beta.star)
  pcVaR <- drop(100* cVaR / VaR.fm)
  
  fm.VaR.decomp <- list(VaR.fm=VaR.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                        mVaR=mVaR, cVaR=cVaR, pcVaR=pcVaR)
  
  return(fm.VaR.decomp)
}
