#' @title Decompose portfolio ES into individual factor contributions
#' 
#' @description Compute the factor contributions to Expected Tail Loss or 
#' Expected Shortfall (ES) of portfolio returns  based on Euler's theorem, given 
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
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param p confidence level for calculation. Default is 0.95.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL, 
#' in which case an equal weights will be used.
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
#' \item{ES.fm}{length-1 vector of factor model ES of portfolio returns.}
#' \item{n.exceed}{length-1 vector of number of observations beyond VaR for 
#' portfolio.}
#' \item{idx.exceed}{numeric vector of index values of exceedances.}
#' \item{mES}{length-(N + K) vector of marginal contributions to VaR.}
#' \item{cES}{length-(N + K) vector of component contributions to VaR.}
#' \item{pcES}{length-(N + K) vector of percentage component contributions to VaR.}
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
#'                      factor.names=colnames(managers[,(7:8)]), data=managers)
#' ES.decomp <- portEsDecomp(fit.macro)
#' # get the component contributions
#' ES.decomp$cES
#' 
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
#' decomp = portEsDecomp(fit.cross) 
#' # get the factor contributions of risk 
#' decomp$cES
#' portEsDecomp(fit.cross, weights = wtsStocks145GmvLo)  
#' @export

portEsDecomp <- function(object, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', or 'ffm'.")
  }
  UseMethod("portEsDecomp")
}

#' @rdname portEsDecomp
#' @method portEsDecomp tsfm
#' @export

portEsDecomp.tsfm <- function(object, weights = NULL, p=0.95, type=c("np","normal"), 
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
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, 1)
  ES.fm <- rep(NA, 1)
  idx.exceed <- list()
  n.exceed <- rep(NA, 1)

  mES <- rep(NA, N+K)
  cES <- rep(NA, N+K)
  pcES <- rep(NA, N+K)
  names(mES)=names(cES)=names(pcES)=c(colnames(beta),asset.names)
  
  # return data for portfolio
  match = colnames(object$data) %in% asset.names
  R.xts <- object$data[,match]
  R.xts <- R.xts * weights
  R.xts = as.xts(rowSums(R.xts), order.by = index(R.xts))
  names(R.xts) = 'RETURN'
  
  # get VaR for portfolio
  if (type=="np") {
    VaR.fm <- quantile(R.xts, probs=1-p, na.rm=TRUE, ...)
  } else {
    VaR.fm <- mean(R.xts, na.rm=TRUE) + sd(R.xts, na.rm=TRUE)*qnorm(1-p)
  }
  
  # index of VaR exceedances
  idx.exceed <- which(R.xts <= VaR.fm)
  # number of VaR exceedances
  n.exceed <- length(idx.exceed)
  
  # compute ES as expected value of asset return, such that the given asset 
  # return is less than or equal to its value-at-risk (VaR)
  ES.fm <- mean(R.xts[idx.exceed], na.rm =TRUE)
  
  # get cov(F.star): (K+N) x (K+N)
  K <- ncol(object$beta)
  factor <- as.matrix(object$data[, object$factor.names])
  factor.cov = cov(factor, use=use, ...)
  factor.star.cov <- diag(K+n.assets)
  factor.star.cov[1:K, 1:K] <- factor.cov
  colnames(factor.star.cov) <- c(colnames(factor.cov),asset.names)
  rownames(factor.star.cov) <- c(colnames(factor.cov),asset.names)
  
  factor.star <- merge(factors.xts, resid.xts)
  
  # compute marginal ES as expected value of factor returns, when the asset's 
  # return is less than or equal to its value-at-risk (VaR)
  mES <- colMeans(factor.star[idx.exceed,], na.rm =TRUE)
  
  # correction factor to ensure that sum(cES) = asset ES
  cf <- as.numeric( ES.fm / sum(mES*beta.star), na.rm=TRUE) 
  
  # compute marginal, component and percentage contributions to ES
  # each of these have dimensions: N x (K+1)
  mES <- drop(cf * mES)
  cES <- drop(mES * beta.star)
  pcES <- drop(100* cES / ES.fm)
  
  fm.ES.decomp <- list(ES.fm=ES.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                       mES=mES, cES=cES, pcES=pcES)
  
  return(fm.ES.decomp)
}



#' @rdname portEsDecomp
#' @method portEsDecomp ffm
#' @export

portEsDecomp.ffm <- function(object, weights = NULL, p=0.95, type=c("np","normal"), ...){
  
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
  
  
  # initialize lists and matrices
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, 1)
  ES.fm <- rep(NA, 1)
  idx.exceed <- list()
  n.exceed <- rep(NA, 1)
  
  mES <- rep(NA, N+K)
  cES <- rep(NA, N+K)
  pcES <- rep(NA, N+K)
  names(mES)=names(cES)=names(pcES)=c(colnames(beta),asset.names)
  
  dat = object$data
  # return data for portfolio
  R.xts = tapply(dat[,object$ret.var], list(dat[,object$date.var], dat[,object$asset.var]), FUN = I)
  R.xts <- R.xts * weights
  R.xts = as.xts(rowSums(R.xts), order.by = object$time.periods)
  names(R.xts) = 'RETURN'
  
  
  # get VaR for portfolio
  if (type=="np") {
    VaR.fm <- quantile(R.xts, probs=1-p, na.rm=TRUE, ...)
  } else {
    VaR.fm <- mean(R.xts, na.rm=TRUE) + sd(R.xts, na.rm=TRUE)*qnorm(1-p)
  }
  
  # index of VaR exceedances
  idx.exceed <- which(R.xts <= VaR.fm)
  # number of VaR exceedances
  n.exceed <- length(idx.exceed)
  
  # compute ES as expected value of asset return, such that the given asset 
  # return is less than or equal to its value-at-risk (VaR)
  ES.fm <- mean(R.xts[idx.exceed], na.rm =TRUE)
  
  # get F.star data object
  time(factors.xts) <- time(resid.xts)
  factor.star <- merge(factors.xts, resid.xts)
  
  # compute marginal ES as expected value of factor returns, when the asset's 
  # return is less than or equal to its value-at-risk (VaR)
  mES <- colMeans(factor.star[idx.exceed,], na.rm =TRUE)
  
  # correction factor to ensure that sum(cES) = asset ES
  cf <- as.numeric( ES.fm / sum(mES*beta.star), na.rm=TRUE) 
  
  # compute marginal, component and percentage contributions to ES
  # each of these have dimensions: N x (K+1)
  mES <- drop(cf * mES)
  cES <- drop(mES * beta.star)
  pcES <- drop(100* cES / ES.fm)
  
  fm.ES.decomp <- list(ES.fm=ES.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                       mES=mES, cES=cES, pcES=pcES)
  
  return(fm.ES.decomp)
  
  
}
  
