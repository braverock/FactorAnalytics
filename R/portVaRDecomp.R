#' @title Decompose portfolio VaR into individual factor contributions
#' 
#' @description Compute the factor contributions to Value-at-Risk (VaR) of 
#' portfolio returns based on Euler's theorem, given the fitted factor model. 
#' The partial derivative of VaR w.r.t. factor beta is computed as the expected 
#' factor return given portfolio return is equal to its VaR and approximated by a
#' kernel estimator. Option to choose between non-parametric and Normal.
#' 
#' @importFrom stats quantile residuals cov resid qnorm
#' @importFrom xts as.xts
#' @importFrom zoo as.Date index
#' 
#' @details The factor model for a portfolio's return at time \code{t} has the 
#' form \cr \cr \code{R(t) = beta'f(t) + e(t) = beta.star'f.star(t)} \cr \cr 
#' where, \code{beta.star=(beta,sig.e)} and \code{f.star(t)=[f(t)',z(t)]'}. By 
#' Euler's theorem, the VaR of the asset's return is given by: 
#' \cr \cr \code{VaR.fm = sum(cVaR_k) = sum(beta.star_k*mVaR_k)} \cr \cr 
#' where, summation is across the \code{K} factors and the residual, 
#' \code{cVaR} and \code{mVaR} are the component and marginal 
#' contributions to \code{VaR} respectively. The marginal contribution to VaR 
#' is defined as the expectation of \code{F.star}, conditional on the loss 
#' being equal to \code{portVaR}. This is approximated as described in 
#' Epperlein & Smillie (2006); a triangular smoothing kernel is used here. 
#' 
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL, 
#' in which case an equal weights will be used.
#' @param factor.cov optional user specified factor covariance matrix with 
#' named columns; defaults to the sample covariance matrix.
#' @param p tail probability for calculation. Default is 0.05.
#' @param type one of "np" (non-parametric) or "normal" for calculating VaR. 
#' Default is "np".
#' @param invert a logical variable to choose if change VaR to positive number, default
#' is False 
#' @param use an optional character string giving a method for computing factor
#' covariances in the presence of missing values. This must be (an 
#' abbreviation of) one of the strings "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is 
#' "pairwise.complete.obs".
#' @param ... other optional arguments passed to \code{\link[stats]{quantile}} and 
#' optional arguments passed to \code{\link[stats]{cov}}
#' 
#' @return A list containing 
#' \item{portVaR}{factor model VaR of portfolio return.}
#' \item{n.exceed}{number of observations beyond VaR.}
#' \item{idx.exceed}{a numeric vector of index values of exceedances.}
#' \item{mVaR}{length-(K + 1) vector of marginal contributions to VaR.}
#' \item{cVaR}{length-(K + 1) vector of component contributions to VaR.}
#' \item{pcVaR}{length-(K + 1) vector of percentage component contributions to VaR.}
#' Where, K is the number of factors.
#' 
#' @author Douglas Martin, Lingjie Yi
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{fitFfm}}
#' for the different factor model fitting functions.
#' 
#' \code{\link{portSdDecomp}} for factor model Sd decomposition.
#' \code{\link{portEsDecomp}} for factor model ES decomposition.
#' 
#' @examples
#' # Time Series Factor Model
#' 
#'  # load data
#' data(managers, package = 'PerformanceAnalytics')
#' colnames(managers)
#'  # Make syntactically valid column names
#' colnames(managers) <- make.names( colnames(managers))
#' colnames(managers)
#' 
#' fit.macro <- FactorAnalytics::fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:9)]),
#'                      rf.name=colnames(managers[,10]), 
#'                      data=managers)
#'                      
#' decomp <- portVaRDecomp(fit.macro,invert = TRUE)
#' 
#' # get the factor contributions of risk
#' decomp$cVaR
#' 
#' # random weights 
#' wts = runif(6)
#' wts = wts/sum(wts)
#' names(wts) <- colnames(managers)[1:6]
#' portVaRDecomp(fit.macro, wts)
#' 
#' 
#' # Fundamental Factor Model
#' data("stocks145scores6")
#' dat = stocks145scores6
#' dat$DATE = as.yearmon(dat$DATE)
#' dat = dat[dat$DATE >=as.yearmon("2008-01-01") & 
#'           dat$DATE <= as.yearmon("2012-12-31"),]
#'
#' # Load long-only GMV weights for the return data
#' data("wtsStocks145GmvLo")
#' wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  
#'                                                       
#' # fit a fundamental factor model
#' fit.cross <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE","VOL121",
#'               "EP"),date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
#'               fit.method="WLS", z.score = "crossSection")
#'               
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
#' @importFrom zoo index 
#' @export


portVaRDecomp.tsfm <- function(object, weights = NULL, factor.cov, p=0.05, type=c("np","normal"),  
                               invert = FALSE, use="pairwise.complete.obs", ...) {
  
  # set default for type
  type = type[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  # get beta.star: 1 x (K+1)
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
    if(!is.null(names(weights))){
      weights = weights[asset.names]
    }else{
      stop("Invalid argument: names of weights vector should match with asset names")
    }
  } 
  
  # get portfolio beta.star: 1 x (K+1)
  beta.star <- as.matrix(cbind(weights %*% as.matrix(beta), sqrt(sum(weights^2 * object$resid.sd^2))))  
  colnames(beta.star)[dim(beta.star)[2]] <- "Residuals"


  # factor returns and residuals data
  factors.xts <- object$data[,object$factor.names]
  resid.xts <- as.xts(t(t(residuals(object))/object$resid.sd) %*% weights)
  zoo::index(resid.xts) <- as.Date(zoo::index(resid.xts))
  
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
    names(MU) <- c(colnames(factor.cov),"Residuals")
    
    # SIGMA*Beta to compute normal mVaR
    SIGB <-  beta.star %*% factor.star.cov
  }
  
  # initialize lists and matrices
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, 1)
  idx.exceed <- list()
  n.exceed <- rep(NA, 1)
  
  mVaR <- rep(NA, 1+K)
  cVaR <- rep(NA, 1+K)
  pcVaR <- rep(NA, 1+K)
  names(mVaR)=names(cVaR)=names(pcVaR)=colnames(beta.star)

  # return data for portfolio
  match = colnames(object$data) %in% asset.names
  R.xts <- object$data[,match]
  R.xts <- R.xts * weights
  R.xts = as.xts(rowSums(R.xts), order.by = zoo::index(R.xts))
  names(R.xts) = 'RETURN'
  
  if (type=="np") {
    # get VaR for asset i
    VaR.fm <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
    
    # get F.star data object
    factor.star <- merge(factors.xts, resid.xts)
    colnames(factor.star)[dim(factor.star)[2]] <- "Residuals"
    
    # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
    # the constant 2.575 corresponds to a triangular kernel 
    eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
    
    # compute marginal VaR as expected value of factor returns, such that the
    # asset return was incident in the triangular kernel region peaked at the 
    # VaR value and bandwidth = epsilon.
    k.weight <- as.vector(1 - abs(R.xts - VaR.fm) / eps)
    k.weight[k.weight<0] <- 0
    mVaR <- colMeans(factor.star*k.weight, na.rm =TRUE)
    
  } else if (type=="normal")  {
    # get VaR for asset i
    VaR.fm <- drop(beta.star %*% MU + sqrt(beta.star %*% factor.star.cov %*% t(beta.star))*qnorm(p))
    # compute marginal VaR
    mVaR <- drop(MU + SIGB * qnorm(p)/sd(R.xts, na.rm=TRUE))
  }
  
  # index of VaR exceedances
  idx.exceed <- which(R.xts <= VaR.fm)
  # number of VaR exceedances
  n.exceed <- length(idx.exceed)
  
  # correction factor to ensure that sum(cVaR) = asset VaR
  cf <- as.numeric( VaR.fm / sum(mVaR*beta.star, na.rm=TRUE) )
  
  # compute marginal, component and percentage contributions to VaR
  # each of these have dimensions: N x (K+1)
  mVaR <- drop(cf * mVaR)
  cVaR <- drop(mVaR * beta.star)
  pcVaR <- drop(100* cVaR / VaR.fm)

  if(invert){
    VaR.fm <- -VaR.fm
  } 
  
  fm.VaR.decomp <- list(portVaR=VaR.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                        mVaR=mVaR, cVaR=cVaR, pcVaR=pcVaR)
  
  return(fm.VaR.decomp)
}


#' @rdname portVaRDecomp
#' @method portVaRDecomp ffm
#' @importFrom zoo index 
#' @export

portVaRDecomp.ffm <- function(object, weights = NULL, factor.cov, p=0.05, type=c("np","normal"),
                              invert = FALSE , ...) {
  
  # set default for type
  type = type[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  beta <- object$beta
  beta[is.na(beta)] <- 0
  n.assets = nrow(beta)
  asset.names <- unique(object$data[[object$asset.var]])
  
  # check if there is weight input
  if(is.null(weights)){
    weights = rep(1/n.assets, n.assets)
  }else{
    # check if number of weight parameter matches 
    if(n.assets != length(weights)){
      stop("Invalid argument: incorrect number of weights")
    }
    if(!is.null(names(weights))){
      weights = weights[asset.names]
    }else{
      stop("Invalid argument: names of weights vector should match with asset names")
    }
  }   

  # get portfolio beta.star: 1 x (K+1)  
  beta.star <- as.matrix(cbind(weights %*% beta, sqrt(sum(weights^2 * object$resid.var))))
  colnames(beta.star)[dim(beta.star)[2]] <- "Residuals"

  # factor returns and residuals data
  factors.xts <- object$factor.returns
  resid.xts <- as.xts( t(t(residuals(object))/sqrt(object$resid.var)) %*% weights)
  zoo::index(resid.xts) <- as.Date(zoo::index(resid.xts))

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
    names(MU) <- c(colnames(factor.cov),"Residuals")
    
    # SIGMA*Beta to compute normal mVaR
    SIGB <-  beta.star %*% factor.star.cov
  }

  # initialize lists and matrices
  K <- length(object$factor.names)
  VaR.fm <- rep(NA, 1)
  idx.exceed <- list()
  n.exceed <- rep(NA, 1)
  
  mVaR <- rep(NA, 1+K)
  cVaR <- rep(NA, 1+K)
  pcVaR <- rep(NA, 1+K)
  names(mVaR)=names(cVaR)=names(pcVaR)=colnames(beta.star)
  
  dat = object$data
  # return data for portfolio
  R.xts = tapply(dat[,object$ret.var], list(dat[,object$date.var], dat[,object$asset.var]), FUN = I)
  R.xts <- R.xts * weights
  R.xts = as.xts(rowSums(R.xts), order.by = object$time.periods)
  names(R.xts) = 'RETURN'
  
  if (type=="np") {
    VaR.fm <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
    
    # get F.star data object
    zoo::index(factors.xts) <- zoo::index(resid.xts)
    factor.star <- merge(factors.xts, resid.xts)
    colnames(factor.star)[dim(factor.star)[2]] <- "Residuals"
    
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
    VaR.fm <- drop(beta.star %*% MU + sqrt(beta.star %*% factor.star.cov %*% t(beta.star))*qnorm(p))
    mVaR <- drop(MU + drop(SIGB) * qnorm(p)/sd(R.xts, na.rm=TRUE))
  }
  
  # index of VaR exceedances
  idx.exceed <- which(R.xts <= VaR.fm)
  # number of VaR exceedances
  n.exceed <- length(idx.exceed)
  
  # correction factor to ensure that sum(cVaR) = asset VaR
  cf <- as.numeric( VaR.fm / sum(mVaR*beta.star, na.rm=TRUE) )
  
  # compute marginal, component and percentage contributions to VaR
  # each of these have dimensions: N x (K+1)
  mVaR <- drop(cf * mVaR)
  cVaR <- drop(mVaR * beta.star)
  pcVaR <- drop(100* cVaR / VaR.fm)
  
  if(invert){
    VaR.fm <- -VaR.fm
  } 
  
  fm.VaR.decomp <- list(portVaR=VaR.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                        mVaR=mVaR, cVaR=cVaR, pcVaR=pcVaR)
  
  return(fm.VaR.decomp)
}
