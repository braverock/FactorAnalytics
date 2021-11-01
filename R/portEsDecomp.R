#' @title Decompose portfolio ES into individual factor contributions
#' 
#' @description Compute the factor contributions to Expected Tail Loss or 
#' Expected Shortfall (ES) of portfolio returns  based on Euler's theorem, given 
#' the fitted factor model. The partial derivative of ES with respect to factor 
#' beta is computed as the expected factor return given portfolio return is less 
#' than or equal to its value-at-risk (VaR). Option to choose between 
#' non-parametric and Normal.
#' 
#' @importFrom stats quantile residuals cov resid qnorm
#' @importFrom xts as.xts  
#' @importFrom zoo zoo as.yearmon index
#' 
#' @details The factor model for a portfolio's return at time \code{t} has the 
#' form \cr \cr \code{R(t) = beta'f(t) + e(t) = beta.star'f.star(t)} \cr \cr 
#' where, \code{beta.star=(beta,sig.e)} and \code{f.star(t)=[f(t)',z(t)]'}. By 
#' Euler's theorem, the ES of the portfolio's return is given by:
#' \cr \cr \code{ES.fm = sum(cES_k) = sum(beta.star_k*mES_k)} \cr \cr
#' where, summation is across the \code{K} factors and the residual, 
#' \code{cES} and \code{mES} are the component and marginal 
#' contributions to \code{ES} respectively. The marginal contribution to ES is
#' defined as the expected value of \code{F.star}, conditional on the loss 
#' being less than or equal to \code{portVaR}. This is estimated as a sample 
#' average of the observations in that data window. 
#' 
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param weights a vector of weights of the assets in the portfolio, names of 
#' the vector should match with asset names. Default is NULL, in which case an 
#' equal weights will be used.
#' @param factor.cov optional user specified factor covariance matrix with 
#' named columns; defaults to the sample covariance matrix.
#' @param p tail probability for calculation. Default is 0.05.
#' @param type one of "np" (non-parametric) or "normal" for calculating Es. 
#' Default is "np".
#' @param invert a logical variable to choose if change ES to positive number, default
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
#' \item{portES}{factor model ES of portfolio returns.}
#' \item{mES}{length-(K + 1) vector of marginal contributions to Es.}
#' \item{cES}{length-(K + 1) vector of component contributions to Es.}
#' \item{pcES}{length-(K + 1) vector of percentage component contributions to Es.}
#' Where, K is the number of factors. 
#' 
#' @author Douglas Martin, Lingjie Yi
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{fitFfm}}
#' for the different factor model fitting functions.
#' 
#' \code{\link{portSdDecomp}} for factor model Sd decomposition.
#' \code{\link{portVaRDecomp}} for factor model VaR decomposition.
#' 
#' @examples
#' # Time Series Factor Model
#' 
#'  # load data
#' data(managers, package = 'PerformanceAnalytics')
#' 
#' fit.macro <- fitTsfm(asset.names = colnames(managers[,(1:6)]),
#'                      factor.names = colnames(managers[,(7:9)]),
#'                      rf.name = colnames(managers[,10]), 
#'                      data = managers)
#'                      
#' ES.decomp <- portEsDecomp(fit.macro, invert = TRUE)
#' 
#' # get the component contributions
#' ES.decomp$cES
#' 
#' # random weights 
#' wts = runif(6)
#' wts = wts/sum(wts)
#' names(wts) <- colnames(managers)[1:6]
#' 
#' portEsDecomp(fit.macro, wts)
#' 
#' # Fundamental Factor Model
#' data("stocks145scores6")
#' dat = stocks145scores6
#' dat$DATE = zoo::as.yearmon(dat$DATE)
#' dat = dat[dat$DATE >=zoo::as.yearmon("2008-01-01") & dat$DATE <= zoo::as.yearmon("2012-12-31"),]
#'
#' # Load long-only GMV weights for the return data
#' data("wtsStocks145GmvLo")
#' wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  
#'                                                      
#' # fit a fundamental factor model
#' fit.cross <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","SIZE", "EP"),
#'               date.var = "DATE", 
#'               ret.var = "RETURN", 
#'               asset.var = "TICKER", 
#'               fit.method="WLS", 
#'               z.score = "crossSection")
#'               
#' decomp = portEsDecomp(fit.cross) 
#' 
#' #get the factor contributions of risk 
#' decomp$cES
#' 
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
#' @importFrom zoo index 
#' @importFrom xts as.xts
#' @export

portEsDecomp.tsfm <- function(object, weights = NULL, p=0.05, type=c("np","normal"), 
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
  resid.xts <- xts::as.xts(t(t(residuals(object))/object$resid.sd) %*% weights)
  zoo::index(resid.xts) <- as.Date(zoo::index(resid.xts))
  
  if (type=="normal") {
    # get cov(F): K x K
    factor <- as.matrix(object$data[, object$factor.names])
    factor.cov = cov(factor, use=use, ...)
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
  ES.fm <- rep(NA, 1)
  idx.exceed <- list()

  mES <- rep(NA, 1+K)
  cES <- rep(NA, 1+K)
  pcES <- rep(NA, 1+K)
  names(mES)=names(cES)=names(pcES) <- colnames(beta.star)
  
  # return data for portfolio
  match = colnames(object$data) %in% asset.names
  R.xts <- object$data[,match]
  R.xts <- R.xts * weights
  R.xts = xts::as.xts(rowSums(R.xts), order.by = zoo::index(R.xts))
  names(R.xts) = 'RETURN'
  
  if (type=="np") { 
    # get VaR for asset i
    VaR.fm <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
    # index of VaR exceedances
    idx.exceed <- which(R.xts <= VaR.fm)
    # compute ES as expected value of asset return, such that the given asset 
    # return is less than or equal to its value-at-risk (VaR)
    ES.fm <- mean(R.xts[idx.exceed], na.rm =TRUE)
    
    # get F.star data object
    factor.star <- merge(factors.xts, resid.xts)
    colnames(factor.star)[dim(factor.star)[2]] <- "Residuals"
    # compute marginal ES as expected value of factor returns, when the asset's 
    # return is less than or equal to its value-at-risk (VaR)
    
    mES <- colMeans(factor.star[idx.exceed,], na.rm =TRUE)
    
  } else if (type=="normal") {

    # compute ES
    ES.fm <- -drop(beta.star %*% MU + sqrt(beta.star %*% factor.star.cov %*% t(beta.star))
                  *dnorm(qnorm(p))/(p))
    # compute marginal ES
    mES <- -drop(MU + SIGB/sd(R.xts, na.rm=TRUE) * dnorm(qnorm(p))/(p))
  }
  
  # correction factor to ensure that sum(cES) = asset ES
  cf <- as.numeric( ES.fm / sum(mES*beta.star), na.rm=TRUE) 
  
  # compute marginal, component and percentage contributions to ES
  # each of these have dimensions: N x (K+1)
  mES <- drop(cf * mES)
  cES <- drop(mES * beta.star)
  pcES <- drop(100* cES / ES.fm)
  
  if(invert){
    ES.fm <- -ES.fm
  } 
  
  fm.ES.decomp <- list(portES=ES.fm, mES=mES, cES=cES, pcES=pcES)
  
  return(fm.ES.decomp)
}



#' @rdname portEsDecomp
#' @method portEsDecomp ffm
#' @importFrom zoo index 
#' @export

portEsDecomp.ffm <- function(object, weights = NULL, factor.cov, p=0.05, type=c("np","normal"), 
                             invert = FALSE, ...){
  
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
  resid.xts <- xts::as.xts( t(t(residuals(object))/sqrt(object$resid.var)) %*% weights)
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
  ES.fm <- rep(NA, 1)
  idx.exceed <- list()
  
  mES <- rep(NA, 1+K)
  cES <- rep(NA, 1+K)
  pcES <- rep(NA, 1+K)
  names(mES)=names(cES)=names(pcES)=c(colnames(beta),"Residuals")
  
  dat = object$data
  # return data for portfolio
  R.xts = tapply(dat[,object$ret.var], list(dat[,object$date.var], dat[,object$asset.var]), FUN = I)
  R.xts <- R.xts * weights
  R.xts = xts::as.xts(rowSums(R.xts), order.by = object$time.periods)
  names(R.xts) = 'RETURN'
  
  
  if (type=="np") {
    # get VaR for asset i
    VaR.fm <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
    # index of VaR exceedances
    idx.exceed <- which(R.xts <= VaR.fm)
    # compute ES as expected value of asset return, such that the given asset 
    # return is less than or equal to its value-at-risk (VaR)
    ES.fm <- mean(R.xts[idx.exceed], na.rm =TRUE)
    
    # get F.star data object
    zoo::index(factors.xts) <- zoo::index(resid.xts)
    factor.star <- merge(factors.xts, resid.xts)
    colnames(factor.star)[dim(factor.star)[2]] <- "Residuals"
    
    # compute marginal ES as expected value of factor returns, when the asset's 
    # return is less than or equal to its value-at-risk (VaR)
    mES <- colMeans(factor.star[idx.exceed,], na.rm =TRUE)
    
  } else if (type=="normal")  {

    # compute ES
    ES.fm <- -drop(beta.star %*% MU + sqrt(beta.star %*% factor.star.cov %*% t(beta.star))
                  *dnorm(qnorm(p))/(p))
    # compute marginal ES
    mES <- -drop(MU + SIGB/sd(R.xts, na.rm=TRUE) * dnorm(qnorm(p))/(p))
  }
  
  # correction factor to ensure that sum(cES) = asset ES
  cf <- as.numeric( ES.fm / sum(mES*beta.star), na.rm=TRUE) 
  
  # compute marginal, component and percentage contributions to ES
  # each of these have dimensions: N x (K+1)
  mES <- drop(cf * mES)
  cES <- drop(mES * beta.star)
  pcES <- drop(100* cES / ES.fm)
  
  if(invert){
    ES.fm <- -ES.fm
  } 
  
  fm.ES.decomp <- list(portES=ES.fm, mES=mES, cES=cES, pcES=pcES)
  
  return(fm.ES.decomp)
}
  
