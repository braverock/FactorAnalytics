#' @title Decompose portfolio standard deviation into individual factor contributions
#' 
#' @description Compute the factor contributions to standard deviation (Sd) of 
#' portfolio returns based on Euler's theorem, given the fitted factor model.
#' 
#' @importFrom stats quantile residuals cov resid qnorm
#' @importFrom xts as.xts 
#' @importFrom zoo index as.Date  
#' 
#' @details The factor model for a portfolio's return at time \code{t} has the 
#' form \cr \cr \code{R(t) = beta'f(t) + e(t) = beta.star'f.star(t)} \cr \cr 
#' where, \code{beta.star=(beta,sig.e)} and \code{f.star(t)=[f(t)',z(t)]'}. 
#' \cr \cr By Euler's theorem, the standard deviation of the portfolio's return 
#' is given as: \cr \cr 
#' \code{portSd = sum(cSd_k) = sum(beta.star_k*mSd_k)} \cr \cr 
#' where, summation is across the \code{K} factors and the residual, 
#' \code{cSd} and \code{mSd} are the component and marginal 
#' contributions to \code{Sd} respectively. Computing \code{portSd} and 
#' \code{mSd} is very straight forward. The formulas are given below and 
#' details are in the references. The covariance term is approximated by the 
#' sample covariance. \cr \cr
#' \code{portSd = sqrt(beta.star''cov(F.star)beta.star)} \cr 
#' \code{mSd = cov(F.star)beta.star / portSd}
#' 
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL, 
#' in which case an equal weights will be used.
#' @param factor.cov optional user specified factor covariance matrix with 
#' named columns; defaults to the sample covariance matrix.
#' @param use an optional character string giving a method for computing 
#' covariances in the presence of missing values. This must be (an 
#' abbreviation of) one of the strings "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is 
#' "pairwise.complete.obs".
#' @param ... optional arguments passed to \code{\link[stats]{cov}}.
#' 
#' @return A list containing 
#' \item{portSd}{factor model Sd of portfolio return.}
#' \item{mSd}{length-(K + 1) vector of marginal contributions to Sd.}
#' \item{cSd}{length-(K + 1) vector of component contributions to Sd.}
#' \item{pcSd}{length-(K + 1) vector of percentage component contributions to Sd.}
#' Where, K is the number of factors.
#' 
#' @author Douglas Martin, Lingjie Yi
#' 
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{fitFfm}}
#' for the different factor model fitting functions.
#' 
#' \code{\link{portVaRDecomp}} for portfolio factor model VaR decomposition.
#' \code{\link{portEsDecomp}} for portfolio factor model ES decomposition.
#' 
#' 
#' @examples
#' # Time Series Factor Model
#' data(managers)
#' fit.macro <- factorAnalytics::fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:9)]),
#'                      rf.name=colnames(managers[,10]), data=managers)
#' decomp <- portSdDecomp(fit.macro)
#' # get the factor contributions of risk
#' decomp$cSd
#' 
#' # random weights 
#' wts = runif(6)
#' wts = wts/sum(wts)
#' names(wts) <- colnames(managers)[1:6]
#' portSdDecomp(fit.macro, wts)
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
#'               fit.method="WLS", z.score = TRUE)
#'               
#' decomp = portSdDecomp(fit.cross) 
#' # get the factor contributions of risk 
#' decomp$cSd
#' portSdDecomp(fit.cross, wtsStocks145GmvLo)               
#'  
#' @export    
                                   

portSdDecomp <- function(object, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm',  or 'ffm'.")
  }
  UseMethod("portSdDecomp")
}

#' @rdname portSdDecomp
#' @method portSdDecomp tsfm
#' @export

portSdDecomp.tsfm <- function(object, weights = NULL, factor.cov, 
                              use="pairwise.complete.obs", ...) {
  
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
  colnames(beta.star)[dim(beta.star)[2]] <- "residual"
  
  # get cov(F): K x K
  # get cov(F): K x K
  factor <- as.matrix(object$data[, object$factor.names])
  if (missing(factor.cov)) {
    factor.cov = cov(factor, use=use, ...) 
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
  
  # compute factor model sd; a vector of length 1
  Sd.fm <- sqrt(rowSums(beta.star %*% factor.star.cov * beta.star))
  
  # compute marginal, component and percentage contributions to sd
  # each of these have dimensions: N + 1
  mSd <- drop((t(factor.star.cov %*% t(beta.star)))/Sd.fm) 
  cSd <- drop(mSd * beta.star) 
  pcSd <- drop(100* cSd/Sd.fm) 
  
  fm.sd.decomp <- list(portSd=Sd.fm, mSd=mSd, cSd=cSd, pcSd=pcSd)
  
  return(fm.sd.decomp)
}

#' @rdname portSdDecomp
#' @method portSdDecomp ffm
#' @export

portSdDecomp.ffm <- function(object, weights = NULL, factor.cov, ...) {
  
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
  colnames(beta.star)[dim(beta.star)[2]] <- "residual"
  
  
  # get cov(F): K x K
  if (missing(factor.cov)) {
    factor.cov = object$factor.cov
  } else {
    if (!identical(dim(factor.cov), dim(object$factor.cov))) {
      stop("Dimensions of user specified factor covariance matrix are not 
           compatible with the number of factors (including dummies) in the 
           fitFfm object")
    }
  }
  
  # get cov(F.star): (K+1) x (K+1)
  K <- ncol(beta)
  factor.star.cov <- diag(K+1)
  factor.star.cov[1:K, 1:K] <- factor.cov
  colnames(factor.star.cov) <- c(colnames(factor.cov),"residuals")
  rownames(factor.star.cov) <- c(colnames(factor.cov),"residuals")
  

  # compute factor model sd; a vector of length 1
  Sd.fm <- drop(sqrt(rowSums(beta.star %*% factor.star.cov * beta.star)))
  
  # compute marginal, component and percentage contributions to sd
  # each of these have dimensions: N+K
  mSd <- drop((t(factor.star.cov %*% t(beta.star)))/Sd.fm)
  cSd <- drop(mSd * beta.star)
  pcSd <- drop(100* cSd/Sd.fm) 
  
  fm.sd.decomp <- list(portSd=Sd.fm, mSd=mSd, cSd=cSd, pcSd=pcSd)
  
  return(fm.sd.decomp)
}
