#' @title Decompose portfolio standard deviation into individual factor contributions
#' 
#' @description Compute the factor contributions to standard deviation (SD) of 
#' portfolio return based on Euler's theorem, given the fitted factor model.
#' 
#' @importFrom stats cov
#' 
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL.
#' @param use an optional character string giving a method for computing 
#' covariances in the presence of missing values. This must be (an 
#' abbreviation of) one of the strings "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is 
#' "pairwise.complete.obs".
#' @param ... optional arguments passed to \code{\link[stats]{cov}}.
#' 
#' @return A list containing 
#' \item{Sd.fm}{length-1 vector of factor model SDs of portfolio return.}
#' \item{mSd}{length-(N + K) vector of marginal contributions to SD.}
#' \item{cSd}{length-(N + K) vector of component contributions to SD.}
#' \item{pcSd}{length-(N + K) vector of percentage component contributions to SD.}
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
#' decomp <- portSdDecomp(fit.macro)
#' # get the factor contributions of risk
#' decomp$cSd
#' # random weights
#' wts = runif(6)
#' wts = wts/sum(wts)
#' portSdDecomp(fit.macro, wts) 
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

portSdDecomp.tsfm <- function(object, weights = NULL, use="pairwise.complete.obs", ...) {
  
  # get beta.star: 1 x (K+N)
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
  beta.star <- as.matrix(cbind(weights %*% as.matrix(beta), t(weights * object$resid.sd)))  

  # get cov(F): K x K
  factor <- as.matrix(object$data[, object$factor.names])
  factor.cov = cov(factor, use=use, ...)
  
  # get cov(F.star): (K+N) x (K+N)
  K <- ncol(object$beta)
  factor.star.cov <- diag(K+n.assets)
  factor.star.cov[1:K, 1:K] <- factor.cov
  colnames(factor.star.cov) <- c(colnames(factor.cov),asset.names)
  rownames(factor.star.cov) <- c(colnames(factor.cov),asset.names)
  
  # compute factor model sd; a vector of length 1
  Sd.fm <- sqrt(rowSums(beta.star %*% factor.star.cov * beta.star))
  
  # compute marginal, component and percentage contributions to sd
  # each of these have dimensions: N + K
  mSd <- (t(factor.star.cov %*% t(beta.star)))/Sd.fm 
  cSd <- mSd * beta.star 
  pcSd = 100* cSd/Sd.fm 
  
  fm.sd.decomp <- list(Sd.fm=Sd.fm, mSd=mSd, cSd=cSd, pcSd=pcSd)
  
  return(fm.sd.decomp)
}

#' @rdname portSdDecomp
#' @method portSdDecomp ffm
#' @export

portSdDecomp.ffm <- function(object, weights = NULL, ...) {
  
  which.numeric <- sapply(object$data[,object$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- object$exposure.vars[which.numeric]
  exposures.char <- object$exposure.vars[!which.numeric]
  
  # get beta.star: 1 x (K+N)
  if(!length(exposures.char)){
    beta <- object$beta[,-1]
  }else{
    beta <- object$beta
  }
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
    weights = weights[asset.names]
  } 
  
  # get portfolio beta.star: 1 x (K+N)
  beta.star <- as.matrix(cbind(weights %*% beta, t(weights * sqrt(object$resid.var))))
  
  # get cov(F): K x K
  if(!length(exposures.char)){
    factor.cov = object$factor.cov[,-1][-1,]
  }else{
    factor.cov = object$factor.cov
  }  

  
  # get cov(F.star): (K+N) x (K+N)
  K <- ncol(beta)
  factor.star.cov <- diag(K+n.assets)
  factor.star.cov[1:K, 1:K] <- factor.cov
  colnames(factor.star.cov) <- c(colnames(factor.cov),asset.names)
  rownames(factor.star.cov) <- c(colnames(factor.cov),asset.names)
  
  # compute factor model sd; a vector of length 1
  Sd.fm <- sqrt(rowSums(beta.star %*% factor.star.cov * beta.star))
  
  # compute marginal, component and percentage contributions to sd
  # each of these have dimensions: N+K
  mSd <- drop((t(factor.star.cov %*% t(beta.star)))/Sd.fm)
  cSd <- drop(mSd * beta.star)
  pcSd <- drop(100* cSd/Sd.fm) 
  
  fm.sd.decomp <- list(Sd.fm=Sd.fm, mSd=mSd, cSd=cSd, pcSd=pcSd)
  
  return(fm.sd.decomp)
}
