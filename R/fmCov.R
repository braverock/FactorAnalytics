#' @title Covariance Matrix for assets' returns from fitted factor model.
#' 
#' @description Computes the covariance matrix for assets' returns based on a 
#' fitted factor model. This is a generic function with methods for classes 
#' \code{tsfm}, \code{sfm} and \code{ffm}.
#' 
#' @details \code{R(i, t)}, the return on asset \code{i} at time \code{t}, 
#' is assumed to follow a factor model of the form, \cr \cr 
#' \code{R(i,t) = alpha(i) + beta(i)*f(t) + e(i,t)}, \cr \cr  
#' where, \code{alpha(i)} is the intercept, \code{f(t)} is a {K x 1} vector of 
#' factor returns at time \code{t}, \code{beta(i)} is a \code{1 x K} vector of 
#' factor exposures and the error terms \code{e(i,t)} are serially 
#' uncorrelated across time and contemporaneously uncorrelated across assets 
#' so that \code{e(i,t) ~ iid(0,sig(i)^2)}. Thus, the variance of asset 
#' \code{i}'s return is given by \cr \cr
#' \code{var(R(i)) = beta(i)*cov(F)*tr(beta(i)) + sig(i)^2}. \cr \cr
#' And, the \code{N x N} covariance matrix of asset returns is \cr \cr
#' \code{var(R) = B*cov(F)*tr(B) + D}, \cr \cr 
#' where, B is the \code{N x K} matrix of factor betas and \code{D} is a 
#' diagonal matrix with \code{sig(i)^2} along the diagonal.
#' 
#' The method for computing covariance can be specified via the \dots 
#' argument. Note that the default of \code{use="pairwise.complete.obs"} for 
#' handling NAs restricts the method to "pearson".
#' 
#' @param object fit object of class \code{tsfm}, \code{sfm} or \code{ffm}.
#' @param factor.cov factor covariance matrix (optional); defaults to the 
#' sample covariance matrix.
#' @param use method for computing covariances in the presence of missing 
#' values; one of "everything", "all.obs", "complete.obs", "na.or.complete", or 
#' "pairwise.complete.obs". Default is "pairwise.complete.obs".
#' @param ... optional arguments passed to \code{\link[stats]{cov}}.
#' 
#' @return The computed \code{N x N} covariance matrix for asset returns based 
#' on the fitted factor model.
#' 
#' @author Eric Zivot, Yi-An Chen and Sangeetha Srinivasan.
#' 
#' @references 
#' Zivot, E., & Jia-hui, W. A. N. G. (2006). Modeling Financial Time 
#' Series with S-Plus Springer-Verlag.
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{fitSfm}}, \code{\link{fitFfm}}
#' 
#' \code{\link[stats]{cov}} for more details on arguments \code{use} and 
#' \code{method}.
#' 
#' @examples
#' # Time Series Factor model
#' data(managers)
#' fit <- fitTsfm(asset.names=colnames(managers[, (1:6)]), 
#'                factor.names=c("EDHEC.LS.EQ","SP500.TR"), data=managers)                              
#' fmCov(fit)
#' 
#' # Statistical Factor Model
#' data(StockReturns)
#' sfm.pca.fit <- fitSfm(r.M, k=2)
#' fmCov(sfm.pca.fit)
#' 
#' # Fundamental factor Model
#' data(Stocks.df)
#' exposure.vars <- c("BOOK2MARKET", "LOG.MARKETCAP", "GICS.SECTOR")
#' fit2 <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars=exposure.vars)
#' fmCov(fit2)
#' 
#' @rdname fmCov
#' @export

fmCov <- function(object, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
  }
  UseMethod("fmCov")
}

#' @rdname fmCov
#' @method fmCov tsfm
#' @export

fmCov.tsfm <- function(object, factor.cov, use="pairwise.complete.obs", ...) {
  
  # get parameters and factors from factor model
  beta <- as.matrix(object$beta)
  # convert NAs to 0 to enable matrix multiplication
  beta[is.na(beta)] <- 0
  sig2.e = object$resid.sd^2
  factor <- as.matrix(object$data[, object$factor.names])
  
  # factor covariance matrix 
  if (missing(factor.cov)) {
    factor.cov = cov(factor, use=use, ...) 
  } else {
    identical(dim(factor.cov), as.integer(c(ncol(factor), ncol(factor))))
  }
  
  # residual covariance matrix D
  if (length(sig2.e) > 1) {
    D.e = diag(sig2.e)
  } else {
    D.e =  as.vector(sig2.e)
  }
  
  cov.fm = beta %*% factor.cov %*% t(beta) + D.e
  
  if (any(diag(chol(cov.fm))==0)) {
    warning("Covariance matrix is not positive definite!")
  }
  
  return(cov.fm)
}

#' @rdname fmCov
#' @method fmCov sfm
#' @export

fmCov.sfm <- function(object, use="pairwise.complete.obs", ...) {
  
  # already computed via fitSfm function
  return(object$Omega)
}

#' @rdname fmCov
#' @method fmCov ffm
#' @export

fmCov.ffm <- function(object, use="pairwise.complete.obs", ...) {
  
  # already computed via fitFfm function
  return(object$return.cov)
}
