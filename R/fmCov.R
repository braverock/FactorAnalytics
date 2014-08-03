#' @title Covariance Matrix for assets' returns from fitted factor model.
#' 
#' @description Computes the covariance matrix for assets' returns based on a 
#' fitted factor model. This is a generic function with methods for classes 
#' \code{tsfm}, \code{sfm} and \code{ffm}.
#' 
#' @details \code{R(i, t)}, the return on asset \code{i} at time \code{t}, 
#' is assumed to follow a factor model of the form, \cr \cr 
#' \code{R(i,t) = alpha(i) + beta*F(t) + e(i,t)}, \cr \cr  
#' where, \code{alpha(i)} is the intercept, \code{F(t)} is a {K x 1} vector of 
#' the \code{K} factor values at time \code{t}, \code{beta} is a \code{1 x K} 
#' vector of factor exposures and the error terms \code{e(i,t)} are serially 
#' uncorrelated across time and contemporaneously uncorrelated across assets 
#' so that \code{e(i,t) ~ iid(0,sig(i)^2)}. Thus, the variance of asset 
#' \code{i}'s return is given by \cr \cr
#' \code{var(R(i,t)) = beta*var(F(t))*tr(beta) + sig(i)^2}. \cr \cr
#' And, the \code{N x N} covariance matrix of N asset returns is \cr \cr
#' \code{var(R) = B*var(F(t))*tr(B) + D}, \cr \cr 
#' where, B is the \code{N x K} matrix of factor betas and \code{D} is a 
#' diagonal matrix with \code{sig(i)^2} along the diagonal.
#' 
#' The method for computing covariance can be specified via the \dots 
#' argument. Note that the default of \code{use="pairwise.complete.obs"} for 
#' handling NAs restricts the method to "pearson".
#' 
#' @param object fit object of class \code{tsfm}, \code{sfm} or \code{ffm}.
#' @param use an optional character string giving a method for computing 
#' covariances in the presence of missing values. This must be (an 
#' abbreviation of) one of the strings "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is 
#' "pairwise.complete.obs".
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
#'                factor.names=c("EDHEC LS EQ","SP500 TR"), data=managers)                              
#' fmCov(fit)
#' 
#' \dontrun{
#' # Statistical Factor Model
#' data(stat.fm.data)
#' sfm.pca.fit <- fitSfm(sfm.dat, k=2)
#' #' fmCov(t(sfm.pca.fit$loadings), var(sfm.pca.fit$factors), 
#'                          sfm.pca.fit$resid.sd)
#' 
#' sfm.apca.fit <- fitSfm(sfm.apca.dat, k=2)
#' 
#' fmCov(t(sfm.apca.fit$loadings), var(sfm.apca.fit$factors), 
#'                       sfm.apca.fit$resid.sd)
#'
#' # Fundamental Factor Model
#' data(stock)
#' # there are 447 assets  
#' exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
#' beta.mat <- subset(stock, DATE=="2003-12-31")[, exposure.names]
#' beta.mat1 <- cbind(rep(1, 447), beta.mat1)
#' # FM return covariance 
#' fit.fund <- fitFfm(exposure.names=c("BOOK2MARKET", "LOG.MARKETCAP"), 
#'                    data=stock, returnsvar="RETURN", datevar="DATE", 
#'                    assetvar="TICKER", wls=TRUE, regression="classic", 
#'                    covariance="classic", full.resid.cov=FALSE)
#' ret.cov.fundm <- fmCov(beta.mat1,fit.fund$factor.cov$cov,fit.fund$resid.sd)
#' fit.fund$returns.cov$cov == ret.cov.fundm
#' }
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

fmCov.tsfm <- function(object, use="pairwise.complete.obs", ...) {
  
  # get parameters and factors from factor model
  beta <- as.matrix(object$beta)
  beta[is.na(beta)] <- 0
  sig2.e = object$resid.sd^2
  factor <- as.matrix(object$data[, object$factor.names])
  
  # factor covariance matrix 
  factor.cov = cov(factor, use=use, ...)
  
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
