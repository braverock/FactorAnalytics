#' @title Factor model Covariance Matrix for assets' returns.
#' 
#' @description Computes the covariance matrix for assets' returns based on a 
#' fitted factor model.
#' 
#' @details The return on asset \code{i} is assumed to follow a factor model 
#' of the form, \cr \cr \code{R(i,t) = alpha + beta*F(t) + e(i,t)}, \cr \cr  
#' where, \code{e(i,t) ~ iid(0,sig(i)^2)}, \code{beta} is a \code{1 x K} vector 
#' of factor exposures and the error terms are serially uncorrelated and 
#' contenporaneously uncorrelated across assets. Thus, the variance of asset 
#' \code{i}'s return is given by \cr \cr
#' \code{var(R(i,t)) = beta*var(F(t))*tr(beta) + sig(i)^2}. \cr \cr
#' And, the \code{N x N} covariance matrix of N asset returns is \cr \cr
#' \code{var(R) = B*var(F(t))*tr(B) + D}, \cr \cr 
#' where, B is the \code{N x K} matrix of asset betas and \code{D} is a diagonal 
#' matrix with \code{sig(i)^2} along the diagonal.
#' 
#' @param beta an \code{N x K} matrix of factor betas, where \code{N} is the
#' number of assets and \code{K} is the number of factors.
#' @param factor.cov a \code{K x K} factor covariance matrix.
#' @param resid.sd an \code{N x 1} vector of asset specific residual
#' volatilities from the factor model.
#' 
#' @return The computed \code{N x N} covariance matrix for asset returns based 
#' on the given factor model parameters.
#' 
#' @author Eric Zivot, Yi-An Chen and Sangeetha Srinivasan.
#' 
#' @references Zivot, E. and J. Wang (2006), \emph{Modeling Financial Time
#' Series with S-PLUS, Second Edition}, Springer-Verlag.
#' 
#' @seealso \code{\link{fitTSFM}}, \code{\link{fitSFM}}, \code{\link{fitFFM}}
#' 
#' @examples
#' \dontrun{
#' # Time Series Factor model
#' data(managers.df)
#' factors = managers.df[, (7:9)]
#' fit <- fitTSFM(assets.names=colnames(managers.df[, (1:6)]), 
#'                factors.names=c("EDHEC.LS.EQ","SP500.TR"), data=managers.df, 
#'                fit.method="OLS")
#' factors = managers.df[, (7:8)]                               
#' factorModelCovariance(fit$beta, var(factors), fit$resid.sd)
#' 
#' # Statistical Factor Model
#' data(stat.fm.data)
#' sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat, k=2)
#' #' factorModelCovariance(t(sfm.pca.fit$loadings), var(sfm.pca.fit$factors), 
#'                          sfm.pca.fit$resid.sd)
#' 
#' sfm.apca.fit <- fitSFM(sfm.apca.dat, k=2)
#' 
#' factorModelCovariance(t(sfm.apca.fit$loadings), var(sfm.apca.fit$factors), 
#'                       sfm.apca.fit$resid.sd)
#'
#' # Fundamental Factor Model
#' data(stock)
#' # there are 447 assets  
#' exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
#' beta.mat <- subset(stock, DATE=="2003-12-31")[, exposure.names]
#' beta.mat1 <- cbind(rep(1, 447), beta.mat1)
#' # FM return covariance 
#' fit.fund <- fitFFM(exposure.names=c("BOOK2MARKET", "LOG.MARKETCAP"), 
#'                    data=stock, returnsvar="RETURN", datevar="DATE", 
#'                    assetvar="TICKER", wls=TRUE, regression="classic", 
#'                    covariance="classic", full.resid.cov=FALSE)
#' ret.cov.fundm <- factorModelCovariance(beta.mat1, fit.fund$factor.cov$cov, 
#'                                        fit.fund$resid.sd)
#' fit.fund$returns.cov$cov == ret.cov.fundm
#' }
#' @export
#' 

factorModelCovariance <- function(beta, factor.cov, resid.sd) {
  
  beta = as.matrix(beta)
  factor.cov = as.matrix(factor.cov)
  sig2.e = as.vector(resid.sd)^2
  
  if (length(sig.e) > 1) {
    D.e = diag(as.vector(sig2.e))
  } else {
    D.e =  as.matrix(sig2.e)
  }
  
  if (ncol(beta) != ncol(factor.cov)) {
    stop("'beta' and 'factor.cov' must have same number of columns.")
  }
  
  if (nrow(D.e) != nrow(beta)) {
    stop("'beta' and 'D.e' must have same number of rows.")
  }
  
  cov.fm = beta %*% factor.cov %*% t(beta) + D.e
  
  if (any(diag(chol(cov.fm)) == 0)) {
    warning("Covariance matrix is not positive definite!")
  }
  
  return(cov.fm)
}

