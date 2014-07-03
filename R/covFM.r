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
#' @param object fit object of class \code{tsfm}, \code{sfm} or \code{ffm}.
#' 
#' @return The computed \code{N x N} covariance matrix for asset returns based 
#' on the fitted factor model.
#' 
#' @author Eric Zivot, Yi-An Chen and Sangeetha Srinivasan.
#' 
#' @references 
#' \enumerate{
#' \item Zivot, Eric, and W. A. N. G. Jia-hui. "Modeling Financial Time Series 
#' with S-Plus Springer-Verlag." (2006).
#' }
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
#'                add.up.market=FALSE, add.market.sqd=FALSE, fit.method="OLS")                              
#' covFM(fit)
#' 
#' # Statistical Factor Model
#' data(stat.fm.data)
#' sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat, k=2)
#' #' covFM(t(sfm.pca.fit$loadings), var(sfm.pca.fit$factors), 
#'                          sfm.pca.fit$resid.sd)
#' 
#' sfm.apca.fit <- fitSFM(sfm.apca.dat, k=2)
#' 
#' covFM(t(sfm.apca.fit$loadings), var(sfm.apca.fit$factors), 
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
#' ret.cov.fundm <- covFM(beta.mat1, fit.fund$factor.cov$cov, 
#'                                        fit.fund$resid.sd)
#' fit.fund$returns.cov$cov == ret.cov.fundm
#' }
#' 
#' @rdname covFM
#' @export

covFM <- function(object){
UseMethod("covFM")
}
