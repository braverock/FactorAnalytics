#' Compute portfolio VaR decomposition by assets given normality assumption.
#' 
#' compute normal portfolio VaR, marginal contribution to portfolio VaR,
#' component contribution to portfolio VaR and percentage contribution to
#' portfolio VaR given portfolio weights, mean vector and covariance matrix.
#' 
#' 
#' @param mu n x 1 vector of expected returns.
#' @param Sigma n x n return covariance matrix.
#' @param w n x 1 vector of portfolio weights.
#' @param tail.prob scalar tail probability.
#' @return an S3 list containing
#' @returnItem VaR.p Scalar, portfolio VaR reported as a positive number.
#' @returnItem mVaR 1 x n matrix of marginal contribution to portfolio VaR
#' values for each fund.
#' @returnItem cVaR 1 x n matrix of component contribution to portfolio VaR
#' values.
#' @returnItem pcVaR 1 x n matrix of percent contributions to portfolio VaR
#' values.
#' @author Eric Zivot and Yi-An Chen
#' @examples
#' 
#' # from data
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' mu <- mean(ret.assets[,1:3])
#' Sigma <- var(ret.assets[,1:3])
#' w <- rep(1/3,3)
#' normalPortfolioVaRDecomposition(mu,Sigma,w)
#' 
#' # given some multinormal distribution
#' # compute 2 asset returns ES decomposition given multinormal distribution assumption.
#' normalPortfolioVaRDecomposition(mu=c(1,2),Sigma=diag(2),w=c(0.5,0.5),tail.prob = 0.01)
#' 
normalPortfolioVaRDecomposition <-
function(mu, Sigma, w, tail.prob = 0.01) {
## compute normal portfolio VaR given portfolio weights, mean vector and
## covariance matrix
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## VaR.p       scalar left tail average return of normal portfolio distn returned
##            as a positive number.
## mVaR        1 x n matrix of marginal VaR values for each fund
## cVaR        1 x n matrix of component VaR values
## pcVaR       1 x n matrix of percent contributions to portfolio VaR values
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  # compute the portfolio VaR
  VaR.p = crossprod(w, mu) + sqrt( t(w) %*% Sigma %*% w ) * qnorm(tail.prob)
  # marginal ES
  sigma.p = as.numeric(sqrt( t(w) %*% Sigma %*% w ))
  mVaR = mu + Sigma %*% w * qnorm(tail.prob) / sigma.p
  # component ES and percentage contribuiton to ES
  cVaR = mVaR * w
  pcVaR = cVaR/as.numeric(VaR.p)
  # show pES, mES and cES as positive number
colnames(mVaR) = "MVaR"
colnames(cVaR) = "CVaR"
colnames(pcVaR) = "PCVaR"

ans = list(VaR.p = -VaR.p,
           mVaR = t(-mVaR),
           cVaR = t(-cVaR),
           pcvaR = t(pcVaR))
  return(ans)
}

