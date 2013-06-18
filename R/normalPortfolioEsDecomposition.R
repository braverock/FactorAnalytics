#' Compute portfolio ES decomposition by assets given normality assumption.
#' 
#' compute normal portfolio ES, marginal contribution to portfolio ES,
#' component contribution to portfolio ES and percentage component contribution
#' to portfolio ES given portfolio weights, mean vector and covariance matrix.
#' 
#' 
#' @param mu n x 1 vector of expected returns.
#' @param Sigma n x n return covariance matrix.
#' @param w n x 1 vector of portfolio weights.
#' @param tail.prob scalar tail probability.
#' @return an S3 list containing
#' @returnItem ES.p Scalar, portfolio ES reported as a positive number.
#' @returnItem mES 1 x n matrix of marginal contribution to portfolio ES values
#' for each fund.
#' @returnItem cES 1 x n matrix of component contribution to portfolio ES
#' values.
#' @returnItem pcES 1 x n matrix of percent component contributions to
#' portfolio ES values.
#' @author Eric Zivot and Yi-An Chen
#' @examples
#' 
#' # from data
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' mu <- mean(ret.assets[,1:3])
#' Sigma <- var(ret.assets[,1:3])
#' w <- rep(1/3,3)
#' normalPortfolioEsDecomposition(mu,Sigma,w)
#' 
#' # given some multinormal distribution
#' # compute 2 asset returns ES decomposition given multinormal distribution assumption.
#' normalPortfolioEsDecomposition(mu=c(1,2),Sigma=diag(2),w=c(0.5,0.5),tail.prob = 0.01)
#' 
normalPortfolioEsDecomposition <-
function(mu, Sigma, w, tail.prob = 0.01) {
## compute normal portfolio ES given portfolio weights, mean vector and
## covariance matrix
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## ES.p        scalar left tail average return of normal portfolio distn returned
##            as a positive number.
## mES        1 x n matrix of marginal ES values for each fund
## cES        1 x n matrix of component ES values
## pcES       1 x n matrix of percent contributions to portfolio ES values
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  # compute the portfolio ES
  pES = crossprod(w, mu) - sqrt( t(w) %*% Sigma %*% w )*dnorm(qnorm(tail.prob))/tail.prob
  # marginal ES
  mES = mu - as.numeric((1/sqrt( t(w) %*% Sigma %*% w ))*dnorm(qnorm(tail.prob))/tail.prob)* (Sigma%*%w)
  # component ES and percentage contribuiton to ES
  cES = mES*w
  pcES = cES/as.numeric(pES)
  # show pES, mES and cES as positive number
colnames(mES) = "MCES"
colnames(cES) = "CES"
colnames(pcES) = "PCES"

ans = list(ES.p = -pES,
           mES = t(-mES),
           cES = t(-cES),
           pcES = t(pcES))
  return(ans)
}

