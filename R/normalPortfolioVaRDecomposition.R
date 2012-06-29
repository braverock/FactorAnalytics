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

