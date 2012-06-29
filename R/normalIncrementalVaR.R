normalIncrementalVaR <-
function(mu, Sigma, w, tail.prob = 0.01) {
## compute normal incremental VaR given portfolio weights, mean vector and
## covariance matrix
## Incremental VaR is defined as the change in portfolio VaR that occurs
## when an asset is removed from the portfolio
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## output:
## iVaR      n x 1 vector of incremental VaR values
## References:
## Jorian (2007) pg. 168
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  
  n.w = nrow(mu)
  ## portfolio VaR with all assets
  pVaR = crossprod(w, mu) + sqrt( t(w) %*% Sigma %*% w ) * qnorm(tail.prob)
  temp.w = w
  iVaR = matrix(0, n.w, 1)
  for (i in 1:n.w) {
  ## set weight for asset i to zero and renormalize
    temp.w[i,1] = 0
    temp.w = temp.w/sum(temp.w)
    pVaR.new = crossprod(temp.w, mu) + sqrt( t(temp.w) %*% Sigma %*% temp.w ) * qnorm(tail.prob)    
    iVaR[i,1] = pVaR.new - pVaR
  ## reset weight
    temp.w = w
  }
  return(-iVaR)
}

