modifiedIncrementalVaR <-
function(bootData, w, tail.prob = 0.01, i1,i2) {
## Compute incremental VaR given bootstrap data and portfolio weights.
## Incremental VaR is defined as the change in portfolio VaR that occurs
## when an asset is removed from the portfolio and allocation is spread equally
## among remaining assets. VaR is computed as an estimated 
## quantile using the Cornish-Fisher expansion
## inputs:
## bootData   B x n matrix of B bootstrap returns on n assets in portfolio
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## i1,i2       if ff object is used,  the ffapply functions do apply an EXPRession and 
##                provide two indices FROM="i1" and TO="i2", which mark beginning and end 
##                of the batch and can be used in the applied expression.  
## output:
## iVaR        n x 1 matrix of incremental VaR values for each asset
## References:
## Jorian, P. (2007). Value-at-Risk, pg. 168.
   require(PerformanceAnalytics)
  require(ff)
  if (!is.ff(bootData))
    bootData = as.matrix(bootData)
  w = as.matrix(w)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  
  if (ncol(bootData) == 1) {
  ## EZ: Added 12/2/2009
  ## one asset portfolio so iES = 0 by construction
    iES = 0
  } else { 
      
  n.w = nrow(w)
  ## portfolio VaR with all assets
  if (is.ff(bootData)) {
  ## use on disk ff object
    r.p = ffrowapply( bootData[i1:i2, ]%*%w, X=bootData, RETURN=TRUE, RETCOL=1)
  } else {
  ## use in RAM object
    r.p = bootData %*% w
  }
    ## use Cornish-Fisher VaR
    VaR.p = as.numeric(VaR(r.p, p=(1-tail.prob),method="modified"))
    temp.w = w
    iVaR = matrix(0, n.w, 1)
    rownames(iVaR) = colnames(bootData)
    colnames(iVaR) = "i.VaR"
    for (i in 1:n.w) {
    ## set weight for asset i to zero and renormalize
      temp.w[i,1] = 0
      temp.w = temp.w/sum(temp.w)
      if (is.ff(bootData)) {
        temp.r.p = ffrowapply( bootData[i1:i2, ]%*%temp.w, X=bootData, RETURN=TRUE, RETCOL=1)
      } else {
        temp.r.p = bootData %*% temp.w
      }   
      VaR.p.new = as.numeric(VaR(temp.r.p, p=(1-tail.prob),method="modified"))
      iVaR[i,1] = VaR.p.new - VaR.p
    ## reset weight
      temp.w = w
  }
  }
  return(iVaR)
}

