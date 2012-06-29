normalVaRReport <-
function(mu, Sigma, w, tail.prob = 0.01,
                           nav, nav.p, fundStrategy,fundNames) {
## compute VaR by normal distibution report for collection of assets in a portfolio given
## simulated (bootstrapped) return data 
## Report format follows that of Excel VaR report
## inputs:
## inputs:
## mu         n x 1 vector of expected returns
## Sigma      n x n return covariance matrix
## w          n x 1 vector of portfolio weights
## tail.prob  scalar tail probability
## nav            n x 1 vector of net asset values in each fund
## nav.p          scalar, net asset value of portfolio. 
## fundStrategy   n x 1 vector of fund strategies
## output:
## VaRreport.df   dataframe with the following columns
  
##               dollar allocation
##               percent allocation   
## w             n x 1 vector of asset weights
## aVaR           n x 1 vector of asset specific VaR values
## mVaR           n x 1 vector of asset specific marginal VaR values
## iVaR           n x 1 vector of asset specific incremental VaR values
## cVaR           n x 1 vector of asset specific component VaR values
## pcVaR          n x 1 vector of asset specific percent contribution to VaR values
##
##  To-do: Add information for cash position. 
  mu = as.matrix(mu)
  Sigma = as.matrix(Sigma)
  w = as.matrix(w)
  if ( nrow(mu) != nrow(Sigma) )
    stop("mu and Sigma must have same number of rows")
  if ( nrow(mu) != nrow(w) )
    stop("mu and w must have same number of elements")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  nav = as.matrix(nav)
  nVaR <- normalPortfolioVaRDecomposition(mu, Sigma, w, tail.prob)
  # compute individual VaR  mu + sigma*qnorm(tail.prob)
  asset.VaR = sapply(1:nrow(mu),function(i) -mu[i] - sqrt(Sigma[i,i])*(qnorm(tail.prob))  )
  portfolio.VaR = as.numeric(nVaR$VaR.p)
  marginal.VaR = as.numeric(nVaR$mVaR)
  component.VaR = as.numeric(nVaR$cVaR)
  incremental.VaR = as.numeric(normalIncrementalVaR(mu, Sigma, w, tail.prob))
  
    
  VaRreport.df = data.frame(Strategy = fundStrategy,
                            Net.Asset.Value = nav,
                            Allocation = as.numeric(w),
                            Mean = as.numeric(mu),
                            Std.Dev = as.numeric(diag(Sigma)),
                            Asset.VaR = asset.VaR,
                            cVaR = component.VaR,
                            cVaR.dollar = component.VaR*nav.p, 
                            pcVaR = component.VaR/portfolio.VaR,                                                 
                            iVaR = incremental.VaR,
                            iVaR.dollar = incremental.VaR*nav.p,
                            mVaR = marginal.VaR, 
                            mVaR.dollar = marginal.VaR*nav.p)
  rownames(VaRreport.df) = fundNames                            
  return(VaRreport.df)
}

