normalEsReport <-
function(mu, Sigma, w, tail.prob = 0.01,
                           nav, nav.p, fundStrategy,fundNames) {
## compute ES by normal distibution report for collection of assets in a portfolio given
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
## ESreport.df   dataframe with the following columns
  
##               dollar allocation
##               percent allocation   
## w             n x 1 vector of asset weights
## aES           n x 1 vector of asset specific ES values
## mES           n x 1 vector of asset specific marginal ES values
## iES           n x 1 vector of asset specific incremental ES values
## cES           n x 1 vector of asset specific component ES values
## pcES          n x 1 vector of asset specific percent contribution to ES values
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
  nES <- normalPortfolioEsDecomposition(mu, Sigma, w, tail.prob)
  # compute individual ES 
  asset.ES = sapply(1:nrow(mu),function(i) -(mu[i] - sqrt(Sigma[i,i])*dnorm(qnorm(tail.prob))/tail.prob))
  portfolio.ES = as.numeric(nES$ES.p)
  marginal.ES = as.numeric(nES$mES)
  component.ES = as.numeric(nES$cES)
  incremental.ES = as.numeric(normalIncrementalES(mu, Sigma, w, tail.prob))
  
    
  ESreport.df = data.frame(Strategy = fundStrategy,
                            Net.Asset.Value = nav,
                            Allocation = as.numeric(w),
                            Mean = as.numeric(mu),
                            Std.Dev = as.numeric(diag(Sigma)),
                            Asset.ES = asset.ES,
                            cES = component.ES,
                            cES.dollar = component.ES*nav.p, 
                            pcES = component.ES/portfolio.ES,                                                 
                            iES = incremental.ES,
                            iES.dollar = incremental.ES*nav.p,
                            mES = marginal.ES, 
                            mES.dollar = marginal.ES*nav.p)
  rownames(ESreport.df) = fundNames                            
  return(ESreport.df)
}

