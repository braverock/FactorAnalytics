#' compute ES report with normal distribution for collection of assets in a
#' portfolio given portfolio weights, mean vector and covariance matrix.
#' 
#' compute ES report with normal distribution for collection of assets in a
#' portfolio given portfolio weights, mean vector and covariance matrix. Report
#' format follows that of Excel VaR report.
#' 
#' 
#' @param mu n x 1 vector of expected returns.
#' @param Sigma n x n return covariance matrix.
#' @param w n x 1 vector of portfolio weights.
#' @param tail.prob scalar tail probability.
#' @param nav n x 1 vector of net asset values in each fund.
#' @param nav.p scalar, net asset value of portfolio.
#' @param fundStrategy n x 1 vector of fund strategies.
#' @param fundNames Name of the funds in the portfolio.
#' @return dataframe with the following columns: Strategy n x 1 strategy.
#' Net.Asset.value n x 1 net asset values.  Allocation n x 1 vector of asset
#' weights. Mean n x 1 mean of each funds. Std.Dev n x 1 standard deviation of
#' each funds.  Assets.ES n x 1 vector of asset specific ES values. cES n x 1
#' vector of asset specific component ES values. cES.dollar n x 1 vector of
#' asset specific component ES values in dollar terms. pcES n x 1 vector of
#' asset specific percent contribution to ES values. iES n x 1 vector of asset
#' specific incremental ES values. iES.dollar n x 1 vector of asset specific
#' component ES values in dollar terms. mES n x 1 vector of asset specific
#' marginal ES values. mES.dollar n x 1 vector of asset specific marginal ES
#' values in dollar terms.
#' @author Eric Zivot and Yi-An Chen.
#' @examples
#' 
#' # from data
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' mu <- mean(ret.assets[,1:3])
#' Sigma <- var(ret.assets[,1:3])
#' w <- rep(1/3,3)
#' normalEsReport(mu,Sigma,w,nav=c(100,200,100), nav.p=500, fundStrategy=c("S1","S2","S3"),fundNames=colnames(ret.assets[,1:3]))
#' 
#' # given some multinormal distribution
#' normalEsReport(mu=c(1,2),Sigma=matrix(c(1,0.5,0.5,3),2,2),w=c(0.5,0.5),tail.prob = 0.01,
#'                     nav=c(100,100), nav.p=200, fundStrategy=c("S1","S2"),fundNames=c("N1","N2"))
#' 
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

