#' compute VaR report with normal distribution for collection of assets in a
#' portfolio given portfolio weights, mean vector and covariance matrix.
#' 
#' compute VaR report with normal distribution for collection of assets in a
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
#' each funds.  Assets.VaR n x 1 vector of asset specific VaR values. cVaR n x
#' 1 vector of asset specific component VaR values. cVaR.dollar n x 1 vector of
#' asset specific component VaR values in dollar terms. pcVaR n x 1 vector of
#' asset specific percent contribution to VaR values. iVaR n x 1 vector of
#' asset specific incremental VaR values. iVaR.dollar n x 1 vector of asset
#' specific component VaR values in dollar terms. mVaR n x 1 vector of asset
#' specific marginal VaR values. mVaR.dollar n x 1 vector of asset specific
#' marginal VaR values in dollar terms.
#' @author Eric Zivot and Yi-An Chen.
#' @examples
#' 
#' # from data
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' mu <- mean(ret.assets[,1:3])
#' Sigma <- var(ret.assets[,1:3])
#' w <- rep(1/3,3)
#' normalVaRReport(mu,Sigma,w,nav=c(100,200,100), nav.p=500, fundStrategy=c("S1","S2","S3"),fundNames=colnames(ret.assets[,1:3]))
#' 
#' # given some multinormal distribution
#' normalVaRReport(mu=c(1,2),Sigma=matrix(c(1,0.5,0.5,3),2,2),w=c(0.5,0.5),tail.prob = 0.01,
#'                     nav=c(100,100), nav.p=200, fundStrategy=c("S1","S2"),fundNames=c("N1","N2"))
#' 
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

