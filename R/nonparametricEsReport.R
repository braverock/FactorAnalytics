#' compute ES report via nonparametric method for collection of assets in a
#' portfolio given simulated (bootstrapped) return data.
#' 
#' compute ES report via nonparametric method for collection of assets in a
#' portfolio given simulated (bootstrapped) return data Report format follows
#' that of Excel VaR report.
#' 
#' 
#' @param bootData B x n matrix of B bootstrap returns on assets in portfolio.
#' @param w n x 1 vector of portfolio weights.
#' @param delta.w scalar, change in portfolio weight for computing numerical
#' derivative. Default value is 0.010.
#' @param tail.prob scalar tail probability.
#' @param method character, method for computing marginal ES. Valid choices are
#' "derivative" for numerical computation of the derivative of portfolio ES wrt
#' fund portfolio weight; "average" for approximating E[Ri | Rp<=VaR]
#' @param nav n x 1 vector of net asset values in each fund.
#' @param nav.p scalar, net asset value of portfolio percentage.
#' @param fundStrategy n x 1 vector of fund strategies.
#' @param i1,i2 if ff object is used, the ffapply functions do apply an
#' EXPRession and provide two indices FROM="i1" and TO="i2", which mark
#' beginning and end of the batch and can be used in the applied expression.
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
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' nonparametricEsReport (bootData= ret.assets[,1:3], w=c(1/3,1/3,1/3), delta.w = 0.001, 
#'                        tail.prob = 0.01, 
#'                        method="derivative",nav=c(100,200,100), nav.p=500,
#'                        fundStrategy=c("S1","S2","S3"))
#' 
nonparametricEsReport <-
function(bootData, w, delta.w = 0.001, tail.prob = 0.01, 
                              method=c("derivative", "average"),
                              nav, nav.p, fundStrategy,i1,i2) {
## compute ES report for collection of assets in a portfolio given
## simulated (bootstrapped) return data 
## Report format follows that of Excel VaR report
## inputs:
## bootData       B x n matrix of B bootstrap returns on assets in portfolio. 
## w              n x 1 vector of portfolio weights
## delta.w        scalar, change in portfolio weight for computing numerical derivative.
##                Default value is 0.01.
## tail.prob      scalar tail probability
## method         character, method for computing marginal ES. Valid choices are 
##                "derivative" for numerical computation of the derivative of portfolio
##                ES wrt fund portfolio weight; "average" for approximating E[Ri | Rp<=VaR]
## nav            n x 1 vector of net asset values in each fund
## nav.p          scalar, net asset value of portfolio percentage.
## i1,i2       if ff object is used,  the ffapply functions do apply an EXPRession and 
##                provide two indices FROM="i1" and TO="i2", which mark beginning and end 
##                of the batch and can be used in the applied expression.  
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
  require(PerformanceAnalytics)
  require(ff)
  method = method[1]
  if (!is.ff(bootData))
    bootData = as.matrix(bootData)
  w = as.matrix(w)
  nav = as.matrix(nav)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")
  nES <- nonparametricPortfolioEsDecomposition(bootData, w, delta.w, tail.prob,method)
  asset.ES = as.numeric(-ES(R=bootData,p=1-tail.prob,method="historical"))
  portfolio.ES = as.numeric(nES$ES.p)
  marginal.ES = as.numeric(nES$mES)
  component.ES = as.numeric(nES$cES)
  incremental.ES = as.numeric(nonparametricIncrementalES(bootData, w, tail.prob))
  
  if (is.ff(bootData)) {
    mean.vals = ffrowapply(colMeans(bootData[i1:i2,,drop=FALSE]), 
                           X=bootData, RETURN=TRUE, CFUN="cmean")
    sd.vals = ffrowapply(colMeans(bootData[i1:i2,,drop=FALSE]^2) - colMeans(bootData[i1:i2,,drop=FALSE])^2,
                         X=bootData, RETURN=TRUE, CFUN="cmean")
    sd.vals = sqrt(sd.vals)                                            
  } else {
    mean.vals = colMeans(bootData)
    sd.vals = apply(bootData, 2, sd)
  }
  
  ESreport.df = data.frame(Strategy = fundStrategy,
                            Net.Asset.Value = nav,
                            Allocation = w,
                            Mean = mean.vals,
                            Std.Dev = sd.vals,
                            Asset.ES = asset.ES,
                            cES = component.ES,
                            cES.dollar = component.ES*nav.p, 
                            pcES = component.ES/portfolio.ES,                                                 
                            iES = incremental.ES,
                            iES.dollar = incremental.ES*nav.p,
                            mES = marginal.ES, 
                            mES.dollar = marginal.ES*nav.p)
  rownames(ESreport.df) = colnames(bootData)                            
  return(ESreport.df)
}

