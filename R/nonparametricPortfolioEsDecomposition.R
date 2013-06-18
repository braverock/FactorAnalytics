#' Compute portfolio ES (risk) decomposition by assets.
#' 
#' Compute portfolio ES decomposition given historical or simulated data and
#' portfolio weights. Marginal ES is computed either as the numerical
#' derivative of ES with respect to portfolio weight or as the expected fund
#' return given portfolio return is less than or equal to portfolio VaR VaR is
#' compute as the sample quantile.
#' 
#' 
#' @param bootData B x N matrix of B bootstrap returns on assets in portfolio.
#' @param w N x 1 vector of portfolio weights
#' @param delta.w Scalar, change in portfolio weight for computing numerical
#' derivative.
#' @param tail.prob Scalar, tail probability.
#' @param method Character, method for computing marginal ES. Valid choices are
#' "derivative" for numerical computation of the derivative of portfolio ES
#' with respect to fund portfolio weight; "average" for approximating E[R_i |
#' R_p<=VaR].
#' @return an S3 list containing
#' @returnItem VaR.p Scalar, portfolio VaR reported as a positive number.
#' @returnItem ES.p Scalar, portfolio ES reported as a positive number.
#' @returnItem n.exceed Scalar, number of VaR exceedences.
#' @returnItem idx.exceed n.exceed x 1 vector of exceedence indices.
#' @returnItem mES 1 x n matrix of marginal ES values for each fund.
#' @returnItem cES 1 x n matrix of component ES values.
#' @returnItem pcES 1 x n matrix of percent contributions to portfolio ES
#' values.
#' @note This functionality is provided by the ES() function in
#' PerformanceAnalytics.
#' @author Eric Zivot and Yi-An Chen.
#' @references 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A
#' General Analysis", The Journal of Risk 5/2. 2. Yamai and Yoshiba (2002).
#' "Comparative Analyses of Expected Shortfall and Value-at-Risk: Their
#' Estimation Error, Decomposition, and Optimization Bank of Japan.
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' nonparametricPortfolioEsDecomposition(ret.assets[,1:3], w=c(1/3,1/3,1/3), 
#'                                       delta.w = 0.001, tail.prob = 0.01, 
#'                                       method=c("derivative"))
#' 
nonparametricPortfolioEsDecomposition <-
function(bootData, w, delta.w = 0.001, tail.prob = 0.01,
                                                   method=c("derivative", "average")) {
## Compute portfolio ES decomposition given historical or simulated data and portfolio weights.
## Marginal ES is computed either as the numerical derivative of ES wrt portfolio weight or
## as the expected fund return given portfolio return is less than or equal to portfolio VaR
## VaR is compute either as the sample quantile 
## inputs:
## bootData   B x n matrix of B bootstrap returns on assets in portfolio.
## w          n x 1 vector of portfolio weights
## delta.w    scalar, change in portfolio weight for computing numerical derivative
## tail.prob  scalar, tail probability 
## method     character, method for computing marginal ES. Valid choices are
##            "derivative" for numerical computation of the derivative of portfolio
##            ES wrt fund portfolio weight; "average" for approximating E[Ri | Rp<=VaR]
## output:
## pVaR       scalar, portfolio VaR reported as a positive number
## pES        scalar, portfolio ES reported as a positive number
## n.exceed   scalar, number of VaR exceedences
## idx.exceed n.exceed x 1 vector of exceedence indices
## mES        1 x n matrix of marginal ES values for each fund
## cES        1 x n matrix of component ES values
## pcES       1 x n matrix of percent contributions to portfolio ES values
## References:
## 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
##    The Journal of Risk 5/2.
## 2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and
##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
##    Bank of Japan.
  require(PerformanceAnalytics)
  method = method[1]
  bootData = as.matrix(bootData)
  w = as.matrix(w)
  if ( ncol(bootData) != nrow(w) )
    stop("Columns of bootData and rows of w do not match")
  if ( tail.prob < 0 || tail.prob > 1)
    stop("tail.prob must be between 0 and 1")

  n.w = nrow(w)

  ## portfolio VaR and ES with all assets
  r.p = bootData %*% w
  pVaR = quantile(r.p, prob=tail.prob)
  idx = which(r.p <= pVaR)
  pES = -mean(r.p[idx])
  
  ##
  ## compute marginal ES
  ##
  if (method=="derivative") {
  ## compute marginal ES as derivative wrt portfolio weight
    temp.w = w
    mES = matrix(0, n.w, 1)
    for (i in 1:n.w) {
    ## increment weight for asset i by delta.w
      temp.w[i,1] = w[i,1] + delta.w
      temp.r.p = bootData %*% temp.w
      pVaR.new = quantile(temp.r.p, prob=tail.prob)
      idx = which(temp.r.p <= pVaR.new)
      pES.new = -mean(temp.r.p[idx])
      mES[i,1] = (pES.new - pES)/delta.w
    ## reset weight
      temp.w = w
    }
  } else {
  ## compute marginal ES as expected value of fund return given portfolio
  ## return is less than or equal to portfolio VaR
    if (ncol(bootData) > 1) {
           if (length(idx)==1){
        mES = - as.matrix(bootData[idx,])
      } else{
         mES = -as.matrix(apply(bootData[idx,],2,mean))
      }
        } else {
      mES = -as.matrix(mean(bootData[idx, ]))
    }
  }
## compute correction factor so that sum of weighted marginal ES adds to portfolio VaR
cf = as.numeric( pES / sum(mES*w) )
## mES = cf*mES

## compute component and percent ES
cES = mES * w
pcES = cES/pES
rownames(mES) = colnames(bootData)
colnames(mES) = "MCES"
colnames(cES) = "CES"
colnames(pcES) = "PCES"

ans = list(VaR.p = -pVaR,
           ES.p = pES,
           n.exceed = length(idx),
           idx.exceed = idx,
           mES = t(mES),
           cES = t(cES),
           pcES = t(pcES))
return(ans)
}

