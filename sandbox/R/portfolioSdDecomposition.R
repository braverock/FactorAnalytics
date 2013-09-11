#' Compute portfolio sd (risk) decomposition by asset.
#' 
#' Compute portfolio sd (risk) decomposition by asset.
#' 
#' 
#' @param w.vec N x 1 vector of portfolio weights
#' @param cov.assets N x N asset covariance matrix
#' @return an S3 list containing
#' @returnItem sd.p Scalar, portfolio standard deviation.
#' @returnItem mcsd.p 1 x N vector, marginal contributions to portfolio
#' standard deviation.
#' @returnItem csd.p 1 x N vector, contributions to portfolio standard
#' deviation.
#' @returnItem pcsd.p 1 x N vector, percent contribution to portfolio standard
#' deviation.
#' @author Eric Zivot and Yi-An Chen
#' @references Qian, Hua and Sorensen (2007) Quantitative Equity Portfolio
#' Management, chapter 3.
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' factors    = managers.df[,(7:9)]
#' # fit the factor model with OLS
#' fit <-fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
#'                                   variable.selection="all subsets", factor.set = 3)
#' # factor SD decomposition for HAM1
#' cov.factors = var(factors)
#' manager.names = colnames(managers.df[,(1:6)])
#' factor.names  = colnames(managers.df[,(7:9)])
#' # assuming equal weight vector
#' w.vec = rep(1/6,6)
#' # compute with sample covariance matrix (omit NA)
#' cov.sample = ccov(managers.df[,manager.names],na.action=na.omit)
#' port.sd.decomp.sample = portfolioSdDecomposition(w.vec, cov.sample$cov)
#' # show bar chart
#' barplot(port.sd.decomp.sample$pcsd.p,
#'         main="Fund Percent Contributions to Portfolio SD",
#'         ylab="Percent Contribution", legend.text=FALSE,
#'         col="blue")
#' 
#' # compute with factor model covariance matrix
#' returnCov.mat<- factorModelCovariance(fit$beta.mat,var(factors),fit$residVars.vec)
#' port.sd.decomp.fm = portfolioSdDecomposition(w.vec, returnCov.mat)                                                         
#' 
#' 
portfolioSdDecomposition <-
function(w.vec, cov.assets) {
## Inputs:
## w.vec         n x 1 vector of portfolio weights
## cov.assets		 n x n asset covariance matrix
## Output:
## A list with the following components:
## sd.p          scalar, portfolio sd
## mcsd.p        1 x n vector, marginal contributions to portfolio sd
## csd.p         1 x n vector, contributions to portfolio sd
## pcsd.p        1 x n vector, percent contribution to portfolio sd
	
 if (any(diag(chol(cov.assets)) == 0))
    warning("Asset covariance matrix is not positive definite")
 ## compute portfolio level variance
 var.p = as.numeric(t(w.vec) %*% cov.assets %*% w.vec)
 sd.p = sqrt(var.p)
 ## compute marginal, component and percentage contributions to risk
 mcsd.p = (cov.assets %*% w.vec)/sd.p
 csd.p = w.vec*mcsd.p
 pcsd.p = csd.p/sd.p
 colnames(mcsd.p) = "MCSD"
 colnames(csd.p) = "CSD"
 colnames(pcsd.p) = "PCSD"
 ## return results
 ans = list(sd.p=sd.p,
            mcsd.p=t(mcsd.p),
            csd.p=t(csd.p),
            pcsd.p=t(pcsd.p))
 return(ans)
}

