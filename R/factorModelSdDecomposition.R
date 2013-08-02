#' Compute factor model factor risk (sd) decomposition for individual fund.
#' 
#' Compute factor model factor risk (sd) decomposition for individual fund.
#' 
#' 
#' @param beta.vec k x 1 vector of factor betas with factor names in the
#' rownames.
#' @param factor.cov k x k factor excess return covariance matrix.
#' @param sig2.e scalar, residual variance from factor model.
#' @return an S3 object containing
#' @returnItem sd.fm Scalar, std dev based on factor model.
#' @returnItem mcr.fm (K+1) x 1 vector of factor marginal contributions to risk
#' (sd).
#' @returnItem cr.fm (K+1) x 1 vector of factor component contributions to risk
#' (sd).
#' @returnItem pcr.fm (K+1) x 1 vector of factor percent contributions to risk
#' (sd).
#' @author Eric Zivot and Yi-An Chen
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' factors    = managers.df[,(7:9)]
#' # fit the factor model with OLS
#' fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS", 
#'                                  variable.selection="all subsets",
#'                                  factor.set = 3)
#' # factor SD decomposition for HAM1
#' cov.factors = var(factors)
#' manager.names = colnames(managers.df[,(1:6)])
#' factor.names  = colnames(managers.df[,(7:9)])
#' factor.sd.decomp.HAM1 = factorModelSdDecomposition(fit$beta.mat["HAM1",],
#'                                                    cov.factors, fit$residVars.vec["HAM1"])
#'  
#' @export                                       
#' 
factorModelSdDecomposition <-
function(beta.vec, factor.cov, sig2.e) {
## Inputs:
## beta   		   k x 1 vector of factor betas with factor names in the rownames
## factor.cov		 k x k factor excess return covariance matrix 
## sig2.e			   scalar, residual variance from factor model (residVars.vec in fitFundamentalFactorModel)
## Output:
## A list with the following components:
## sd.fm              scalar, std dev based on factor model
## mcr.fm             k+1 x 1 vector of factor marginal contributions to risk (sd)
## cr.fm              k+1 x 1 vector of factor component contributions to risk (sd)
## pcr.fm             k+1 x 1 vector of factor percent contributions to risk (sd)
## Remarks:
## The factor model has the form
## R(t) = beta'F(t) + e(t) = beta.star'F.star(t)
## where beta.star = (beta, sig.e)' and F.star(t) = (F(t)', z(t))'
## By Euler's theorem
## sd.fm = sum(cr.fm) = sum(beta*mcr.fm)
  if(is.matrix(beta.vec)) {
    beta.names = c(rownames(beta.vec), "residual")
  } else if(is.vector(beta.vec)) {
    beta.names = c(names(beta.vec), "residual")
  } else {
   stop("beta.vec is not a matrix or a vector")
  }  
  beta.vec = as.vector(beta.vec)
	beta.star.vec = c(beta.vec, sqrt(sig2.e))
	names(beta.star.vec) = beta.names
	factor.cov = as.matrix(factor.cov)
	k.star = length(beta.star.vec)
	k = k.star - 1
	factor.star.cov = diag(k.star)
	factor.star.cov[1:k, 1:k] = factor.cov
	
## compute factor model sd
  sd.fm = as.numeric(sqrt(t(beta.star.vec) %*% factor.star.cov %*% beta.star.vec))
## compute marginal and component contributions to sd
	mcr.fm = (factor.star.cov %*% beta.star.vec)/sd.fm
	cr.fm = mcr.fm * beta.star.vec
	pcr.fm = cr.fm/sd.fm
	rownames(mcr.fm) <- rownames(cr.fm) <- rownames(pcr.fm) <- beta.names
	colnames(mcr.fm) = "MCR"
	colnames(cr.fm) = "CR"
	colnames(pcr.fm) = "PCR"
## return results
	ans = list(sd.fm = sd.fm,
             mcr.fm = t(mcr.fm),
             cr.fm = t(cr.fm),
             pcr.fm = t(pcr.fm))
	return(ans)
}

