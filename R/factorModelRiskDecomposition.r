## factorModelRiskDecomposition.r
## 
## purpose: Compute factor model risk decomposition for individual fund
## author: Eric Zivot
## created: January 21, 2009
## revised: January 28, 2009

factorModelRiskDecomposition <- function(beta.vec, factor.cov, sig2.e) {
## Inputs:
## beta   		   k x 1 vector of factor betas
## factor.cov		 k x k factor excess return covariance matrix
## sig2.e			   scalar, residual variance from factor model
## Output:
## cov.fm		     n x n excess return covariance matrix based on
##				       estimated factor model
## Output:
## A list with the following components:
## var.fm             scalar, variance based on factor model
## var.systematic     scalar, variance due to factors
#  var.specific       scalar, residual variance
## var.cov            scalar, variance due to covariance contribution
## var.factors        k x 1 vector of variances due to factors
## Remarks:
## var.fm = var.systematic + var.specific = sum(var.factors) + var.cov + var.specific

	beta.vec = as.vector(beta.vec)
	factor.cov = as.matrix(factor.cov)
	
## compute factor model variance
  	var.systematic = t(beta.vec) %*% factor.cov %*% beta.vec
	var.fm = var.systematic + sig2.e

## compute factor model variance contributions
	var.factors = beta.vec^2 * diag(factor.cov)
  	var.cov = var.systematic - sum(var.factors)

## return results
	ans = list(var.fm=as.numeric(var.fm),
            var.systematic=as.numeric(var.systematic),
            var.specific=sig2.e,
            var.cov=as.numeric(var.cov),
            var.factors=var.factors)
	return(ans)
}
