## factorModelPortfolioRiskDecomposition.r
##
## purpose: Compute factor model sd (risk) decomposition for portfolio
## author: Eric Zivot
## created: January 21, 2009
## revised: January 28, 2009
## references:
## Qian, Hua and Sorensen (2007) Quantitative Equity Portfolio Management, 
## chapter 3.

factorModelPortfolioRiskDecomposition <- function(w.vec, beta.mat, factor.cov, sig2.e) {
## Inputs:
## w.vec         n x 1 vector of portfolio weights
## beta.mat		   n x k matrix of factor betas
## factor.cov		 k x k factor excess return covariance matrix
## sig2.e			   n x 1 vector of residual variances from factor model
## Output:
## cov.fm		     n x n excess return covariance matrxi based on
##				       estimated factor model
## Output:
## A list with the following components:
## var.p              scalar, portfolio variance based on factor model
## var.p.systematic   scalar, portfolio variance due to factors
## var.p.specific     scalar, portfolio variance not explanied by factors
## var.p.cov          scalar, portfolio variance due to covariance terms
## var.p.factors      k x 1 vector, portfolio variances due to factors
## mcr.p              n x 1 vector, marginal contributions to portfolio total risk
## mcr.p.systematic   n x 1 vector, marginal contributions to portfolio systematic risk
## mcr.p.specific     n x 1 vector, marginal contributions to portfolio specific risk
## pcr.p              n x 1 vector, percent contribution to portfolio total risk
	beta.mat = as.matrix(beta.mat)
	factor.cov = as.matrix(factor.cov)
	sig2.e = as.vector(sig2.e)
	if (length(sig2.e) > 1) {
	 D.e = diag(as.vector(sig2.e))
	} else {
   D.e = as.matrix(sig2.e)
  } 
	if (ncol(beta.mat) != ncol(factor.cov))
		stop("beta.mat and factor.cov must have same number of columns")
	if (nrow(D.e) != nrow(beta.mat))
		stop("beta.mat and D.e must have same number of rows")
 ## compute factor model covariance matrix
  cov.systematic = beta.mat %*% factor.cov %*% t(beta.mat)
	cov.fm = cov.systematic + D.e
	if (any(diag(chol(cov.fm)) == 0))
    warning("Covariance matrix is not positive definite")
 ## compute portfolio level variance
 var.p = as.numeric(t(w.vec) %*% cov.fm %*% w.vec)
 var.p.systematic = as.numeric(t(w.vec) %*% cov.systematic %*% w.vec)
 var.p.specific = as.numeric(t(w.vec) %*% D.e %*% w.vec)
 beta.p = crossprod(w.vec, beta.mat)
 var.p.factors = beta.p^2 * diag(factor.cov)
 var.p.cov = var.p.systematic - sum(var.p.factors)
 
 
 ## compute marginal contributions to risk
 mcr.p = (cov.systematic %*% w.vec + D.e %*% w.vec)/sqrt(var.p)
 mcr.p.systematic = (cov.systematic %*% w.vec + D.e %*% w.vec)/sqrt(var.p.systematic)
 mcr.p.specific = (D.e %*% w.vec)/sqrt(var.p.specific)
 ## compute percentage risk contribution
 pcr.p = (w.vec * mcr.p)/sqrt(var.p)
 ## return results
 ans = list(var.p=var.p,
            var.p.systematic=var.p.systematic,
            var.p.specific=var.p.specific,
            var.p.cov=var.p.cov,
            var.p.factors=var.p.factors,
            mcr.p=mcr.p,
            mcr.p.systematic=mcr.p.systematic,
            mcr.p.specific=mcr.p.specific,
            pcr.p=pcr.p)
 return(ans)
}