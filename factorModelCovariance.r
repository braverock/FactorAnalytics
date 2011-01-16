## factorModelCovariance.r
##
## purpose: compute covariance matrix from estimated factor model
## author: Eric Zivot
## created: November 25, 2008
## revised: July 20, 2009

factorModelCovariance <- function(beta.mat, factor.cov, sig.e) {
## Inputs:
## beta.mat		   	n x k matrix of factor betas
## factor.cov		  k x k factor excess return covariance matrix
## sig.e			    n x 1 vector of residual variances from factor model
## Output:
## cov.fm			    n x n excess return covariance matrix based on
##				        estimated factor model
	beta.mat = as.matrix(beta.mat)
	factor.cov = as.matrix(factor.cov)
	sig.e = as.vector(sig.e)
	if (length(sig.e) > 1) {
	 D.e = diag(as.vector(sig.e))
  } else {
   D.e =  as.matrix(sig.e)
  }
	if (ncol(beta.mat) != ncol(factor.cov))
		stop("beta.mat and factor.cov must have same number of columns")
		
	if (nrow(D.e) != nrow(beta.mat))
		stop("beta.mat and D.e must have same number of rows")
	cov.fm = beta.mat %*% factor.cov %*% t(beta.mat) + D.e
	if (any(diag(chol(cov.fm)) == 0))
    warning("Covariance matrix is not positive definite")
	return(cov.fm)
}
