factorModelCovariance <-
function(beta.mat, factor.cov, residVars.vec) {
## Inputs:
## beta.mat		   	n x k matrix of factor betas
## factor.cov		  k x k factor return covariance matrix
## residVars.vec  n x 1 vector of residual variances from factor model
## Output:
## cov.fm			    n x n return covariance matrix based on
##				        estimated factor model.
	beta.mat = as.matrix(beta.mat)
	factor.cov = as.matrix(factor.cov)
	sig.e = as.vector(residVars.vec)
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

