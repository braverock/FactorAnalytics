#' Compute Factor Model Covariance Matrix.
#' 
#' Compute asset return covariance matrix from factor model parameters.
#' 
#' The return on asset \code{i} (\code{i = 1,...,N}) is assumed to follow the
#' factor model \cr \code{R(i,t) = alpha + t(beta)*F(t) + e(i,t), e(i,t) ~ iid
#' (0, sig(i)^2)} \cr where \code{beta} is a \code{K x 1} vector of factor
#' exposures. The return variance is then \cr \code{var(R(i,t) =
#' t(beta)*var(F(t))*beta + sig(i)^2}, \cr and the \code{N x N} covariance
#' matrix of the return vector \code{R} is \cr \code{var(R) = B*var(F(t))*t(B)
#' + D} \cr where B is the \code{N x K} matrix of asset betas and \code{D} is a
#' diagonal matrix with \code{sig(i)^2} values along the diagonal.
#' 
#' @param beta.mat \code{N x K} matrix of factor betas, where \code{N} is the
#' number of assets and \code{K} is the number of factors.
#' @param factor.cov \code{K x K} factor return covariance matrix.
#' @param residVars.vec \code{N x 1} vector of asset specific residual
#' variances from the factor model.
#' @return \code{N x N} return covariance matrix based on factor model
#' parameters.
#' @author Eric Zivot and Yi-An Chen.
#' @references Zivot, E. and J. Wang (2006), \emph{Modeling Financial Time
#' Series with S-PLUS, Second Edition}, Springer-Verlag.
#' @examples
#' 
#' # factorModelCovariance
#' data(managers.df)
#' factors    = managers.df[,(7:9)]
#' ret.assets = managers.df[,(1:6)]
#' fit <-fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
#'                                   variable.selection="all subsets", factor.set = 3)
#' factorModelCovariance(fit$beta.mat,var(factors),fit$residVars.vec)
#' 
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

