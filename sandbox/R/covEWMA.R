#' Compute RiskMetrics-type EWMA Covariance Matrix
#' 
#' Compute time series of RiskMetrics-type EWMA covariance matrices of returns.
#' Initial covariance matrix is assumed to be the unconditional covariance
#' matrix.
#' 
#' The EWMA covariance matrix at time \code{t} is compute as \cr \code{Sigma(t)
#' = lambda*Sigma(t-1) + (1-lambda)*R(t)t(R(t))} \cr where \code{R(t)} is the
#' \code{K x 1} vector of returns at time \code{t}.
#' 
#' @param factors \code{T x K} data.frame containing asset returns, where
#' \code{T} is the number of time periods and \code{K} is the number of assets.
#' @param lambda Scalar exponential decay factor. Must lie between between 0
#' and 1.
#' @param return.cor Logical, if TRUE then return EWMA correlation matrices.
#' @return \code{T x K x K} array giving the time series of EWMA covariance
#' matrices if \code{return.cor=FALSE} and EWMA correlation matrices if
#' \code{return.cor=TRUE}.
#' @author Eric Zivot and Yi-An Chen.
#' @references Zivot, E. and J. Wang (2006), \emph{Modeling Financial Time
#' Series with S-PLUS, Second Edition}, Springer-Verlag.
#' @examples
#' 
#' # compute time vaying covariance of factors.
#' data(managers.df)
#' factors    = managers.df[,(7:9)]
#' cov.f.ewma <- covEWMA(factors)
#' cov.f.ewma[120,,]
#' 
covEWMA <-
function(factors, lambda=0.96, return.cor=FALSE) {
## Inputs:
## factors    N x K numerical factors data.  data is class data.frame
##            N is the time length and K is the number of the factors.  
## lambda     scalar. exponetial decay factor between 0 and 1. 
## return.cor Logical, if TRUE then return EWMA correlation matrices
## Output:  
## cov.f.ewma  array. dimension is N x K x K.
## comments:
## 1. add optional argument cov.start to specify initial covariance matrix
## 2. allow data input to be data class to be any rectangular data object
  

if (is.data.frame(factors)){
  factor.names  = colnames(factors)
  t.factor      = nrow(factors)
  k.factor      = ncol(factors)
  factors       = as.matrix(factors)
  t.names       = rownames(factors)
} else {
  stop("factor data should be saved in data.frame class.") 
}
if (lambda>=1 || lambda <= 0){
  stop("exponential decay value lambda should be between 0 and 1.")
} else {
  cov.f.ewma = array(,c(t.factor,k.factor,k.factor))
  cov.f = var(factors)  # unconditional variance as EWMA at time = 0 
  FF = (factors[1,]- mean(factors)) %*% t(factors[1,]- mean(factors))
  cov.f.ewma[1,,] = (1-lambda)*FF  + lambda*cov.f
  for (i in 2:t.factor) {
    FF = (factors[i,]- mean(factors)) %*% t(factors[i,]- mean(factors))
    cov.f.ewma[i,,] = (1-lambda)*FF  + lambda*cov.f.ewma[(i-1),,]
  }
    
}
  # 9/15/11: add dimnames to array
  dimnames(cov.f.ewma) = list(t.names, factor.names, factor.names)
  
  if(return.cor) {
   cor.f.ewma = cov.f.ewma
   for (i in 1:dim(cor.f.ewma)[1]) {
    cor.f.ewma[i, , ] = cov2cor(cov.f.ewma[i, ,])
   }
   return(cor.f.ewma)
  } else{
      return(cov.f.ewma)  
  }
}

