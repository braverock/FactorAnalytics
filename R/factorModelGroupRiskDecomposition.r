## factorModelGroupRiskDecomposition.r
## 
## purpose: Compute factor model risk decomposition for individual fund by risk groups
##          Risk groups are equity, rates, credit, fx, commondity, strategy
##
## author: Eric Zivot
## created: July 9, 2009
## revised: July 9, 2009

factorModelGroupRiskDecomposition <- function(beta.vec, factor.cov, sig2.e,
                                              equityIds, ratesIds, creditIds, 
                                              fxIds, cmdtyIds, strategyIds) {
## Inputs:
## beta   		   k x 1 vector of factor betas
## factor.cov		 k x k factor excess return covariance matrix
## sig2.e			   scalar, residual variance from factor model
## equityIds     k1 x 1 vector of equity factor Ids
## ratesIds      k2 x 1 vector of rates factor Ids
## creditIds     k3 x 1 vector of credit factor Ids
## fxIds         k4 x 1 vector of fx factor Ids
## cmdtyIds      k5 x 1 vector of commodity factor Ids
## strategyIds   k6 x 1 vector of strategy (blind) factor Ids
##
## Output:
## A list with the following components:
## var.fm             scalar, variance based on factor model
## var.systematic     scalar, variance contribution due to factors
## var.specific       scalar, residual variance contribution
## var.cov            scalar, variance contribution due to covariances between factor groups
## var.equity         scalar, variance contribution due to equity factors
## var.rates          scalar, variance contribution due to rates factors
## var.credit         scalar, variance contribution due to credit factors
## var.fx             scalar, variance contribution due to fx factors
## var.cmdty          scalar, variance contribution due to commodity factors
## var.strategy       scalar, variance contribution due to strategy (pca) factors
## Remarks:
## k1 + ... + k6 = k
## var.fm = var.systematic + var.specific = sum(var.factors) + var.cov + var.specific

	beta.vec = as.matrix(beta.vec)
	n.beta = length(beta.vec)
	n.factors = length(c(equityIds, ratesIds, creditIds, fxIds, cmdtyIds, strategyIds))
	if (n.beta != n.factors)
	 stop("Number of supplied factor Ids is not equal to number of betas")
	factor.cov = as.matrix(factor.cov)
	
## compute factor model variance
  var.systematic = t(beta.vec) %*% factor.cov %*% beta.vec
	var.fm = var.systematic + sig2.e

## compute factor model variance contributions
	var.equity = t(beta.vec[equityIds,]) %*% factor.cov[equityIds,equityIds] %*% beta.vec[equityIds,]
	var.rates = t(beta.vec[ratesIds,]) %*% factor.cov[ratesIds,ratesIds] %*% beta.vec[ratesIds,]
	var.credit = t(beta.vec[creditIds,]) %*% factor.cov[creditIds,creditIds] %*% beta.vec[creditIds,]
	var.fx = t(beta.vec[fxIds,]) %*% factor.cov[fxIds,fxIds] %*%  beta.vec[fxIds,]
	var.cmdty = t(beta.vec[cmdtyIds,]) %*% factor.cov[cmdtyIds,cmdtyIds] %*% beta.vec[cmdtyIds,]
	if (!is.null(strategyIds)) {
    var.strategy = t(beta.vec[strategyIds,]) %*% factor.cov[strategyIds,strategyIds] %*% beta.vec[strategyIds,]
	} else {
   var.strategy = 0
  } 
	 
# compute covariance contribution	 
  var.cov = var.systematic - (var.equity + var.rates + var.credit + var.fx + var.cmdty + var.strategy)

## return results
	ans = list(var.fm=as.numeric(var.fm),
            var.systematic=as.numeric(var.systematic),
            var.specific=sig2.e,
            var.cov=as.numeric(var.cov),
            var.equity=as.numeric(var.equity),
            var.rates=as.numeric(var.rates),
            var.credit=as.numeric(var.credit),
            var.fx=as.numeric(var.fx),
            var.cmdty=as.numeric(var.cmdty),
            var.strategy=var.strategy)
	return(ans)
}
