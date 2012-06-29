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

