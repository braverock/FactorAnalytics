factorModelMonteCarlo <-
function(n.boot=1000, factorData, Beta.mat, Alpha.mat=NULL,
                                  residualData, residual.dist = c("normal", "Cornish-Fisher", "skew-t"),
                                  boot.method = c("random", "block"),
                                  seed=123, return.factors= FALSE , return.residuals= FALSE ) {
## inputs:
## n.boot         number of bootstrap samples
## factorData     n.months x n.funds matrix or data.frame of factor returns
## Beta.mat       n.funds x n.factors matrix of factor betas
## Alpha.mat      n.funds x 1 matrix of factor alphas (intercepts). If NULL then
##                assume that all alphas are zero.
## residualData   n.funds x n.parms matrix of residual distribution parameters. The
##                columns of residualData depend on the value of residual.dist. If
##                residual.dist = "normal", then residualData has one column vector
##                containing variance values; if residual.dist = "Cornish-Fisher",
##                then residualData has three columns containing variance,
##                skewness and excess kurtosis values; if residual.dist="skew-t",
##                then residualData has four columns containing location, scale,
##                shape and df values.
## residual.dist  character vector specifying the residual distribution. Choices are
##                "normal" for the normal distribution; "Cornish-Fisher" for the
##                Cornish-Fisher distribution based on the Cornish-Fisher expansion
##                of the normal distribution quantile; "skew-t" for the skewed Student's
##                t distribution of Azzalini and Captiano.
## boot.method    character vector specifying the resampling method. Choices are
##                "random" for random sampling with replacement (non-parametric bootstrap);
##                "block" for stationary block bootstrapping.
## seed           integer random number seed.
## return.factors logical; if TRUE then return resampled factors
## return.residuals logical; if TRUE then return simulated residuals
##
## output: A list with the following components:
## returns        n.boot x n.funds matrix of simulated fund returns
## factors        n.boot x n.factors matrix of resampled factor returns. Returned 
##                only if return.factors = TRUE.
## residuals      n.boot x n.funds matrix of simulated fund residuals. Returned only
##                if return.residuals = TRUE.
 require(tseries) # for function tsbootstrap()
 require(sn) # for function rst()
 boot.method = boot.method[1]
 residual.dist = residual.dist[1]
 set.seed(seed)
 if (nrow(Beta.mat) != nrow(residualData)) {
  stop("Beta.mat and residualData have different number of rows")
 }
 factorData = as.matrix(factorData)
 n.funds = nrow(Beta.mat)
 fund.names = rownames(Beta.mat)
 if (is.null(Alpha.mat)) {
  Alpha.mat = matrix(0, nrow(Beta.mat))
  rownames(Alpha.mat) = fund.names
 }
##
## reseample from empirical distribution of factors
## 
 if (boot.method == "random") {
  bootIdx = sample(nrow(factorData),  n.boot, replace=TRUE)
 } else {
  n.samples = round(n.boot/nrow(factorData))
  n.adj = n.boot - n.samples*nrow(factorData)
  bootIdx = as.vector(tsbootstrap(1:nrow(factorData), nb=n.samples))
  if (n.adj > 0) {
## need to make sure that length(bootIdx) = n.boot
   bootIdx = c(bootIdx, bootIdx[1:n.adj])
  }
 }
 factorDataBoot = factorData[bootIdx, ]
##
## run factor model Monte Carlo loop over funds
##
  fundReturnsBoot = matrix(0, n.boot, n.funds)
  residualsSim = matrix(0, n.boot, n.funds)
  colnames(fundReturnsBoot) = colnames(residualsSim) = fund.names
  for (i in fund.names) {
  ## set random number seed for fund specific residual simulations
    set.seed(which(fund.names == i))
  ## simulate from residual distributions
    if (residual.dist == "normal") {
      residualsSim[, i] = rnorm(n.boot, sd=sqrt(residualData[i,]))
    } else if (residual.dist == "Cornish-Fisher") {
    ## residual distribution is CornishFisher
      residualsSim[, i] = rCornishFisher(n.boot,
                                         sigma=sqrt(residualData[i,"var"]),
                                         skew=residualData[i,"skew"],
                                         ekurt=residualData[i,"ekurt"])
    } else if (residual.dist == "skew-t") {
    ## residual distribution is CornishFisher
      residualsSim[, i] = rst(n.boot,
                              location=residualData[i, "location"],
                              scale=residualData[i,"scale"],
                              shape=residualData[i,"shape"],
                              df=residualData[i,"df"])
    } else {
     stop("Invalid residual distribution")
    }
    ## simulated fund returns
    fundReturnsBoot[, i] = Alpha.mat[i,1] + factorDataBoot[, colnames(Beta.mat)] %*% t(Beta.mat[i, ,drop=FALSE]) + residualsSim[, i]
  } # end loop over funds

  ans = list(returns=fundReturnsBoot)
  if (return.factors) {
   ans$factors=factorDataBoot
  }
  if (return.residuals) {
   ans$residuals=residualsSim
  }
  return(ans)
}

