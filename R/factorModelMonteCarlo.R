#' Simulate returns using factor model Monte Carlo method.
#' 
#' Simulate returns using factor model Monte Carlo method. Parametric method
#' like normal distribution, Cornish-Fisher and skew-t distribution for
#' residuals can be selected. Resampling method like non-parametric bootstrap
#' or stationary bootstrap can be selected.
#' 
#' The factor model Monte Carlo method is described in Jiang (2009).
#' 
#' @param n.boot Integer number of bootstrap samples.
#' @param factorData \code{n.months x n.funds} matrix or data.frame of factor
#' returns.
#' @param Beta.mat \code{n.funds x n.factors} matrix of factor betas.
#' @param Alpha.mat \code{n.funds x 1} matrix of factor alphas (intercepts). If
#' \code{NULL} then assume that all alphas are zero.
#' @param residualData \code{n.funds x n.parms} matrix of residual distribution
#' parameters. The columns of \code{residualData} depend on the value of
#' \code{residual.dist}. If \code{residual.dist = "normal"}, then
#' \code{residualData} has one column containing variance values; if
#' \code{residual.dist = "Cornish-Fisher"}, then \code{residualData} has three
#' columns containing variance, skewness and excess kurtosis values; if
#' \code{residual.dist="skew-t"}, then \code{residualData} has four columns
#' containing location, scale, shape, and df values.
#' @param residual.dist character vector specifying the residual distribution.
#' Choices are "normal" for the normal distribution; "Cornish-Fisher" for the
#' Cornish-Fisher distribution based on the Cornish-Fisher expansion of the
#' normal distribution quantile; "skew-t" for the skewed Student's t
#' distribution of Azzalini and Captiano.
#' @param boot.method character vector specifying the resampling method.
#' Choices are "random" for random sampling with replacement (non-parametric
#' bootstrap); "block" for stationary block bootstrapping.
#' @param seed integer random number seed used for resampling the factor
#' returns.
#' @param return.factors logical; if \code{TRUE} then return resampled factors
#' in output list object.
#' @param return.residuals logical; if \code{TRUE} then return simulated
#' residuals in output list object.
#' @return A list with the following components:
#' @returnItem returns \code{n.boot x n.funds} matrix of simulated fund
#' returns.
#' @returnItem factors \code{n.boot x n.factors} matrix of resampled factor
#' returns. Returned only if \code{return.factors = TRUE}.
#' @returnItem residuals \code{n.boot x n.funds} matrix of simulated fund
#' residuals. Returned only if \code{return.residuals = TRUE}.
#' @author Eric Zivot and Yi-An Chen.
#' @references Jiang, Y. (2009). UW PhD Thesis.
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' fit <- fitTimeseriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                 factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                 data=managers.df,fit.method="OLS")
#' factorData=factors  
#' Beta.mat=fit$beta.mat
#' residualData=as.matrix(fit$residVars.vec,1,6) 
#' n.boot=1000
#' # bootstrap returns data from factor model with residuals sample from normal distribution
#' bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="normal",
#'                                   residualData, Alpha.mat=NULL, boot.method="random",
#'                                   seed = 123, return.factors = "TRUE", return.residuals = 
#'                                   "TRUE")
#' # Cornish-Fisher distribution
#' # build different residualData matrix
#' residualData <- cbind(c(1,2,1,3,0.1,0.5),rnorm(6),c(2,3,1,2,1,0))
#' colnames(residualData) <- c("var","skew","ekurt")
#' rownames(residualData) <- colnames(managers.df[,(1:6)])
#' bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="Cornish-Fisher",
#'                                   residualData, Alpha.mat=NULL, boot.method="random",
#'                                   seed = 123, return.factors = "TRUE", return.residuals =
#'                                   "TRUE")
#' 
#' 
#' # skew-t distribution
#' # build residualData matrix
#' residualData <- cbind(rnorm(6),c(1,2,1,3,0.1,0.5),rnorm(6),c(2,3,1,6,10,100))
#' colnames(residualData) <- c("location","scale","shape","df")
#' rownames(residualData) <- colnames(managers.df[,(1:6)])
#' bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="skew-t",
#'                                   residualData, Alpha.mat=NULL, boot.method="random",
#'                                   seed = 123, return.factors = "TRUE", return.residuals =
#'                                   "TRUE")
#' 
factorModelMonteCarlo <-
function(n.boot=1000, factorData, Beta.mat, Alpha.mat=NULL,
                                  residualData, residual.dist = c("normal", "Cornish-Fisher", "skew-t"),
                                  boot.method = c("random", "block"),
                                  seed=123, return.factors= FALSE , return.residuals= FALSE ) {

  
 require(tseries) # for function tsbootstrap()
 require(sn) # for function rst()
 require(PerformanceAnalytics)
 
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

