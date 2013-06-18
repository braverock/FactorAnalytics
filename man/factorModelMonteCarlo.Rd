\name{factorModelMonteCarlo}
\alias{factorModelMonteCarlo}
\title{Simulate returns using factor model Monte Carlo method.}
\usage{
  factorModelMonteCarlo(n.boot = 1000, factorData,
    Beta.mat, Alpha.mat = NULL, residualData,
    residual.dist = c("normal", "Cornish-Fisher", "skew-t"),
    boot.method = c("random", "block"), seed = 123,
    return.factors = FALSE, return.residuals = FALSE)
}
\arguments{
  \item{n.boot}{Integer number of bootstrap samples.}

  \item{factorData}{\code{n.months x n.funds} matrix or
  data.frame of factor returns.}

  \item{Beta.mat}{\code{n.funds x n.factors} matrix of
  factor betas.}

  \item{Alpha.mat}{\code{n.funds x 1} matrix of factor
  alphas (intercepts). If \code{NULL} then assume that all
  alphas are zero.}

  \item{residualData}{\code{n.funds x n.parms} matrix of
  residual distribution parameters. The columns of
  \code{residualData} depend on the value of
  \code{residual.dist}. If \code{residual.dist = "normal"},
  then \code{residualData} has one column containing
  variance values; if \code{residual.dist =
  "Cornish-Fisher"}, then \code{residualData} has three
  columns containing variance, skewness and excess kurtosis
  values; if \code{residual.dist="skew-t"}, then
  \code{residualData} has four columns containing location,
  scale, shape, and df values.}

  \item{residual.dist}{character vector specifying the
  residual distribution. Choices are "normal" for the
  normal distribution; "Cornish-Fisher" for the
  Cornish-Fisher distribution based on the Cornish-Fisher
  expansion of the normal distribution quantile; "skew-t"
  for the skewed Student's t distribution of Azzalini and
  Captiano.}

  \item{boot.method}{character vector specifying the
  resampling method. Choices are "random" for random
  sampling with replacement (non-parametric bootstrap);
  "block" for stationary block bootstrapping.}

  \item{seed}{integer random number seed used for
  resampling the factor returns.}

  \item{return.factors}{logical; if \code{TRUE} then return
  resampled factors in output list object.}

  \item{return.residuals}{logical; if \code{TRUE} then
  return simulated residuals in output list object.}
}
\value{
  A list with the following components:
}
\description{
  Simulate returns using factor model Monte Carlo method.
  Parametric method like normal distribution,
  Cornish-Fisher and skew-t distribution for residuals can
  be selected. Resampling method like non-parametric
  bootstrap or stationary bootstrap can be selected.
}
\details{
  The factor model Monte Carlo method is described in Jiang
  (2009).
}
\examples{
# load data from the database
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
# fit the factor model with OLS
fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                 variable.selection="all subsets",factor.set=3)
factorData=factors
Beta.mat=fit$beta.mat
residualData=as.matrix(fit$residVars.vec,1,6)
n.boot=1000
# bootstrap returns data from factor model with residuals sample from normal distribution
bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="normal",
                                  residualData, Alpha.mat=NULL, boot.method="random",
                                  seed = 123, return.factors = "TRUE", return.residuals =
                                  "TRUE")
# Cornish-Fisher distribution
# build different residualData matrix
residualData <- cbind(c(1,2,1,3,0.1,0.5),rnorm(6),c(2,3,1,2,1,0))
colnames(residualData) <- c("var","skew","ekurt")
rownames(residualData) <- colnames(managers.df[,(1:6)])
bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="Cornish-Fisher",
                                  residualData, Alpha.mat=NULL, boot.method="random",
                                  seed = 123, return.factors = "TRUE", return.residuals =
                                  "TRUE")


# skew-t distribution
# build residualData matrix
residualData <- cbind(rnorm(6),c(1,2,1,3,0.1,0.5),rnorm(6),c(2,3,1,6,10,100))
colnames(residualData) <- c("location","scale","shape","df")
rownames(residualData) <- colnames(managers.df[,(1:6)])
bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="skew-t",
                                  residualData, Alpha.mat=NULL, boot.method="random",
                                  seed = 123, return.factors = "TRUE", return.residuals =
                                  "TRUE")
}
\author{
  Eric Zivot and Yi-An Chen.
}
\references{
  Jiang, Y. (2009). UW PhD Thesis.
}

