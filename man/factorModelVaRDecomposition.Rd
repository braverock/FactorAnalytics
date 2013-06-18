\name{factorModelVaRDecomposition}
\alias{factorModelVaRDecomposition}
\title{Compute factor model factor VaR decomposition}
\usage{
  factorModelVaRDecomposition(bootData, beta.vec, sig2.e,
    tail.prob = 0.01,
    VaR.method = c("HS", "CornishFisher"))
}
\arguments{
  \item{bootData}{B x (k+2) matrix of bootstrap data. First
  column contains the fund returns, second through k+1
  columns contain factor returns, (k+2)nd column contain
  residuals scaled to have unit variance .}

  \item{beta.vec}{k x 1 vector of factor betas.}

  \item{sig2.e}{scalar, residual variance from factor
  model.}

  \item{tail.prob}{scalar, tail probability}

  \item{VaR.method}{character, method for computing VaR.
  Valid choices are "HS" for historical simulation
  (empirical quantile); "CornishFisher" for modified VaR
  based on Cornish-Fisher quantile estimate. Cornish-Fisher
  computation is done with the VaR.CornishFisher in the
  PerformanceAnalytics package.}
}
\value{
  an S3 object containing
}
\description{
  Compute factor model factor VaR decomposition based on
  Euler's theorem given historic or simulated data and
  factor model parameters. The partial derivative of VaR
  wrt factor beta is computed as the expected factor return
  given fund return is equal to its VaR and approximated by
  kernel estimator. VaR is compute either as the sample
  quantile or as an estimated quantile using the
  Cornish-Fisher expansion.
}
\details{
  The factor model has the form R(t) = beta'F(t) + e(t) =
  beta.star'F.star(t) where beta.star = (beta, sig.e)' and
  F.star(t) = (F(t)', z(t))' By Euler's theorem VaR.fm =
  sum(cVaR.fm) = sum(beta.star*mVaR.fm)
}
\examples{
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
# fit the factor model with OLS
fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                 variable.selection="all subsets",
                                 factor.set = 3)

residualData=as.matrix(fit$residVars.vec,1,6)
bootData <- factorModelMonteCarlo(n.boot=100, factors ,fit$beta.mat, residual.dist="normal",
                                  residualData, Alpha.mat=NULL, boot.method="random",
                                  seed = 123, return.factors = "TRUE", return.residuals = "TRUE")

# compute risk factor contribution to VaR using bootstrap data
# combine fund returns, factor returns and residual returns for HAM1
tmpData = cbind(bootData$returns[,1], bootData$factors,
               bootData$residuals[,1]/sqrt(fit$residVars.vec[1]))
colnames(tmpData)[c(1,5)] = c("HAM1", "residual")
factor.VaR.decomp.HAM1 <- factorModelVaRDecomposition(tmpData, fit$beta.mat[1,],
                                                      fit$residVars.vec[1], tail.prob=0.05,VaR.method="HS")
}
\author{
  Eric Zivot and Yi-An Chen
}
\references{
  1. Hallerback (2003), "Decomposing Portfolio
  Value-at-Risk: A General Analysis", The Journal of Risk
  5/2. 2. Yamai and Yoshiba (2002). "Comparative Analyses
  of Expected Shortfall and Value-at-Risk: Their Estimation
  Error, Decomposition, and Optimization Bank of Japan. 3.
  Meucci (2007). "Risk Contributions from Generic
  User-Defined Factors," Risk. 4. Epperlein and Smillie
  (2006) "Cracking VAR with Kernels," Risk.
}

