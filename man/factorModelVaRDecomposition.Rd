\name{factorModelVaRDecomposition}
\alias{factorModelVaRDecomposition}
\title{Compute factor model factor VaR decomposition}
\usage{
  factorModelVaRDecomposition(Data, beta.vec, sig2.e,
    tail.prob = 0.01,
    VaR.method = c("modified", "gaussian", "historical", "kernel"))
}
\arguments{
  \item{Data}{B x (k+2) matrix of bootstrap data. First
  column contains the fund returns, second through k+1
  columns contain factor returns, (k+2)nd column contain
  residuals scaled to have unit variance .}

  \item{beta.vec}{k x 1 vector of factor betas.}

  \item{sig2.e}{scalar, residual variance from factor
  model.}

  \item{tail.prob}{scalar, tail probability}

  \item{VaR.method}{character, method for computing VaR.
  Valid choices are one of
  "modified","gaussian","historical", "kernel". computation
  is done with the \code{VaR} in the PerformanceAnalytics
  package.}
}
\value{
  an S3 object containing \itemize{ \item{VaR.fm} Scalar,
  bootstrap VaR value for fund reported as a positive
  number. \item{n.exceed} Scalar, number of observations
  beyond VaR. \item{idx.exceed} n.exceed x 1 vector giving
  index values of exceedences. \item{mVaR.fm} (K+1) x 1
  vector of factor marginal contributions to VaR.
  \item{cVaR.fm} (K+1) x 1 vector of factor component
  contributions to VaR. \item{pcVaR.fm} (K+1) x 1 vector of
  factor percent contributions to VaR. }
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
fit.macro <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                                     factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                                     data=managers.df,fit.method="OLS")
# risk factor contribution to VaR
# combine fund returns, factor returns and residual returns for HAM1
tmpData = cbind(managers.df[,1],managers.df[,c("EDHEC.LS.EQ","SP500.TR")] ,
residuals(fit.macro$asset.fit$HAM1)/sqrt(fit.macro$resid.variance[1]))
colnames(tmpData)[c(1,4)] = c("HAM1", "residual")
factor.VaR.decomp.HAM1 = factorModelVaRDecomposition(tmpData, fit.macro$beta[1,],
                                                  fit.macro$resid.variance[1], tail.prob=0.05,
                                                  VaR.method="historical")
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

