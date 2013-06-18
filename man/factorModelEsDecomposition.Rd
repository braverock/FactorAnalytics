\name{factorModelEsDecomposition}
\alias{factorModelEsDecomposition}
\title{Compute Factor Model Factor ES Decomposition}
\usage{
  factorModelEsDecomposition(Data, beta.vec, sig2.e,
    tail.prob = 0.05)
}
\arguments{
  \item{Data}{\code{B x (k+2)} matrix of historic or
  simulated data. The first column contains the fund
  returns, the second through \code{k+1}st columns contain
  the returns on the \code{k} factors, and the
  \code{(k+2)}nd column contain residuals scaled to have
  unit variance.}

  \item{beta.vec}{\code{k x 1} vector of factor betas.}

  \item{sig2.e}{scalar, residual variance from factor
  model.}

  \item{tail.prob}{scalar, tail probability for VaR
  quantile. Typically 0.01 or 0.05.}
}
\value{
  A list with the following components:
}
\description{
  Compute the factor model factor expected shortfall (ES)
  decomposition for an asset based on Euler's theorem given
  historic or simulated data and factor model parameters.
  The partial derivative of ES with respect to factor beta
  is computed as the expected factor return given fund
  return is less than or equal to its value-at-risk (VaR).
  VaR is compute as the sample quantile of the historic or
  simulated data.
}
\details{
  The factor model has the form \cr \code{R(t) =
  t(beta)*F(t) + e(t) = t(beta.star)*F.star(t)} \cr where
  \code{beta.star = t(beta, sig.e)} and \code{F.star(t) =
  (t(F(t)), t(z(t)))} By Euler's theorem \cr \code{ES.fm =
  sum(cES.fm) = sum(beta.star*mcES.fm)} \cr
}
\examples{
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
# fit the factor model with OLS
fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                 variable.selection="all subsets",factor.set=3)
# risk factor contribution to ETL
# combine fund returns, factor returns and residual returns for HAM1
tmpData = cbind(ret.assets[,1], factors,
               residuals(fit$asset.fit$HAM1)/sqrt(fit$residVars.vec[1]))
colnames(tmpData)[c(1,5)] = c("HAM1", "residual")
factor.es.decomp.HAM1 = factorModelEsDecomposition(tmpData, fit$beta.mat[1,],
                                           fit$residVars.vec[1], tail.prob=0.05)
}
\author{
  Eric Zviot and Yi-An Chen.
}
\references{
  1. Hallerback (2003), "Decomposing Portfolio
  Value-at-Risk: A General Analysis", \emph{The Journal of
  Risk} 5/2. \cr 2. Yamai and Yoshiba (2002). "Comparative
  Analyses of Expected Shortfall and Value-at-Risk: Their
  Estimation Error, Decomposition, and Optimization", Bank
  of Japan. \cr 3. Meucci (2007). "Risk Contributions from
  Generic User-Defined Factors," \emph{Risk}.
}

