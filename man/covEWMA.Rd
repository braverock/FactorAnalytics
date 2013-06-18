\name{covEWMA}
\alias{covEWMA}
\title{Compute RiskMetrics-type EWMA Covariance Matrix}
\usage{
  covEWMA(factors, lambda = 0.96, return.cor = FALSE)
}
\arguments{
  \item{factors}{\code{T x K} data.frame containing asset
  returns, where \code{T} is the number of time periods and
  \code{K} is the number of assets.}

  \item{lambda}{Scalar exponential decay factor. Must lie
  between between 0 and 1.}

  \item{return.cor}{Logical, if TRUE then return EWMA
  correlation matrices.}
}
\value{
  \code{T x K x K} array giving the time series of EWMA
  covariance matrices if \code{return.cor=FALSE} and EWMA
  correlation matrices if \code{return.cor=TRUE}.
}
\description{
  Compute time series of RiskMetrics-type EWMA covariance
  matrices of returns. Initial covariance matrix is assumed
  to be the unconditional covariance matrix.
}
\details{
  The EWMA covariance matrix at time \code{t} is compute as
  \cr \code{Sigma(t) = lambda*Sigma(t-1) +
  (1-lambda)*R(t)t(R(t))} \cr where \code{R(t)} is the
  \code{K x 1} vector of returns at time \code{t}.
}
\examples{
# compute time vaying covariance of factors.
data(managers.df)
factors    = managers.df[,(7:9)]
cov.f.ewma <- covEWMA(factors)
cov.f.ewma[120,,]
}
\author{
  Eric Zivot and Yi-An Chen.
}
\references{
  Zivot, E. and J. Wang (2006), \emph{Modeling Financial
  Time Series with S-PLUS, Second Edition},
  Springer-Verlag.
}

