\name{factorModelPerformanceAttribution}
\alias{factorModelPerformanceAttribution}
\title{Compute performance attribution}
\usage{
  factorModelPerformanceAttribution(fit, ...)
}
\arguments{
  \item{fit}{Class of "TimeSeriesFactorModel",
  "FundamentalFactorModel" or "statFactorModel".}

  \item{...}{Other controled variables for fit methods.}
}
\value{
  an object of class \code{FM.attribution} containing
  \itemize{ \item{cum.ret.attr.f} N X J matrix of
  cumulative return attributed to factors.
  \item{cum.spec.ret} 1 x N vector of cumulative specific
  returns.  \item{attr.list} list of time series of
  attributed returns for every portfolio. }
}
\description{
  Decompose total returns or active returns into returns
  attributed to factors and specific returns. Class of
  FM.attribution is generated and generic function
  \code{plot()} and \code{summary()},\code{print()} can be
  used.
}
\details{
  total returns can be decomposed into returns attributed
  to factors and specific returns. \eqn{R_t = \sum_j b_{j}
  * f_{jt} + u_t},t=1..T,\eqn{b_{j}} is exposure to factor
  j and \eqn{f_{jt}} is factor j. The returns attributed to
  factor j is \eqn{b_{j} * f_{jt}} and specific returns is
  \eqn{u_t}.
}
\examples{
data(managers.df)
fit.ts <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                                     factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                                      data=managers.df,fit.method="OLS")
# withoud benchmark
fm.attr <- factorModelPerformanceAttribution(fit.ts)
}
\author{
  Yi-An Chen.
}
\references{
  Grinold,R and Kahn R, \emph{Active Portfolio Management},
  McGraw-Hill.
}

