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
  Decompose total returns into returns attributed to
  factors and specific returns. Class of FM.attribution is
  generated and generic function \code{plot()} and
  \code{summary()},\code{print()} can be applied.
}
\details{
  Total returns can be decomposed into returns attributed
  to factors and specific returns. \cr \eqn{R_t = \sum b_j
  * f_jt + u_t,t=1...T} \cr \code{b_j} is exposure to
  factor j and \code{f_jt} is factor j. The returns
  attributed to factor j is \code{b_j * f_jt} and specific
  returns is \code{u_t}.
}
\examples{
\dontrun{
data(managers.df)
fit.ts <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                                     factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                                      data=managers.df,fit.method="OLS")
# withoud benchmark
fm.attr <- factorModelPerformanceAttribution(fit.ts)
}
}
\author{
  Yi-An Chen.
}
\references{
  Grinold,R and Kahn R, \emph{Active Portfolio Management},
  McGraw-Hill.
}

