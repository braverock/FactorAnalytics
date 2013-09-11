\name{impliedFactorReturns}
\alias{impliedFactorReturns}
\title{Compute Implied Factor Returns Using Covariance Matrix Approach}
\usage{
  impliedFactorReturns(factor.scenarios, mu.factors,
    cov.factors)
}
\arguments{
  \item{factor.scenarios}{m x 1 vector of scenario values
  for a subset of the n > m risk factors}

  \item{mu.factors}{\code{n x 1} vector of factor mean
  returns.}

  \item{cov.factors}{\code{n x n} factor covariance
  matrix.}
}
\value{
  \code{(n - m) x 1} vector of implied factor returns
}
\description{
  Compute risk factor conditional mean returns for a one
  group of risk factors given specified returns for another
  group of risk factors based on the assumption that all
  risk factor returns are multivariately normally
  distributed.
}
\details{
  Let \code{y} denote the \code{m x 1} vector of factor
  scenarios and \code{x} denote the \code{(n-m) x 1} vector
  of other factors. Assume that \code{(y', x')'} has a
  multivariate normal distribution with mean \code{(mu.y',
  mu.x')'} and covariance matrix partitioned as
  \code{(cov.yy, cov.yx, cov.xy, cov.xx)}. Then the implied
  factor scenarios are computed as \code{E[x|y] = mu.x +
  cov.xy*cov.xx^-1 * (y - mu.y)}
}
\examples{
# get data
data(managers.df)
factors    = managers.df[,(7:9)]
# make up a factor mean returns scenario for factor SP500.TR
factor.scenarios <- 0.1
names(factor.scenarios) <- "SP500.TR"
mu.factors <- mean(factors)
cov.factors <- var(factors)
# implied factor returns
impliedFactorReturns(factor.scenarios,mu.factors,cov.factors)
}
\author{
  Eric Zivot and Yi-An Chen.
}

