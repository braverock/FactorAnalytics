\name{normalIncrementalES}
\alias{normalIncrementalES}
\title{compute normal incremental ES given portfolio weights, mean vector and
covariance matrix.}
\usage{
  normalIncrementalES(mu, Sigma, w, tail.prob = 0.01)
}
\arguments{
  \item{mu}{n x 1 vector of expected returns.}

  \item{Sigma}{n x n return covariance matrix.}

  \item{w}{n x 1 vector of portfolio weights.}

  \item{tail.prob}{scalar tail probability.}
}
\value{
  n x 1 vector of incremental ES values.
}
\description{
  compute normal incremental ES given portfolio weights,
  mean vector and covariance matrix.Incremental ES is
  defined as the change in portfolio ES that occurs when an
  asset is removed from the portfolio.
}
\examples{
data(managers.df)
ret.assets = managers.df[,(1:6)]
mu <- mean(ret.assets[,1:3])
Sigma <- var(ret.assets[,1:3])
w <- rep(1/3,3)
normalIncrementalES(mu,Sigma,w)

# given some Multinormal distribution
normalIncrementalES(mu=c(1,2),Sigma=matrix(c(1,0.5,0.5,2),2,2),w=c(0.5,0.5),tail.prob = 0.01)
}
\author{
  Eric Zivot and Yi-An Chen
}
\references{
  Jorian, P. (2007). Value at Risk, pg. 168.
}

