\name{normalPortfolioEsDecomposition}
\alias{normalPortfolioEsDecomposition}
\title{Compute portfolio ES decomposition by assets given normality assumption.}
\usage{
  normalPortfolioEsDecomposition(mu, Sigma, w,
    tail.prob = 0.01)
}
\arguments{
  \item{mu}{n x 1 vector of expected returns.}

  \item{Sigma}{n x n return covariance matrix.}

  \item{w}{n x 1 vector of portfolio weights.}

  \item{tail.prob}{scalar tail probability.}
}
\value{
  an S3 list containing
}
\description{
  compute normal portfolio ES, marginal contribution to
  portfolio ES, component contribution to portfolio ES and
  percentage component contribution to portfolio ES given
  portfolio weights, mean vector and covariance matrix.
}
\examples{
# from data
data(managers.df)
ret.assets = managers.df[,(1:6)]
mu <- mean(ret.assets[,1:3])
Sigma <- var(ret.assets[,1:3])
w <- rep(1/3,3)
normalPortfolioEsDecomposition(mu,Sigma,w)

# given some multinormal distribution
# compute 2 asset returns ES decomposition given multinormal distribution assumption.
normalPortfolioEsDecomposition(mu=c(1,2),Sigma=diag(2),w=c(0.5,0.5),tail.prob = 0.01)
}
\author{
  Eric Zivot and Yi-An Chen
}

