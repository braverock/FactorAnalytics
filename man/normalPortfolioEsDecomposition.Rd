\name{normalPortfolioEsDecomposition}
\alias{normalPortfolioEsDecomposition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute portfolio ES decomposition by assets given normality assumption.
}
\description{
compute normal portfolio ES, marginal contribution to portfolio ES, component 
contribution to portfolio ES and percentage component contribution to portfolio 
ES given portfolio weights, mean vector and covariance matrix. 
}
\usage{
normalPortfolioEsDecomposition(mu, Sigma, w, tail.prob = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu}{
n x 1 vector of expected returns.
}
  \item{Sigma}{
n x n return covariance matrix.
}
  \item{w}{
n x 1 vector of portfolio weights.
}
  \item{tail.prob}{
scalar tail probability.
}
}

\value{
\item{ES.p}{
Scalar, portfolio ES reported as a positive number.
}        
\item{mES}{
1 x n matrix of marginal contribution to portfolio ES values for each fund.
}        
\item{cES}{
1 x n matrix of component contribution to portfolio ES values.
}        
\item{pcES}{
1 x n matrix of percent component contributions to portfolio ES values.
}
}

\author{
Eric Zivot and Yi-An Chen
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
