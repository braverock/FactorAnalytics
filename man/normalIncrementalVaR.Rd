\name{normalIncrementalVaR}
\alias{normalIncrementalVaR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute normal incremental VaR given portfolio weights, mean vector and
covariance matrix.
}
\description{
compute normal incremental VaR given portfolio weights, mean vector and
covariance matrix. Incremental VaR is defined as the change in portfolio VaR that occurs
when an asset is removed from the portfolio.
}
\usage{
normalIncrementalVaR(mu, Sigma, w, tail.prob = 0.01)
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
n x 1 vector of incremental VaR values
}
\references{
Jorian (2007) pg. 168
}
\author{
Eric Zivot and Yi-An Chen
}

\examples{
normalIncrementalVaR(mu=c(1,2),Sigma=matrix(c(1,0.5,0.5,2),2,2),w=c(0.5,0.5),tail.prob = 0.01)
}
