\name{CornishFisher}
\alias{CornishFisher}
\alias{dCornishFisher}
\alias{pCornishFisher}
\alias{qCornishFisher}
\alias{rCornishFisher}
\title{Functions for Cornish-Fisher density, CDF, random number simulation and
quantile.}
\usage{
  dCornishFisher(x, n, skew, ekurt)

  pCornishFisher(q, n, skew, ekurt)

  qCornishFisher(p, n, skew, ekurt)

  rCornishFisher(n, sigma, skew, ekurt, seed = NULL)
}
\arguments{
  \item{n}{scalar, number of simulated values in
  rCornishFisher. Sample length in
  density,distribution,quantile function.}

  \item{sigma}{scalar, standard deviation.}

  \item{skew}{scalar, skewness.}

  \item{ekurt}{scalar, excess kurtosis.}

  \item{seed}{set seed here. Default is \code{NULL}.}

  \item{x,q}{vector of standardized quantiles. See detail.}

  \item{p}{vector of probabilities.}
}
\value{
  n simulated values from Cornish-Fisher distribution.
}
\description{
  \itemize{ \item \code{rCornishFisher} simulate
  observations based on Cornish-Fisher quantile expansion
  given mean, standard deviation, skewness and excess
  kurtosis. \item \code{dCornishFisher} Computes
  Cornish-Fisher density from two term Edgeworth expansion
  given mean, standard deviation, skewness and excess
  kurtosis. \item \code{pCornishFisher} Computes
  Cornish-Fisher CDF from two term Edgeworth expansion
  given mean, standard deviation, skewness and excess
  kurtosis. \item \code{qCornishFisher} Computes
  Cornish-Fisher quantiles from two term Edgeworth
  expansion given mean, standard deviation, skewness and
  excess kurtosis. }
}
\details{
  CDF(q) = Pr(sqrt(n)*(x_bar-mu)/sigma < q)
}
\examples{
\dontrun{
 # generate 1000 observation from Cornish-Fisher distribution
rc <- rCornishFisher(1000,1,0,5)
hist(rc,breaks=100,freq=FALSE,main="simulation of Cornish Fisher Distribution",
    xlim=c(-10,10))
lines(seq(-10,10,0.1),dnorm(seq(-10,10,0.1),mean=0,sd=1),col=2)
# compare with standard normal curve

# example from A.dasGupta p.188 exponential example
# x is iid exp(1) distribution, sample size = 5
# then x_bar is Gamma(shape=5,scale=1/5) distribution
q <- c(0,0.4,1,2)
# exact cdf
pgamma(q/sqrt(5)+1,shape=5,scale=1/5)
# use CLT
pnorm(q)
# use edgeworth expansion
pCornishFisher(q,n=5,skew=2,ekurt=6)
}
}
\author{
  Eric Zivot and Yi-An Chen.
}
\references{
  \itemize{ \item A.DasGupta, "Asymptotic Theory of
  Statistics and Probability", Springer Science+Business
  Media,LLC 2008 \item Thomas A.Severini, "Likelihood
  Methods in Statistics", Oxford University Press, 2000 }
}

