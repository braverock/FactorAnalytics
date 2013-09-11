\name{modifiedPortfolioVaRDecomposition}
\alias{modifiedPortfolioVaRDecomposition}
\title{Compute portfolio VaR decomposition given historical or simulated data and
portfolio weights.}
\usage{
  modifiedPortfolioVaRDecomposition(bootData, w,
    delta.w = 0.001, tail.prob = 0.01,
    method = c("derivative", "average"))
}
\arguments{
  \item{bootData}{B x N matrix of B bootstrap returns on
  assets in portfolio.}

  \item{w}{N x 1 vector of portfolio weights}

  \item{delta.w}{Scalar, change in portfolio weight for
  computing numerical derivative.}

  \item{tail.prob}{Scalar, tail probability.}

  \item{method}{Character, method for computing marginal
  ES. Valid choices are "derivative" for numerical
  computation of the derivative of portfolio ES with
  respect to fund portfolio weight; "average" for
  approximating E[R_i | R_p =VaR].}
}
\value{
  an S3 list containing
}
\description{
  Compute portfolio VaR decomposition given historical or
  simulated data and portfolio weights. The partial
  derivative of VaR wrt factor beta is computed as the
  expected factor return given fund return is equal to its
  VaR and approximated by kernel estimator. VaR is compute
  as an estimated quantile using the Cornish-Fisher
  expansion.
}
\examples{
data(managers.df)
ret.assets = managers.df[,(1:6)]
modifiedPortfolioVaRDecomposition(ret.assets[,1:3], w=c(1/3,1/3,1/3), delta.w = 0.001,
                                  tail.prob = 0.01, method=c("average"))
}
\author{
  Eric Zivot and Yi-An Chen.
}
\references{
  1. Hallerback (2003), "Decomposing Portfolio
  Value-at-Risk: A General Analysis", The Journal of Risk
  5/2. 2. Yamai and Yoshiba (2002). "Comparative Analyses
  of Expected Shortfall and Value-at-Risk: Their Estimation
  Error, Decomposition, and Optimization Bank of Japan. 3.
  Epperlein and Smillie (2006) "Cracking VAR with Kernels,"
  Risk.
}

