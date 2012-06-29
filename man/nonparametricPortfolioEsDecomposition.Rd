\name{nonparametricPortfolioEsDecomposition}
\alias{nonparametricPortfolioEsDecomposition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute portfolio ES (risk) decomposition by assets.
}
\description{
Compute portfolio ES decomposition given historical or simulated data and portfolio weights.
Marginal ES is computed either as the numerical derivative of ES with respect to portfolio weight or
as the expected fund return given portfolio return is less than or equal to portfolio VaR
VaR is compute as the sample quantile. 
}
\usage{
nonparametricPortfolioEsDecomposition(bootData, w, delta.w = 0.001, tail.prob = 0.01, method = c("derivative", "average"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootData}{
B x N matrix of B bootstrap returns on assets in portfolio.
}
  \item{w}{
N x 1 vector of portfolio weights
}
  \item{delta.w}{
Scalar, change in portfolio weight for computing numerical derivative.
}
  \item{tail.prob}{
Scalar, tail probability.
}
  \item{method}{
Character, method for computing marginal ES. Valid choices are
"derivative" for numerical computation of the derivative of portfolio
ES with respect to fund portfolio weight; "average" for approximating E[R_i | R_p<=VaR].
}
}

\value{
\item{VaR.p}{
Scalar, portfolio VaR reported as a positive number.
}       
\item{ES.p}{
Scalar, portfolio ES reported as a positive number.
}        
\item{n.exceed}{
Scalar, number of VaR exceedences.
}   
\item{idx.exceed}{
n.exceed x 1 vector of exceedence indices.
} 
\item{mES}{
1 x n matrix of marginal ES values for each fund.
}        
\item{cES}{
1 x n matrix of component ES values.
}        
\item{pcES}{
1 x n matrix of percent contributions to portfolio ES values.
}       
}
\references{
1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
    The Journal of Risk 5/2.
2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and
    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
    Bank of Japan.
}
\author{
Eric Zivot and Yi-An Chen.
}
\note{
This functionality is provided by the ES() function in PerformanceAnalytics.
}

\examples{
# load data from the database
data(managers.df)
ret.assets = managers.df[,(1:6)]
nonparametricPortfolioEsDecomposition(ret.assets[,1:3], w=c(1/3,1/3,1/3), 
                                      delta.w = 0.001, tail.prob = 0.01, 
                                      method=c("derivative"))
}

