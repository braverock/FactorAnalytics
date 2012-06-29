\name{normalVaRReport}
\alias{normalVaRReport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute VaR report via nonparametric method for collection of assets in a portfolio given 
portfolio weights, mean vector and covariance matrix.
}
\description{
compute VaR report via nonparametric method for collection of assets in a portfolio given 
portfolio weights, mean vector and covariance matrix.
Report format follows that of Excel VaR report.
}
\usage{
normalVaRReport(mu, Sigma, w, tail.prob = 0.01, nav, nav.p, fundStrategy, fundNames)
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
  \item{nav}{ 
n x 1 vector of net asset values in each fund.
}
  \item{nav.p}{
scalar, net asset value of portfolio.
}
  \item{fundStrategy}{
n x 1 vector of fund strategies.
}
  \item{fundNames}{
Name of the funds in the portfolio.
}
}

\value{
dataframe with the following columns: 
Strategy         n x 1 strategy.
Net.Asset.value  n x 1 net asset values.  
Allocation       n x 1 vector of asset weights.
Mean             n x 1 mean of each funds.
Std.Dev          n x 1 standard deviation of each funds.   
Assets.VaR       n x 1 vector of asset specific VaR values.
cVaR             n x 1 vector of asset specific component VaR values.
cVaR.dollar      n x 1 vector of asset specific component VaR values in dollar terms.
pcVaR            n x 1 vector of asset specific percent contribution to VaR values.
iVaR             n x 1 vector of asset specific incremental VaR values.
iVaR.dollar      n x 1 vector of asset specific component VaR values in dollar terms.
mVaR             n x 1 vector of asset specific marginal VaR values.
mVaR.dollar      n x 1 vector of asset specific marginal VaR values in dollar terms.
}

\author{
Eric Zivot and Yi-An Chen.
}

\examples{
# from data
data(managers.df)
ret.assets = managers.df[,(1:6)]
mu <- mean(ret.assets[,1:3])
Sigma <- var(ret.assets[,1:3])
w <- rep(1/3,3)
normalVaRReport(mu,Sigma,w,nav=c(100,200,100), nav.p=500, fundStrategy=c("S1","S2","S3"),fundNames=colnames(ret.assets[,1:3]))

# given some multinormal distribution
normalVaRReport(mu=c(1,2),Sigma=matrix(c(1,0.5,0.5,3),2,2),w=c(0.5,0.5),tail.prob = 0.01,
                    nav=c(100,100), nav.p=200, fundStrategy=c("S1","S2"),fundNames=c("N1","N2"))
}
