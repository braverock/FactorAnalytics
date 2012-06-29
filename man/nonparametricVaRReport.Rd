\name{nonparametricVaRReport}
\alias{nonparametricVaRReport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute VaR report via sample quantitle for collection of assets in a portfolio given
simulated (bootstrapped) return data.
}
\description{
compute VaR report via sample quantile for collection of assets in a portfolio given
simulated (bootstrapped) return data.
Report format follows that of Excel VaR report.
}
\usage{
nonparametricVaRReport(bootData, w, delta.w = 0.001, tail.prob = 0.01, 
                        method = c("derivative","average"), nav, nav.p, 
                        fundStrategy, i1,i2) 
} 
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootData}{
B x n matrix of B bootstrap returns on assets in portfolio.
}
  \item{w}{
n x 1 vector of portfolio weights.
}
  \item{delta.w}{
scalar, change in portfolio weight for computing numerical derivative.
Default value is 0.010.
}
  \item{tail.prob}{
scalar tail probability.
}
  \item{method}{
character, method for computing marginal VaR Valid choices are 
"derivative" for numerical computation of the derivative of portfolio
VaR wrt fund portfolio weight; "average" for approximating E[Ri | Rp =VaR]
}
  \item{nav}{ 
n x 1 vector of net asset values in each fund.
}
  \item{nav.p}{
scalar, net asset value of portfolio percentage.
}
  \item{fundStrategy}{
n x 1 vector of fund strategies.
}
 \item{i1,i2}{
if ff object is used,  the ffapply functions do apply an EXPRession and 
provide two indices FROM="i1" and TO="i2", which mark beginning and end 
of the batch and can be used in the applied expression.
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
data(managers.df)
ret.assets = managers.df[,(1:6)]
nonparametricVaRReport (bootData= ret.assets[,1:3], w=c(1/3,1/3,1/3), delta.w = 0.001, 
                        tail.prob = 0.01, 
                        method="derivative",nav=c(100,200,100), 
                        nav.p=500, fundStrategy=c("S1","S2","S3"))
}
