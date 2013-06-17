\name{plot.FundamentalFactorModel}
\alias{plot.FundamentalFactorModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot FundamentalFactorModel object.
}
\description{
Generic function of plot method for fitFundamentalFactorModel. 
}
\usage{
\method{plot}{FundamentalFactorModel}(fund.fit,which.plot=c("none","1L","2L","3L","4L"),max.show=12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fund.fit}{
fit object created by fitFundamentalFactorModel.
}
\item{which.plot}{
integer indicating which plot to create: "none" will create a menu to choose. Defualt is none.
1 = "factor returns",
2 =   "R square",
3 = "Variance of Residuals",
4 = "FM Correlation",
}
\item{max.show}{
Maximum assets to plot. Default is 12. 
}

}

\author{
Eric Zivot and Yi-An Chen.
}

\examples{
\dontrun{
# BARRA type factor model
# there are 447 assets  
data(stock)
assets = unique(fulldata[,"PERMNO"])
timedates = as.Date(unique(fulldata[,"DATE"]))
exposures <- exposures.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
fund.fit <- fitFundamentalFactorModel(fulldata=fulldata, timedates=timedates, exposures=exposures,covariance="classic", assets=assets,full.resid.cov=TRUE,
                                      regression="classic",wls=TRUE)

plot(fund.fit)
}
}