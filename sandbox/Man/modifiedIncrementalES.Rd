\name{modifiedIncrementalES}
\alias{modifiedIncrementalES}
\title{Compute incremental ES given bootstrap data and portfolio weights.}
\usage{
  modifiedIncrementalES(bootData, w, tail.prob = 0.01, i1,
    i2)
}
\arguments{
  \item{bootData}{B x N matrix of B bootstrap returns on n
  assets in portfolio.}

  \item{w}{N x 1 vector of portfolio weights}

  \item{tail.prob}{scalar tail probability.}

  \item{i1,i2}{if ff object is used, the ffapply functions
  do apply an EXPRession and provide two indices FROM="i1"
  and TO="i2", which mark beginning and end of the batch
  and can be used in the applied expression.}
}
\value{
  n x 1 matrix of incremental ES values for each asset.
}
\description{
  Compute incremental ES given bootstrap data and portfolio
  weights. Incremental ES is defined as the change in
  portfolio ES that occurs when an asset is removed from
  the portfolio and allocation is spread equally among
  remaining assets. VaR used in ES computation is computed
  as an estimated quantile using the Cornish-Fisher
  expansion.
}
\examples{
data(managers.df)
ret.assets = managers.df[,(1:6)]
modifiedIncrementalES(ret.assets[,1:3],w=c(1/3,1/3,1/3),tail.prob = 0.05)
}
\author{
  Eric Zivot and Yi-An Chen.
}
\references{
  Jorian, P. (2007). Value-at-Risk, pg. 168.
}

