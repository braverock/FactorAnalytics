\name{modifiedEsReport}
\alias{modifiedEsReport}
\title{compute ES report via Cornish-Fisher expansion for collection of assets in a
portfolio given simulated (bootstrapped) return data.}
\usage{
  modifiedEsReport(bootData, w, delta.w = 0.001,
    tail.prob = 0.01, method = c("derivative", "average"),
    nav, nav.p, fundStrategy, i1, i2)
}
\arguments{
  \item{bootData}{B x n matrix of B bootstrap returns on
  assets in portfolio.}

  \item{w}{n x 1 vector of portfolio weights.}

  \item{delta.w}{scalar, change in portfolio weight for
  computing numerical derivative. Default value is 0.010.}

  \item{tail.prob}{scalar tail probability.}

  \item{method}{character, method for computing marginal
  ES. Valid choices are "derivative" for numerical
  computation of the derivative of portfolio ES wrt fund
  portfolio weight; "average" for approximating E[Ri |
  Rp<=VaR]}

  \item{nav}{n x 1 vector of net asset values in each
  fund.}

  \item{nav.p}{scalar, net asset value of portfolio
  percentage.}

  \item{fundStrategy}{n x 1 vector of fund strategies.}

  \item{i1,i2}{if ff object is used, the ffapply functions
  do apply an EXPRession and provide two indices FROM="i1"
  and TO="i2", which mark beginning and end of the batch
  and can be used in the applied expression.}
}
\value{
  dataframe with the following columns: Strategy n x 1
  strategy. Net.Asset.value n x 1 net asset values.
  Allocation n x 1 vector of asset weights. Mean n x 1 mean
  of each funds. Std.Dev n x 1 standard deviation of each
  funds.  Assets.ES n x 1 vector of asset specific ES
  values. cES n x 1 vector of asset specific component ES
  values. cES.dollar n x 1 vector of asset specific
  component ES values in dollar terms. pcES n x 1 vector of
  asset specific percent contribution to ES values. iES n x
  1 vector of asset specific incremental ES values.
  iES.dollar n x 1 vector of asset specific component ES
  values in dollar terms. mES n x 1 vector of asset
  specific marginal ES values. mES.dollar n x 1 vector of
  asset specific marginal ES values in dollar terms.
}
\description{
  compute ES report via Cornish-Fisher expansion for
  collection of assets in a portfolio given simulated
  (bootstrapped) return data. Report format follows that of
  Excel VaR report.
}
\examples{
data(managers.df)
ret.assets = managers.df[,(1:6)]
modifiedEsReport (bootData= ret.assets[,1:3], w=c(1/3,1/3,1/3), delta.w = 0.001, tail.prob = 0.01,
                 method="derivative",nav=c(100,200,100), nav.p=500, fundStrategy=c("S1","S2","S3"))
}
\author{
  Eric Zivot and Yi-An Chen.
}

