\name{summary.FundamentalFactorModel}
\alias{summary.FundamentalFactorModel}
\title{summary FundamentalFactorModel object}
\usage{
  summary.FundamentalFactorModel(fit.fund,
    digits = max(3, .Options$digits - 3), ...)
}
\arguments{
  \item{fit.fund}{fit object created by
  fitFundamentalFactorModel.}

  \item{digits}{integer indicating the number of decimal
  places. Default is 3.}

  \item{...}{Other arguments for print methods.}
}
\description{
  Generic function of summary method for
  fitFundamentalFactorModel.
}
\examples{
data(stock)
# there are 447 assets
exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP")
test.fit <- fitFundamentalFactorModel(data=data,exposure.names=exposure.names,
                                       datevar = "DATE", returnsvar = "RETURN",
                                       assetvar = "TICKER", wls = TRUE,
                                       regression = "classic",
                                       covariance = "classic", full.resid.cov = TRUE,
                                       robust.scale = TRUE)

summary(test.fit)
}
\author{
  Yi-An Chen.
}

