\name{print.FundamentalFactorModel}
\alias{print.FundamentalFactorModel}
\title{print FundamentalFactorModel object}
\usage{
  \method{print}{FundamentalFactorModel} (x,
    digits = max(3, .Options$digits - 3), ...)
}
\arguments{
  \item{x}{fit object created by
  fitFundamentalFactorModel.}

  \item{digits}{integer indicating the number of decimal
  places. Default is 3.}

  \item{...}{Other arguments for print methods.}
}
\description{
  Generic function of print method for
  fitFundamentalFactorModel.
}
\examples{
data(Stock.df)
# there are 447 assets
exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP")
test.fit <- fitFundamentalFactorModel(data=stock,exposure.names=exposure.names,
                                       datevar = "DATE", returnsvar = "RETURN",
                                       assetvar = "TICKER", wls = TRUE,
                                       regression = "classic",
                                       covariance = "classic", full.resid.cov = TRUE,
                                       robust.scale = TRUE)

print(test.fit)
}
\author{
  Yi-An Chen.
}

