\name{plot.FundamentalFactorModel}
\alias{plot.FundamentalFactorModel}
\title{plot FundamentalFactorModel object.}
\usage{
  plot.FundamentalFactorModel(fit.fund,
    which.plot = c("none", "1L", "2L", "3L", "4L"),
    max.show = 10)
}
\arguments{
  \item{fit.fund}{fit object created by
  fitFundamentalFactorModel.}

  \item{which.plot}{integer indicating which plot to
  create: "none" will create a menu to choose. Defualt is
  none. 1 = "factor returns", 2 = "R square", 3 = "Variance
  of Residuals", 4 = "FM Correlation",}

  \item{max.show}{Maximum assets to plot. Default is 12.}
}
\description{
  Generic function of plot method for
  fitFundamentalFactorModel.
}
\examples{
\dontrun{
# BARRA type factor model
# there are 447 assets
data(stock)
# BARRA type factor model
data(stock)
# there are 447 assets
exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP")
fit.fund <- fitFundamentalFactorModel(data=data,exposure.names=exposure.names,
                                       datevar = "DATE", returnsvar = "RETURN",
                                       assetvar = "TICKER", wls = TRUE,
                                       regression = "classic",
                                       covariance = "classic", full.resid.cov = TRUE,
                                       robust.scale = TRUE)

plot(fit.fund)
}
}
\author{
  Eric Zivot and Yi-An Chen.
}

