\name{plot.FundamentalFactorModel}
\alias{plot.FundamentalFactorModel}
\title{plot FundamentalFactorModel object.}
\usage{
  \method{plot}{FundamentalFactorModel} (x,
    which.plot = c("none", "1L", "2L", "3L", "4L", "5L", "6L"),
    max.show = 4, plot.single = FALSE, asset.name,
    which.plot.single = c("none", "1L", "2L", "3L", "4L", "5L", "6L", "7L", "8L", "9L"),
    legend.txt = TRUE, VaR.method = "historical", ...)
}
\arguments{
  \item{x}{fit object created by
  fitFundamentalFactorModel.}

  \item{which.plot}{integer indicating which plot to
  create: "none" will create a menu to choose. Defualt is
  none. \cr 1 = "Factor returns",\cr 2 = "Residual
  plots",\cr 3 = "Variance of Residuals",\cr 4 = "Factor
  Model Correlation",\cr 5 = "Factor Contributions to
  SD",\cr 6 = "Factor Contributions to ES",\cr 7 = "Factor
  Contributions to VaR"\cr}

  \item{max.show}{Maximum assets to plot. Default is 4.}

  \item{plot.single}{Plot a single asset of lm class.
  Defualt is FALSE.}

  \item{asset.name}{Name of the asset to be plotted.}

  \item{which.plot.single}{integer indicating which plot to
  create: "none" will create a menu to choose. Defualt is
  none.\cr 1 = time series plot of actual and fitted
  values,\cr 2 = time series plot of residuals with
  standard error bands,\cr 3 = time series plot of squared
  residuals,\cr 4 = time series plot of absolute
  residuals,\cr 5 = SACF and PACF of residuals,\cr 6 = SACF
  and PACF of squared residuals,\cr 7 = SACF and PACF of
  absolute residuals,\cr 8 = histogram of residuals with
  normal curve overlayed,\cr 9 = normal qq-plot of
  residuals.\cr}

  \item{legend.txt}{Logical. TRUE will plot legend on
  barplot. Defualt is \code{TRUE}.}

  \item{VaR.method}{character, method for computing VaR.
  Valid choices are one of
  "modified","gaussian","historical", "kernel". Computation
  is done with the \code{VaR} in the PerformanceAnalytics
  package. Default is "historical".}

  \item{...}{other variables for barplot method.}
}
\description{
  Generic function of plot method for
  fitFundamentalFactorModel.
}
\examples{
\dontrun{
# BARRA type factor model
data(Stock.df)
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

