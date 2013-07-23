\name{plot.FundamentalFactorModel}
\alias{plot.FundamentalFactorModel}
\title{plot FundamentalFactorModel object.}
\usage{
  plot.FundamentalFactorModel(fit.fund,
    which.plot = c("none", "1L", "2L", "3L", "4L"),
    max.show = 4, plot.single = FALSE, asset.name,
    which.plot.single = c("none", "1L", "2L", "3L", "4L", "5L", "6L", "7L", "8L", "9L"),
    legend.txt = TRUE, ...)
}
\arguments{
  \item{fit.fund}{fit object created by
  fitFundamentalFactorModel.}

  \item{which.plot}{integer indicating which plot to
  create: "none" will create a menu to choose. Defualt is
  none. 1 = "Factor returns", 2 = "Residual plots", 3 =
  "Variance of Residuals", 4 = "Factor Model Correlation",
  5 = "Factor Contributions to SD", 6 = "Factor
  Contributions to ES", 7 = "Factor Contributions to VaR"}

  \item{max.show}{Maximum assets to plot. Default is 4. #'}

  \item{plot.single}{Plot a single asset of lm class.
  Defualt is FALSE.}

  \item{asset.name}{Name of the asset to be plotted.}

  \item{which.plot.single}{integer indicating which plot to
  create: "none" will create a menu to choose. Defualt is
  none. 1 = time series plot of actual and fitted values, 2
  = time series plot of residuals with standard error
  bands, 3 = time series plot of squared residuals, 4 =
  time series plot of absolute residuals, 5 = SACF and PACF
  of residuals, 6 = SACF and PACF of squared residuals, 7 =
  SACF and PACF of absolute residuals, 8 = histogram of
  residuals with normal curve overlayed, 9 = normal qq-plot
  of residuals.}

  \item{legend}{Logical. TRUE will plot legend on barplot.
  Defualt is \code{TRUE}.}

  \item{...}{other variables for barplot method.}
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

