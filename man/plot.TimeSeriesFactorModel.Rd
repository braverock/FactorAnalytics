\name{plot.TimeSeriesFactorModel}
\alias{plot.TimeSeriesFactorModel}
\title{plot TimeSeriesfactorModel object.}
\usage{
  plot.TimeSeriesFactorModel(fit.macro, colorset = c(1:12),
    legend.loc = NULL,
    which.plot = c("none", "1L", "2L", "3L", "4L", "5L", "6L", "7L"),
    max.show = 6, plot.single = FALSE, fundName,
    which.plot.single = c("none", "1L", "2L", "3L", "4L", "5L", "6L", "7L", "8L", "9L", "10L", "11L", "12L", "13L"))
}
\arguments{
  \item{fit.macro}{fit object created by
  fitTimeSeriesFactorModel.}

  \item{colorset}{Defualt colorset is c(1:12).}

  \item{legend.loc}{plot legend or not. Defualt is
  \code{NULL}.}

  \item{which.plot}{integer indicating which plot to
  create: "none" will create a menu to choose. Defualt is
  none. 1 = "Fitted factor returns", 2 = "R square", 3 =
  "Variance of Residuals", 4 = "FM Correlation", 5 =
  "Factor Contributions to SD", 6 = "Factor Contributions
  to ES", 7 = "Factor Contributions to VaR"}

  \item{max.show}{Maximum assets to plot. Default is 6.}

  \item{plot.single}{Plot a single asset of lm class.
  Defualt is FALSE.}

  \item{fundName}{Name of the asset to be plotted.}

  \item{which.plot.single}{integer indicating which plot to
  create: "none" will create a menu to choose. Defualt is
  none. 1 = time series plot of actual and fitted values 2
  = time series plot of residuals with standard error bands
  3 = time series plot of squared residuals 4 = time series
  plot of absolute residuals 5 = SACF and PACF of residuals
  6 = SACF and PACF of squared residuals 7 = SACF and PACF
  of absolute residuals 8 = histogram of residuals with
  normal curve overlayed 9 = normal qq-plot of residuals
  10= CUSUM plot of recursive residuals 11= CUSUM plot of
  OLS residuals 12= CUSUM plot of recursive estimates
  relative to full sample estimates 13= rolling estimates
  over 24 month window}
}
\description{
  Generic function of plot method for
  fitTimeSeriesFactorModel. Either plot all fit models or
  choose a single asset to plot.
}
\examples{
\dontrun{
# load data from the database
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
# fit the factor model with OLS
fit <- fitTimeSeriesFactorModel(ret.assets,factors,fit.method="OLS",
                                 variable.selection="all subsets")
# plot of all assets and show only first 4 assets.
plot(fit.macro,max.show=4)
# single plot of HAM1 asset
plot(fit.macro, plot.single=TRUE, fundName="HAM1")
}
}
\author{
  Eric Zivot and Yi-An Chen.
}

