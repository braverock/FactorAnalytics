\name{plot.FM.attribution}
\alias{plot.FM.attribution}
\title{plot FM.attribution class}
\usage{
  \method{plot}{FM.attribution} (fm.attr,
    which.plot = c("none", "1L", "2L", "3L"), max.show = 6,
    date = NULL, plot.single = FALSE, fundName,
    which.plot.single = c("none", "1L", "2L", "3L"), ...)
}
\arguments{
  \item{fm.attr}{FM.attribution object created by
  factorModelPerformanceAttribution.}

  \item{which.plot}{integer indicating which plot to
  create: "none" will create a menu to choose. Defualt is
  none. 1 = attributed cumulative returns, 2 = attributed
  returns on date selected by user, 3 = time series of
  attributed returns}

  \item{max.show}{Maximum assets to plot. Default is 6.}

  \item{date}{date indicates for attributed returns, the
  date format should be xts compatible.}

  \item{plot.single}{Plot a single asset of lm class.
  Defualt is FALSE.}

  \item{fundName}{Name of the portfolio to be plotted.}

  \item{which.plot.single}{integer indicating which plot to
  create: "none" will create a menu to choose. Defualt is
  none. 1 = attributed cumulative returns, 2 = attributed
  returns on date selected by user, 3 = time series of
  attributed returns}

  \item{...}{more arguements for \code{chart.TimeSeries}
  used for plotting time series}
}
\description{
  Generic function of plot method for
  factorModelPerformanceAttribution. Either plot all fit
  models or choose a single asset to plot.
}
\examples{
\dontrun{
data(managers.df)
fit.ts <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                                  factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                                  data=managers.df,fit.method="OLS")
 fm.attr <- factorModelPerformanceAttribution(fit.ts)
# plot all
plot(fm.attr,legend.loc="topleft",max.show=6)
dev.off()
# plot only one assets "HAM1
plot(fm.attr,plot.single=TRUE,fundName="HAM1")
}
}
\author{
  Yi-An Chen.
}

