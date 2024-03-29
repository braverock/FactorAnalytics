% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.pafm.r
\name{plot.pafm}
\alias{plot.pafm}
\title{plot \code{"pafm"} object}
\usage{
\method{plot}{pafm}(
  x,
  which.plot = c("none", "1L", "2L", "3L"),
  max.show = 6,
  date = NULL,
  plot.single = FALSE,
  fundName,
  which.plot.single = c("none", "1L", "2L", "3L"),
  ...
)
}
\arguments{
\item{x}{object of class \code{"pafm"} created by
\code{paFm}.}

\item{which.plot}{Integer indicates which plot to create: "none" will
create a menu to choose. Defualt is none.\cr
1 = attributed cumulative returns,\cr
2 = attributed returns on date selected by user,\cr
3 = time series of attributed returns}

\item{max.show}{Maximum assets to plot. Default is 6.}

\item{date}{Indicates for attributed returns, the date format should be
xts compatible.}

\item{plot.single}{Plot a single asset of lm class. Defualt is FALSE.}

\item{fundName}{Name of the portfolio to be plotted.}

\item{which.plot.single}{Integer indicates which plot to create: "none"
will create a menu to choose. Defualt is none.\cr
 1 = attributed cumulative returns,\cr
 2 = attributed returns on date selected by user, \cr
 3 = time series of attributed returns}

\item{...}{more arguements for \code{chart.TimeSeries} used for plotting
time series}
}
\description{
Generic function of plot method for paFm.
Either plot all assets or choose a single asset to plot.
}
\examples{
\dontrun{
data(managers, package = 'PerformanceAnalytics')
fit <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
               factor.names=c("EDHEC LS EQ","SP500 TR"), data=managers)
fm.attr <- paFm(fit)
# plot all
plot(fm.attr, legend.loc="topleft", max.show=6)
dev.off()
# plot only one assets "HAM1
plot(fm.attr, plot.single=TRUE, fundName="HAM1")
}

}
\author{
Yi-An Chen.
}
