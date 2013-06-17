\name{plot.FM.attribution}
\alias{plot.FM.attribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot FM.attribution class
}
\description{
Generic function of plot method for factorModelPerformanceAttribution. Either plot all fit models or choose a single asset to plot. 
}
\usage{
\method{plot}{FM.attribution}(fm.attr, which.plot=c("none","1L","2L","3L"),max.show=6,
                                date,plot.single=FALSE,fundName,
                                which.plot.single=c("none","1L","2L","3L"),...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fm.attr}{
FM.attribution object created by factorModelPerformanceAttribution.
}
\item{which.plot}{
integer indicating which plot to create: "none" will create a menu to choose. Defualt is none.
1 = attributed cumulative returns,
2 = attributed returns on date selected by user,
3 = time series of attributed returns
}
\item{max.show}{
Maximum assets to plot. Default is 6. 
}
\item{date}{
date indicates for attributed returns, the date format should be the same as data.
}
\item{plot.single}{
Plot a single asset of lm class. Defualt is FALSE.
}
\item{fundName}{
Name of the portfolio to be plotted. 
}             
\item{which.plot.single}{
integer indicating which plot to create:
"none" will create a menu to choose. Defualt is none.
1 = attributed cumulative returns,
2 = attributed returns on date selected by user,
3 = time series of attributed returns
}
\item{...}{
more arguements for \code{chart.TimeSeries} used for plotting time series
}     
}

\author{
Yi-An Chen.
}

\examples{
\dontrun{
# load data from the database
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
# fit the factor model with OLS
fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
                                 variable.selection="all subsets")
fm.attr <- factorModelPerformanceAttribution(fit.macro)
# group plot
plot(fm.attr,date="2006-12-30")
# single portfolio plot
plot(fm.attr,date="2006-12-30")
}
}