\name{summary.FM.attribution}
\alias{summary.FM.attribution}
\title{summary FM.attribution object.}
\usage{
  \method{summary}{FM.attribution} (fm.attr)
}
\arguments{
  \item{fm.attr}{FM.attribution object created by
  factorModelPerformanceAttribution.}
}
\description{
  Generic function of summary method for
  factorModelPerformanceAttribution.
}
\examples{
# load data from the database
 data(managers.df)
 # fit the factor model with OLS
 fit.ts <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                               factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                               data=managers.df,fit.method="OLS")

  fm.attr <- factorModelPerformanceAttribution(fit.ts)
  summary(fm.attr)
}
\author{
  Yi-An Chen.
}

