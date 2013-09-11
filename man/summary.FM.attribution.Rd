\name{summary.FM.attribution}
\alias{summary.FM.attribution}
\title{summary FM.attribution object.}
\usage{
  \method{summary}{FM.attribution} (fm.attr,
    digits = max(3, .Options$digits - 3), ...)
}
\arguments{
  \item{fm.attr}{FM.attribution object created by
  \code{factorModelPerformanceAttribution}.}

  \item{digits}{integer indicating the number of decimal
  places. Default is 3.}

  \item{...}{Other arguments for \code{print} methods.}
}
\description{
  Generic function of summary method for
  \code{factorModelPerformanceAttribution}.
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

