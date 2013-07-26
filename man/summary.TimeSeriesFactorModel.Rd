\name{summary.TimeSeriesFactorModel}
\alias{summary.TimeSeriesFactorModel}
\title{summary method for TimeSeriesModel object.}
\usage{
  summary.TimeSeriesFactorModel(fit.macro, digits = 3)
}
\arguments{
  \item{fit.macro}{fit.macro object created by
  fitTimeSeiresFactorModel.}

  \item{digits}{Integer indicating the number of decimal
  places. Default is 3.}

  \item{...}{other option used in \code{summary.lm}}
}
\description{
  Generic function of summary method for
  fitTimeSeriesFactorModel.
}
\examples{
# load data from the database
data(managers.df)
# fit the factor model with OLS
fit <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                               factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                               data=managers.df,fit.method="OLS")
summary(fit)
}
\author{
  Yi-An Chen.
}

