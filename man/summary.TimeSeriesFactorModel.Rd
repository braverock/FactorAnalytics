\name{summary.TimeSeriesFactorModel}
\alias{summary.TimeSeriesFactorModel}
\title{summary method for TimeSeriesModel object.}
\usage{
  \method{summary}{TimeSeriesFactorModel} (object,
    digits = 3, ...)
}
\arguments{
  \item{object}{An object created by
  \code{fitTimeSeiresFactorModel}.}

  \item{digits}{Integer indicates the number of decimal
  places. Default is 3.}

  \item{...}{Other option used in \code{print} method.}
}
\description{
  Generic function of summary method for
  \code{fitTimeSeriesFactorModel}.
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

