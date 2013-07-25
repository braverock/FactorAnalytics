\name{print.TimeSeriesFactorModel}
\alias{print.TimeSeriesFactorModel}
\title{print TimeSeriesfactorModel object}
\usage{
  print.TimeSeriesFactorModel(fit.macro,
    digits = max(3, .Options$digits - 3), ...)
}
\arguments{
  \item{fit.macro}{fit object created by
  fitTimeSeriesFactorModel.}

  \item{digits.}{integer indicating the number of decimal
  places. Default is 3.}

  \item{...}{arguments to be passed to print method.}
}
\description{
  Generic function of print method for
  fitTimeSeriesFactorModel.
}
\examples{
# load data from the database
data(managers.df)
fit.macro <- fitTimeseriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                                      factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                                      data=managers.df,fit.method="OLS")
print(fit.macro)
}
\author{
  Eric Zivot and Yi-An Chen.
}

