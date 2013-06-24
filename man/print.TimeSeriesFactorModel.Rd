\name{print.TimeSeriesFactorModel}
\alias{print.TimeSeriesFactorModel}
\title{print TimeSeriesfactorModel object}
\usage{
  print.TimeSeriesFactorModel(fit.macro)
}
\arguments{
  \item{fit.macro}{fit object created by
  fitTimeSeriesFactorModel.}
}
\description{
  Generic function of print method for
  fitTimeSeriesFactorModel.
}
\examples{
# load data from the database
data(managers.df)
ret.assets = managers.df[,(1:6)]
factors    = managers.df[,(7:9)]
# fit the factor model with OLS
fit.macro <- fitTimeSeriesFactorModel(ret.assets,factors,fit.method="OLS",
                                 variable.selection="all subsets")
print(fit.macro)
}
\author{
  Eric Zivot and Yi-An Chen.
}

