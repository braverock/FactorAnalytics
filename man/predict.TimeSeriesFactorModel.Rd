\name{predict.TimeSeriesFactorModel}
\alias{predict.TimeSeriesFactorModel}
\title{predict method for TimeSeriesModel object.}
\usage{
  predict.TimeSeriesFactorModel(fit.macro, ...)
}
\arguments{
  \item{fit}{"TimeSeriesFactorModel" object created by
  fitTimeSeiresFactorModel.}

  \item{...}{Any other arguments used in \code{predict.lm}.
  for example newdata and se.fit.}
}
\description{
  Generic function of predict method for
  fitTimeSeriesFactorModel. It utilizes function
  \code{predict.lm}.
}
\examples{
# load data from the database
data(managers.df)
ret.assets = managers.df[,(1:6)]
# fit the factor model with OLS
fit <- fitTimeseriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                               factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                               data=managers.df,fit.method="OLS")

predict(fit)
predict(fit,newdata,interval="confidence")
}
\author{
  Yi-An Chen.
}

