\name{predict.TimeSeriesFactorModel}
\alias{predict.TimeSeriesFactorModel}
\title{predict method for TimeSeriesModel object.}
\usage{
  \method{predict}{TimeSeriesFactorModel} (object,
    newdata = NULL, ...)
}
\arguments{
  \item{object}{A fit object created by
  fitTimeSeiresFactorModel.}

  \item{newdata}{A vector, matrix, data.frame, xts,
  timeSeries or zoo object to be coerced.}

  \item{...}{Any other arguments used in \code{predict.lm},
  such as \code{newdata} and \code{fit.se}.}
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
fit <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                               factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                               data=managers.df,fit.method="OLS")

pred.fit <- predict(fit)
newdata <- data.frame(EDHEC.LS.EQ = rnorm(n=120), SP500.TR = rnorm(n=120) )
rownames(newdata) <- rownames(fit$data)
pred.fit2 <- predict(fit,newdata,interval="confidence")
}
\author{
  Yi-An Chen.
}

