\name{summary.StatFactorModel}
\alias{summary.StatFactorModel}
\title{summary method for StatFactorModel object.}
\usage{
  summary.StatFactorModel(fit.stat, digits = 3)
}
\arguments{
  \item{fit.stat}{fit.stat object created by
  fitStatisticalFactorModel.}

  \item{digits}{Integer indicating the number of decimal
  places. Default is 3.}

  \item{...}{other option used in \code{summary.lm}}
}
\description{
  Generic function of summary method for
  fitStatisticalFactorModel.
}
\examples{
# load data from the database
data(managers.df)
# fit the factor model with OLS
fit <- fitStatisticalFactorModel(fitStatisticalFactorModel(sfm.dat,k=2,
                                  ckeckData.method="data.frame"))
summary(fit)
}
\author{
  Yi-An Chen.
}

