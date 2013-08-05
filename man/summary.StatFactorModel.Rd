\name{summary.StatFactorModel}
\alias{summary.StatFactorModel}
\title{summary method for StatFactorModel object.}
\usage{
  \method{summary}{StatFactorModel} (object, digits = 3)
}
\arguments{
  \item{object}{An Object created by
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
data(stat.fm.data)
# fit the factor model with OLS
fit <- fitStatisticalFactorModel(sfm.dat,k=2)
summary(fit)
}
\author{
  Yi-An Chen.
}

