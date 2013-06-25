\name{predict.StatFactorModel}
\alias{predict.StatFactorModel}
\title{predict method for StatFactorModel object.}
\usage{
  predict.StatFactorModel(fit, ...)
}
\arguments{
  \item{fit}{"StatFactorModel" object created by
  fitTimeSeiresFactorModel.}

  \item{newdata}{An optional data frame in which to look
  for variables with which to predict. If omitted, the
  fitted values are used.}

  \item{...}{Any other arguments used in \code{predict.lm}}
}
\description{
  Generic function of predict method for
  fitStatisticalFactorModel. It utilizes function
  \code{predict.lm}.
}
\examples{
data(stat.fm.data)
.fit <- fitStatisticalFactorModel(sfm.dat,k=2,

predict(fit)
}
\author{
  Yi-An Chen. '
}

